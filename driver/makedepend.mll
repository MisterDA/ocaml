(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Print the dependencies *)

(* Rules to parse .mll and .mly files *)


{ (* Beginning of header *)

(* Lexer helpers *)

let brace_depth = ref 0
and ocaml_comment_depth = ref 0

let in_pattern () = !brace_depth = 0 && !ocaml_comment_depth = 0

exception Lexical_error of string * string * int * int

let raise_lexical_error lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  raise (Lexical_error (msg,
                        p.Lexing.pos_fname,
                        p.Lexing.pos_lnum,
                        p.Lexing.pos_cnum - p.Lexing.pos_bol + 1))

let handle_lexical_error fn lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line = p.Lexing.pos_lnum
  and column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1
  and file = p.Lexing.pos_fname
  in
  try
    fn lexbuf
  with Lexical_error (msg, "", 0, 0) ->
    raise(Lexical_error(msg, file, line, column))

let warning lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  Printf.eprintf "ocamllex warning:\nFile \"%s\", line %d, character %d: %s.\n"
    p.Lexing.pos_fname p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1) msg;
  flush stderr

let hex_digit_value d =
  let d = Char.code d in
  if d >= 97 then d - 87 else
  if d >= 65 then d - 55 else
  d - 48

let decimal_code c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let hexadecimal_code s =
  let rec loop acc i =
    if i < String.length s then
      let value = hex_digit_value s.[i] in
      loop (16 * acc + value) (i + 1)
    else acc in
  loop 0 0

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }

type token =
  | Theader of Lexing.position * Lexing.position
  | Taction of Lexing.position * Lexing.position
  | Ttrailer of Lexing.position * Lexing.position
  | Teof

type state = HeaderDeclarations | Rules | Trailer
let state = ref HeaderDeclarations
let expect_action = ref false

type comment = OCaml | Yacc

let reset_mll_mly_lexer () =
  brace_depth := 0;
  ocaml_comment_depth := 0;
  state := HeaderDeclarations

} (* End of header *)

(* Lex strings, quoted strings, and characters, in order not to be
   confused by what is inside them. *)

let identstart =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let identbody =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

let lowercase = ['a'-'z' '_']
let ident = identstart identbody*
let extattrident = ident ('.' ident)*
let blank = [' ' '\009' '\012']

rule mly_main = parse
  | "/*"
    { handle_lexical_error (comment Yacc) lexbuf;
      mly_main lexbuf }
  | "(*"
    { ocaml_comment_depth := 1;
      handle_lexical_error (comment OCaml) lexbuf;
      mly_main lexbuf }
  | '\010'
    { Lexing.new_line lexbuf;
      mly_main lexbuf }
  | "%{"
    { let start = Lexing.lexeme_end_p lexbuf in
      Theader (start, handle_lexical_error mly_header lexbuf) }
  | "%%"
    { match !state with
      | HeaderDeclarations ->
         state := Rules;
         mly_rules lexbuf
      | Rules ->
         state := Trailer;
         let start = Lexing.lexeme_end_p lexbuf in
         Ttrailer (start, handle_lexical_error action lexbuf)
      | Trailer -> assert false }
  | "{"
    { incr brace_depth;
      match !state with
      | Rules ->
         let start = Lexing.lexeme_end_p lexbuf in
         Taction (start, handle_lexical_error action lexbuf)
      | HeaderDeclarations | Trailer -> mly_main lexbuf }
  | '"'
    { handle_lexical_error string lexbuf;
      mly_main lexbuf }
  (* note: ''' is a valid character literal (by contrast with the compiler) *)
  | "'" [^ '\\'] "'"
    { mly_main lexbuf }
  | "'" '\\' backslash_escapes "'"
    { mly_main lexbuf }
  | "'" '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)"'"
    { let v = decimal_code c d u in
      if v > 255 then
        raise_lexical_error lexbuf
          (Printf.sprintf "illegal escape sequence \\%c%c%c" c d u)
      else
        mly_main lexbuf }
  | "'" '\\' 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "'"
    { mly_main lexbuf }
  | "'" '\\' 'x'
      ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { mly_main lexbuf }
  | "'" '\\' (_ as c)
    { raise_lexical_error lexbuf
        (Printf.sprintf "illegal escape sequence \\%c" c)
    }
  | eof { Teof }
  | _ { mly_main lexbuf }

and mly_header = parse
  | "(*"
    { incr ocaml_comment_depth;
      handle_lexical_error (comment OCaml) lexbuf;
      mly_header lexbuf }
  | '\010'
    { Lexing.new_line lexbuf; mly_header lexbuf }
  | "%}"
    { Lexing.lexeme_start_p lexbuf }
  | eof
    { raise (Lexical_error ("unterminated header", "", 0, 0)) }
  | _
    { mly_header lexbuf }

and mly_rules = parse
  | "/*"
    { handle_lexical_error (comment Yacc) lexbuf;
      mly_rules lexbuf }
  | "(*"
    { incr ocaml_comment_depth;
      handle_lexical_error (comment OCaml) lexbuf;
      mly_rules lexbuf }
  | "{"
    { incr brace_depth;
      let start = Lexing.lexeme_end_p lexbuf in
      Taction (start, handle_lexical_error action lexbuf) }
  | '\010'
    { Lexing.new_line lexbuf; mly_rules lexbuf }
  | "%%"
    { assert (!state = Rules);
      state := Trailer;
      let start = Lexing.lexeme_end_p lexbuf in
      Ttrailer (start, handle_lexical_error action lexbuf) }
  | eof { Teof }
  | _ { mly_rules lexbuf }

and mll_main = parse
  | "(*"
    { ocaml_comment_depth := 1;
      handle_lexical_error (comment OCaml) lexbuf;
      mll_main lexbuf }
  | '\010'
    { Lexing.new_line lexbuf;
      mll_main lexbuf }
  | "rule"
    { match !state with
      | HeaderDeclarations ->
         state := Rules;
         expect_action := true;
         mll_main lexbuf
      | Rules | Trailer -> mll_main lexbuf }
  | "and" | "|"
    { expect_action := true;
      mll_main lexbuf }
  | "{"
    { incr brace_depth;
      let start = Lexing.lexeme_end_p lexbuf in
      match !state with
      | HeaderDeclarations ->
         let x = Theader (start, handle_lexical_error action lexbuf) in
         assert (!brace_depth = 0);
         x
      | Rules ->
         if !expect_action then begin
           expect_action := false;
           Taction (start, handle_lexical_error action lexbuf)
         end else begin
           state := Trailer;
           Ttrailer (start, handle_lexical_error action lexbuf)
         end
      | Trailer ->
         raise_lexical_error lexbuf "unexpected trailer" }
  | '"'
    { handle_lexical_error string lexbuf;
      mll_main lexbuf }
(* note: ''' is a valid character literal (by contrast with the compiler) *)
  | "'" [^ '\\'] "'"
    { mll_main lexbuf }
  | "'" '\\' backslash_escapes "'"
    { mll_main lexbuf }
  | "'" '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)"'"
    { let v = decimal_code c d u in
      if v > 255 then
        raise_lexical_error lexbuf
          (Printf.sprintf "illegal escape sequence \\%c%c%c" c d u)
      else
        mll_main lexbuf }
  | "'" '\\' 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "'"
    { mll_main lexbuf }
  | "'" '\\' 'x'
       ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
       { mll_main lexbuf }
  | "'" '\\' (_ as c)
    { raise_lexical_error lexbuf
        (Printf.sprintf "illegal escape sequence \\%c" c)
    }
  | eof { Teof }
  | _ { mll_main lexbuf }

and action = parse
  | "(*"
    { incr ocaml_comment_depth;
      handle_lexical_error (comment OCaml) lexbuf;
      action lexbuf }
  | '"'
    { handle_lexical_error string lexbuf;
      action lexbuf }
      (* note: ''' is a valid character literal (by contrast with the compiler) *)
  | "'" [^ '\\'] "'"
    { action lexbuf }
  | "'" '\\' backslash_escapes "'"
    { action lexbuf }
  | "'" '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)"'"
    { let v = decimal_code c d u in
      if v > 255 then
        raise_lexical_error lexbuf
          (Printf.sprintf "illegal escape sequence \\%c%c%c" c d u)
      else
        action lexbuf }
  | "'" '\\' 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "'"
    { action lexbuf }
  | "'" '\\' 'x'
    ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { action lexbuf }
  | "'" '\\' (_ as c)
    { raise_lexical_error lexbuf
        (Printf.sprintf "illegal escape sequence \\%c" c)
    }
  | "{"
    { incr brace_depth; action lexbuf }
  | "}"
    { decr brace_depth;
      if !brace_depth = 0 then Lexing.lexeme_start_p lexbuf
      else action lexbuf }
  | '\010'
    { Lexing.new_line lexbuf;
      action lexbuf }
  | eof
    {
      if !brace_depth = 0 then Lexing.lexeme_start_p lexbuf
      else raise (Lexical_error ("unterminated action", "", 0, 0)) }
  | _
    { action lexbuf }

and comment style = parse
  | "(*"
    { begin match style with
      | OCaml -> incr ocaml_comment_depth
      | Yacc -> () end;
      comment style lexbuf }
  | "*)"
    { match style with
      | OCaml -> decr ocaml_comment_depth;
                 if !ocaml_comment_depth = 0 then () else comment style lexbuf
      | Yacc -> comment style lexbuf }
  | "*/"
    { match style with
      | OCaml -> comment style lexbuf
      | Yacc -> () }
  | '"'
    { begin match style with
      | OCaml -> string lexbuf
      | Yacc -> () end;
      comment style lexbuf }
  | '{' ('%' '%'? extattrident blank*)? (lowercase* as delim) "|"
    { quoted_string delim lexbuf;
      comment style lexbuf }
  | "'"
    { skip_char lexbuf ;
      comment style lexbuf }
  | eof
    { let style = match style with OCaml -> "ocaml" | Yacc -> "yacc" in
      raise_lexical_error lexbuf (Printf.sprintf "unterminated comment %d %s"
                                    !ocaml_comment_depth style) }
  | '\010'
    { Lexing.new_line lexbuf; comment style lexbuf }
  | _
    { comment style lexbuf }

(* String parsing comes from the compiler lexer *)
and string = parse
    '"'
    { () }
  | '\\' ('\013'* '\010') ([' ' '\009'] * as spaces)
    { incr_loc lexbuf (String.length spaces);
      string lexbuf }
  | '\\' (backslash_escapes)
    { string lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9']  as u)
    { let v = decimal_code c d u in
      if in_pattern () then
        if v > 255 then
          raise_lexical_error lexbuf
            (Printf.sprintf
               "illegal backslash escape in string: '\\%c%c%c'" c d u);
      string lexbuf }
  | '\\' 'o' (['0'-'3']) (['0'-'7']) (['0'-'7'])
    { string lexbuf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F']) (['0'-'9' 'a'-'f' 'A'-'F'])
    { string lexbuf }
  | '\\' 'u' '{' (['0'-'9' 'a'-'f' 'A'-'F'] + as s) '}'
    { let v = hexadecimal_code s in
      if in_pattern () then
        if not (Uchar.is_valid v) then
          raise_lexical_error lexbuf
            (Printf.sprintf
              "illegal uchar escape in string: '\\u{%s}'" s);
      string lexbuf }
  | '\\' (_ as c)
    {if in_pattern () then
       warning lexbuf
        (Printf.sprintf "illegal backslash escape in string: '\\%c'" c) ;
     string lexbuf }
  | eof
    { raise(Lexical_error("unterminated string", "", 0, 0)) }
  | '\013'* '\010'
    { if !ocaml_comment_depth = 0 then
        raise_lexical_error lexbuf (Printf.sprintf "unescaped newline in string") ;
      incr_loc lexbuf 0;
      string lexbuf }
  | _
    { string lexbuf }

and quoted_string delim = parse
  | '\013'* '\010'
    { incr_loc lexbuf 0;
      quoted_string delim lexbuf }
  | eof
    { raise (Lexical_error ("unterminated string", "", 0, 0)) }
  | '|' (lowercase* as delim') '}'
    { if delim <> delim' then
      quoted_string delim lexbuf }
  | _
    { quoted_string delim lexbuf }

and skip_char = parse
  | '\\'? ('\013'* '\010') "'"
    { incr_loc lexbuf 1;
    }
  | [^ '\\' '\'' '\010' '\013'] "'" (* regular character *)
(* one character and numeric escape sequences *)
  | '\\' _ "'"
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7'] "'"
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    {()}
(* Perilous *)
  | "" {()}

{ (* Beginning of trailer *)

open Parsetree
module String = Misc.Stdlib.String

let stderr = Format.err_formatter

type file_kind = ML | MLI | MLL | MLY

let ml_synonyms = ref [".ml"]
let mli_synonyms = ref [".mli"]
let mll_synonyms = ref [".mll"]
let mly_synonyms = ref [".mly"]
let shared = ref false
let native_only = ref false
let bytecode_only = ref false
let raw_dependencies = ref false
let sort_files = ref false
let all_dependencies = ref false
let nocwd = ref false
let one_line = ref false
let allow_approximation = ref false
let debug = ref false

(* [(dir, contents)] where [contents] is returned by [Sys.readdir dir]. *)
let load_path = ref ([] : (string * string array) list)
let files =
  ref ([] : (string * file_kind * String.Set.t * string list) list)
let module_map = ref String.Map.empty

module Error_occurred : sig
  val set : unit -> unit
  val get : unit -> bool
end = struct
  (* Once set to [true], [error_occurred] should never be set to
     [false]. *)
  let error_occurred = ref false
  let get () = !error_occurred
  let set () = error_occurred := true
end

let prepend_to_list l e = l := e :: !l

(* Fix path to use '/' as directory separator instead of '\'.
   Only under Windows. *)
let fix_slash s =
  if Sys.os_type = "Unix" then s else begin
    String.map (function '\\' -> '/' | c -> c) s
  end

(* Since we reinitialize load_path after reading OCAMLCOMP,
  we must use a cache instead of calling Sys.readdir too often. *)
let dirs = ref String.Map.empty
let readdir dir =
  try
    String.Map.find dir !dirs
  with Not_found ->
    let contents =
      try
        Sys.readdir dir
      with Sys_error msg ->
        Format.eprintf "@[Bad -I option: %s@]@." msg;
        Error_occurred.set ();
        [||]
    in
    dirs := String.Map.add dir contents !dirs;
    contents

let add_to_load_path dir =
  try
    let dir = Misc.expand_directory Config.standard_library dir in
    let contents = readdir dir in
    prepend_to_list load_path (dir, contents)
  with Sys_error msg ->
    Format.eprintf "@[Bad -I option: %s@]@." msg;
    Error_occurred.set ()

let add_to_synonym_list synonyms suffix =
  if (String.length suffix) > 1 && suffix.[0] = '.' then
    prepend_to_list synonyms suffix
  else begin
    Format.eprintf "@[Bad suffix: '%s'@]@." suffix;
    Error_occurred.set ()
  end

(* Find file 'name' (capitalized) in search path *)
let find_module_in_load_path name =
  let synonyms = !mli_synonyms @ !ml_synonyms @ !mll_synonyms @ !mly_synonyms in
  let names = List.map (fun ext -> name ^ ext) synonyms in
  let unames =
    let uname = Unit_info.normalize name in
    List.map (fun ext -> uname ^ ext) synonyms
  in
  let rec find_in_path = function
    | [] -> raise Not_found
    | (dir, contents) :: rem ->
        let mem s = List.mem s names || List.mem s unames in
        match Array.find_opt mem contents with
        | Some truename ->
            if dir = Filename.current_dir_name then truename
            else Filename.concat dir truename
        | None -> find_in_path rem in
  find_in_path !load_path

let find_dependency target_kind modname (byt_deps, opt_deps) =
  match find_module_in_load_path modname with
  | exception Not_found -> (byt_deps, opt_deps)
  | filename ->
    let basename = Filename.chop_extension filename in
    let cmi_file = basename ^ ".cmi" in
    let cmx_file = basename ^ ".cmx" in
    let mli_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !mli_synonyms
    and ml_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !ml_synonyms
    and mll_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !mll_synonyms
    and mly_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !mly_synonyms in
    if mli_exists || mll_exists || mly_exists then
      let new_opt_dep =
        if !all_dependencies then
          match target_kind with
          | MLI -> [ cmi_file ]
          | ML  ->
             cmi_file :: (if ml_exists then [ cmx_file ] else [])
          | MLL | MLY -> assert false
        else
        (* this is a make-specific hack that makes .cmx to be a 'proxy'
           target that would force the dependency on .cmi via transitivity *)
        if ml_exists || mll_exists || mly_exists
        then [ cmx_file ]
        else [ cmi_file ]
      in
      ( cmi_file :: byt_deps, new_opt_dep @ opt_deps)
    else
      (* "just .ml" case *)
      let bytenames =
        if !all_dependencies then
          match target_kind with
          | MLI -> [ cmi_file ]
          | ML  -> [ cmi_file ]
          | MLL | MLY -> assert false
        else
          (* again, make-specific hack *)
          [basename ^ (if !native_only then ".cmx" else ".cmo")] in
      let optnames =
        if !all_dependencies
        then match target_kind with
          | MLI -> [ cmi_file ]
          | ML  -> [ cmi_file; cmx_file ]
          | MLL | MLY -> assert false
        else [ cmx_file ]
      in
      (bytenames @ byt_deps, optnames @  opt_deps)

let (depends_on, escaped_eol) = (":", " \\\n    ")

let print_filename s =
  let s = if !Clflags.force_slash then fix_slash s else s in
  if not (String.contains s ' ') then begin
    print_string s;
  end else begin
    let rec count n i =
      if i >= String.length s then n
      else if s.[i] = ' ' then count (n+1) (i+1)
      else count n (i+1)
    in
    let spaces = count 0 0 in
    let result = Bytes.create (String.length s + spaces) in
    let rec loop i j =
      if i >= String.length s then ()
      else if s.[i] = ' ' then begin
        Bytes.set result j '\\';
        Bytes.set result (j+1) ' ';
        loop (i+1) (j+2);
      end else begin
        Bytes.set result j s.[i];
        loop (i+1) (j+1);
      end
    in
    loop 0 0;
    print_bytes result;
  end

let print_dependencies target_files deps =
  let pos = ref 0 in
  let print_on_same_line item =
    if !pos <> 0 then print_string " ";
    print_filename item;
    pos := !pos + String.length item + 1;
  in
  let print_on_new_line item =
    print_string escaped_eol;
    print_filename item;
    pos := String.length item + 4;
  in
  let print_compact item =
    if !one_line || (!pos + 1 + String.length item <= 77)
    then print_on_same_line item
    else print_on_new_line item
  in
  let print_dep item =
    if !one_line
    then print_on_same_line item
    else print_on_new_line item
  in
  List.iter print_compact target_files;
  print_string " "; print_string depends_on;
  pos := !pos + String.length depends_on + 1;
  List.iter print_dep deps;
  print_string "\n"

let print_raw_dependencies source_file deps =
  print_filename source_file; print_string depends_on;
  String.Set.iter
    (fun dep ->
       (* filter out "*predef*" *)
      if (String.length dep > 0)
          && (match dep.[0] with
              | 'A'..'Z' | '\128'..'\255' -> true
              | _ -> false) then
        begin
          print_char ' ';
          print_string dep
        end)
    deps;
  print_char '\n'


(* Process one file *)

let print_exception exn =
  Location.report_exception stderr exn

let report_err exn =
  Error_occurred.set ();
  print_exception exn

let tool_name = "ocamldep"

let rec lexical_approximation lexbuf =
  (* Approximation when a file can't be parsed.
     Heuristic:
     - first component of any path starting with an uppercase character is a
       dependency.
     - always skip the token after a dot, unless dot is preceded by a
       lower-case identifier
     - always skip the token after a backquote
  *)
  let rec process ~after_lident lexbuf =
    match Lexer.token lexbuf with
    | Parser.UIDENT name ->
        Depend.free_structure_names :=
          String.Set.add name !Depend.free_structure_names;
        process ~after_lident:false lexbuf
    | Parser.LIDENT _ -> process ~after_lident:true lexbuf
    | Parser.DOT when after_lident -> process ~after_lident:false lexbuf
    | Parser.DOT | Parser.BACKQUOTE -> skip_one lexbuf
    | Parser.EOF -> ()
    | _ -> process ~after_lident:false lexbuf
  and skip_one lexbuf =
    match Lexer.token lexbuf with
    | Parser.DOT | Parser.BACKQUOTE -> skip_one lexbuf
    | Parser.EOF -> ()
    | _ -> process ~after_lident:false lexbuf

  in
  try process ~after_lident:false lexbuf
  with Lexer.Error _ -> lexical_approximation lexbuf

let read_and_approximate inputfile =
  Depend.free_structure_names := String.Set.empty;
  begin try
    In_channel.with_open_bin inputfile @@ fun ic ->
    seek_in ic 0;
    Location.input_name := inputfile;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf inputfile;
    lexical_approximation lexbuf
  with exn ->
    report_err exn
  end;
  !Depend.free_structure_names

let extract_from_ast extract_function ast =
  let bound_vars =
    List.fold_left
      (fun bv modname ->
        let lid =
          let lexbuf = Lexing.from_string modname in
          Location.init lexbuf
            (Printf.sprintf "command line argument: -open %S" modname);
          Parse.simple_module_path lexbuf in
        Depend.open_module bv lid)
      !module_map ((* PR#7248 *) List.rev !Clflags.open_modules)
  in
  let r = extract_function bound_vars ast in
  (!Depend.free_structure_names, r)

let read_parse_and_extract parse_function extract_function def ast_kind
    source_file =
  Depend.pp_deps := [];
  Depend.free_structure_names := String.Set.empty;
  try
    let input_file = Pparse.preprocess source_file in
    Fun.protect ~finally:(fun () -> Pparse.remove_preprocessed input_file)
    @@ fun () ->
      let ast = Pparse.file ~tool_name input_file parse_function ast_kind in
      extract_from_ast extract_function ast
  with x -> begin
    print_exception x;
    if not !allow_approximation then begin
      Error_occurred.set ();
      (String.Set.empty, def)
    end else
      (read_and_approximate source_file, def)
  end

let print_ml_dependencies source_file extracted_deps pp_deps =
  let basename = Filename.chop_extension source_file in
  let byte_targets = [ basename ^ ".cmo" ] in
  let native_targets =
    if !all_dependencies
    then [ basename ^ ".cmx"; basename ^ ".o" ]
    else [ basename ^ ".cmx" ] in
  let shared_targets = [ basename ^ ".cmxs" ] in
  let init_deps = if !all_dependencies then [source_file] else [] in
  let cmi_name = basename ^ ".cmi" in
  let init_deps, extra_targets =
    if List.exists (fun ext -> Sys.file_exists (basename ^ ext))
        !mli_synonyms
    then (cmi_name :: init_deps, cmi_name :: init_deps), []
    else (init_deps, init_deps),
         (if !all_dependencies then [cmi_name] else [])
  in
  let (byt_deps, native_deps) =
    String.Set.fold (find_dependency ML)
      extracted_deps init_deps in
  if not !native_only then
    print_dependencies (byte_targets @ extra_targets) (byt_deps @ pp_deps);
  if not !bytecode_only then
    begin
      print_dependencies (native_targets @ extra_targets)
        (native_deps @ pp_deps);
      if !shared then
        print_dependencies (shared_targets @ extra_targets)
          (native_deps @ pp_deps)
    end

let print_mli_dependencies source_file extracted_deps pp_deps =
  let basename = Filename.chop_extension source_file in
  let (byt_deps, _opt_deps) =
    String.Set.fold (find_dependency MLI)
      extracted_deps ([], []) in
  print_dependencies [basename ^ ".cmi"] (byt_deps @ pp_deps)

(* let print_mll_dependencies source_file extracted_deps pp_deps = *)
(*   ignore source_file; *)
(*   ignore extracted_deps; *)
(*   ignore pp_deps; *)
(*   failwith "print_mll_dependencies" *)

(* let print_mly_dependencies source_file extracted_deps pp_deps = *)
(*   ignore source_file; *)
(*   ignore extracted_deps; *)
(*   ignore pp_deps; *)
(*   failwith "print_mly_dependencies" *)

let print_file_dependencies (source_file, kind, extracted_deps, pp_deps) =
  if !raw_dependencies then begin
    print_raw_dependencies source_file extracted_deps
  end else
    match kind with
    | ML -> print_ml_dependencies source_file extracted_deps pp_deps
    | MLI -> print_mli_dependencies source_file extracted_deps pp_deps
    | MLL -> print_ml_dependencies source_file extracted_deps pp_deps
    | MLY -> print_ml_dependencies source_file extracted_deps pp_deps

let parse_use_file_as_impl lexbuf =
  let f x =
    match x with
    | Ptop_def s -> s
    | Ptop_dir _ -> []
  in
  List.concat_map f (Parse.use_file lexbuf)

let ml_file_dependencies source_file =
  let (extracted_deps, ()) =
    read_parse_and_extract parse_use_file_as_impl Depend.add_implementation ()
                           Pparse.Structure source_file
  in
  prepend_to_list files (source_file, ML, extracted_deps, !Depend.pp_deps)

let mli_file_dependencies source_file =
  let (extracted_deps, ()) =
    read_parse_and_extract Parse.interface Depend.add_signature ()
                           Pparse.Signature source_file
  in
  prepend_to_list files (source_file, MLI, extracted_deps, !Depend.pp_deps)

let mll_mly_file_dependencies source_file kind =
  reset_mll_mly_lexer ();
  let contents = In_channel.with_open_bin source_file Misc.string_of_file in
  let next_action =
    let lexbuf = Lexing.from_string contents in
    let main = match kind with
      | MLL -> mll_main | MLY -> mly_main
      | _ -> assert false in
    fun () -> main lexbuf
  in
  let ast_from_fragment ~need_struct_item start finish =
    let len = finish.Lexing.pos_cnum - start.Lexing.pos_cnum in
    let fragment = String.sub contents start.Lexing.pos_cnum len in
    let fragment = if kind = MLY then
                     String.map (function '$' -> '_' | c -> c) fragment
                   else fragment in
    let fragment = if need_struct_item then "let _ = " ^ fragment
                   else fragment in
    let lexbuf = Lexing.from_string fragment in
    parse_use_file_as_impl lexbuf
  in
  let rec extract items =
    match next_action () with
    | Theader (start, finish)
    | Ttrailer (start, finish) ->
       let ast = ast_from_fragment ~need_struct_item:false start finish in
       extract (ast :: items)
    | Taction (start, finish) ->
       let ast = ast_from_fragment ~need_struct_item:true start finish in
       extract (ast :: items)
    | Teof -> List.rev items
  in
  let ast = List.flatten (extract []) in
  let (extracted_deps, ()) =
    extract_from_ast Depend.add_implementation ast
  in
  files := (source_file, kind, extracted_deps, !Depend.pp_deps) :: !files

let mll_file_dependencies source_file =
  mll_mly_file_dependencies source_file MLL

let mly_file_dependencies source_file =
  mll_mly_file_dependencies source_file MLY

let process_file_as process_fun def source_file =
  Compenv.readenv stderr (Before_compile source_file);
  load_path := [];
  let cwd = if !nocwd then [] else [Filename.current_dir_name] in
  List.iter add_to_load_path (
      (!Compenv.last_include_dirs @
       !Clflags.include_dirs @
       !Compenv.first_include_dirs @
       cwd
      ));
  Location.input_name := source_file;
  try
    if Sys.file_exists source_file then process_fun source_file else def
  with x -> report_err x; def

let process_file source_file ~ml_file ~mli_file ~mll_file ~mly_file ~def =
  if List.exists (Filename.check_suffix source_file) !ml_synonyms then
    process_file_as ml_file def source_file
  else if List.exists (Filename.check_suffix source_file) !mli_synonyms then
    process_file_as mli_file def source_file
  else if List.exists (Filename.check_suffix source_file) !mll_synonyms then
    process_file_as mll_file def source_file
  else if List.exists (Filename.check_suffix source_file) !mly_synonyms then
    process_file_as mly_file def source_file
  else def

let file_dependencies source_file =
  process_file source_file ~def:()
    ~ml_file:ml_file_dependencies
    ~mli_file:mli_file_dependencies
    ~mll_file:mll_file_dependencies
    ~mly_file:mly_file_dependencies

let file_dependencies_as kind =
  match kind with
  | ML -> process_file_as ml_file_dependencies ()
  | MLI -> process_file_as mli_file_dependencies ()
  | MLL -> process_file_as mll_file_dependencies ()
  | MLY -> process_file_as mly_file_dependencies ()

let sort_files_by_dependencies files =
  let h = Hashtbl.create 31 in
  let worklist = ref [] in

(* Init Hashtbl with all defined modules *)
  let files = List.map (fun (file, file_kind, deps, pp_deps) ->
    let modname = Unit_info.modname_from_source file in
    let key = (modname, file_kind) in
    let new_deps = ref [] in
    Hashtbl.add h key (file, new_deps);
    prepend_to_list worklist key;
    (modname, file_kind, deps, new_deps, pp_deps)
  ) files in

(* Keep only dependencies to defined modules *)
  List.iter (fun (modname, file_kind, deps, new_deps, _pp_deps) ->
    let add_dep modname kind = prepend_to_list new_deps (modname, kind) in
    String.Set.iter (fun modname ->
      match file_kind with
          ML -> (* ML depends both on ML and MLI *)
            if Hashtbl.mem h (modname, MLI) then add_dep modname MLI;
            if Hashtbl.mem h (modname, ML) then add_dep modname ML
        | MLI -> (* MLI depends on MLI if exists, or ML otherwise *)
          if Hashtbl.mem h (modname, MLI) then add_dep modname MLI
          else if Hashtbl.mem h (modname, ML) then add_dep modname ML
        | MLL | MLY -> ()
    ) deps;
    if file_kind = ML then (* add dep from .ml to .mli *)
      if Hashtbl.mem h (modname, MLI) then add_dep modname MLI
  ) files;

(* Print and remove all files with no remaining dependency. Iterate
   until all files have been removed (worklist is empty) or
   no file was removed during a turn (cycle). *)
  let printed = ref true in
  while !printed && !worklist <> [] do
    let files = !worklist in
    worklist := [];
    printed := false;
    List.iter (fun key ->
      let (file, deps) = Hashtbl.find h key in
      let set = !deps in
      deps := [];
      List.iter (fun key ->
        if Hashtbl.mem h key then prepend_to_list deps key
      ) set;
      if !deps = [] then begin
        printed := true;
        Printf.printf "%s " file;
        Hashtbl.remove h key;
      end else
        prepend_to_list worklist key
    ) files
  done;

  if !worklist <> [] then begin
    Location.error "cycle in dependencies. End of list is not sorted."
    |> Location.print_report stderr;
    let sorted_deps =
      let li = ref [] in
      Hashtbl.iter (fun _ file_deps -> prepend_to_list li file_deps) h;
      List.sort (fun (file1, _) (file2, _) -> String.compare file1 file2) !li
    in
    List.iter (fun (file, deps) ->
      Format.eprintf "\t@[%s: " file;
      List.iter (fun (modname, kind) ->
        Format.eprintf "%s.%s " modname (if kind=ML then "ml" else "mli")
      ) !deps;
      Format.eprintf "@]@.";
      Printf.printf "%s " file) sorted_deps;
    Error_occurred.set ()
  end;
  Printf.printf "\n%!";
  ()

(* Map *)

let rec dump_map s0 ppf m =
  let open Depend in
  String.Map.iter
    (fun key (Node(s1,m')) ->
      let s = String.Set.diff s1 s0 in
      if String.Set.is_empty s then
        Format.fprintf ppf "@ @[<hv2>module %s : sig%a@;<1 -2>end@]"
          key (dump_map (String.Set.union s1 s0)) m'
      else
        Format.fprintf ppf "@ module %s = %s" key (String.Set.choose s))
    m

let process_ml_map =
  read_parse_and_extract Parse.implementation Depend.add_implementation_binding
                         String.Map.empty Pparse.Structure

let process_mli_map =
  read_parse_and_extract Parse.interface Depend.add_signature_binding
                         String.Map.empty Pparse.Signature

let process_mll_map fname =
  report_err (Failure (fname ^ " : maps are not supported on mll files."));
  String.Set.empty, String.Map.empty

let process_mly_map fname =
  report_err (Failure (fname ^ " : maps are not supported on mll files."));
  String.Set.empty, String.Map.empty

let parse_map fname =
  let old_transp = !Clflags.transparent_modules in
  Clflags.transparent_modules := true;
  let (deps, m) =
    process_file fname ~def:(String.Set.empty, String.Map.empty)
      ~ml_file:process_ml_map
      ~mli_file:process_mli_map
      ~mll_file:process_mll_map
      ~mly_file:process_mly_map
  in
  Clflags.transparent_modules := old_transp;
  let modname = Unit_info.modname_from_source fname in
  if String.Map.is_empty m then
    report_err (Failure (fname ^ " : empty map file or parse error"));
  let mm = Depend.make_node m in
  if !debug then begin
    Format.printf "@[<v>%s:%t%a@]@." fname
      (fun ppf -> String.Set.iter (Format.fprintf ppf " %s") deps)
      (dump_map deps) (String.Map.add modname mm String.Map.empty)
  end;
  let mm = Depend.weaken_map (String.Set.singleton modname) mm in
  module_map := String.Map.add modname mm !module_map

(* Dependency processing *)

type dep_arg =
  | Map of Misc.filepath (* -map option *)
  | Src of Misc.filepath * file_kind option (* -impl, -intf or anon arg *)

let process_dep_arg = function
  | Map file -> parse_map file
  | Src (file, None) -> file_dependencies file
  | Src (file, (Some file_kind)) -> file_dependencies_as file_kind file

let process_dep_args dep_args = List.iter process_dep_arg dep_args

(* Entry point *)

let print_version () =
  Format.printf "ocamldep, version %s@." Sys.ocaml_version;
  exit 0

let print_version_num () =
  Format.printf "%s@." Sys.ocaml_version;
  exit 0


let run_main argv =
  let dep_args_rev : dep_arg list ref = ref [] in
  let add_dep_arg f s = prepend_to_list dep_args_rev (f s) in
  Clflags.classic := false;
  try
    Compenv.readenv stderr Before_args;
    Clflags.reset_arguments (); (* reset arguments from ocamlc/ocamlopt *)
    Clflags.add_arguments __LOC__ [
      "-absname", Arg.Set Clflags.absname,
        " Show absolute filenames in error messages";
      "-no-absname", Arg.Clear Clflags.absname,
        " Do not try to show absolute filenames in error messages (default)";
      "-all", Arg.Set all_dependencies,
        " Generate dependencies on all files";
      "-allow-approx", Arg.Set allow_approximation,
        " Fallback to a lexer-based approximation on unparsable files";
      "-as-map", Arg.Set Clflags.transparent_modules,
        " Omit delayed dependencies for module aliases (-no-alias-deps -w -49)";
        (* "compiler uses -no-alias-deps, and no module is coerced"; *)
      "-debug-map", Arg.Set debug,
        " Dump the delayed dependency map for each map file";
      "-I", Arg.String (prepend_to_list Clflags.include_dirs),
        "<dir>  Add <dir> to the list of include directories";
      "-nocwd", Arg.Set nocwd,
        " Do not add current working directory to \
          the list of include directories";
      "-impl", Arg.String (add_dep_arg (fun f -> Src (f, Some ML))),
        "<f>  Process <f> as a .ml file";
      "-intf", Arg.String (add_dep_arg (fun f -> Src (f, Some MLI))),
        "<f>  Process <f> as a .mli file";
      "-lex", Arg.String (add_dep_arg (fun f -> Src (f, Some MLL))),
        "<f>  Process <f> as a .mll file";
      "-yacc", Arg.String (add_dep_arg (fun f -> Src (f, Some MLY))),
        "<f>  Process <f> as a .mly file";
      "-map", Arg.String (add_dep_arg (fun f -> Map f)),
        "<f>  Read <f> and propagate delayed dependencies to following files";
      "-ml-synonym", Arg.String(add_to_synonym_list ml_synonyms),
        "<e>  Consider <e> as a synonym of the .ml extension";
      "-mli-synonym", Arg.String(add_to_synonym_list mli_synonyms),
        "<e>  Consider <e> as a synonym of the .mli extension";
      "-mll-synonym", Arg.String(add_to_synonym_list mll_synonyms),
        "<e>  Consider <e> as a synonym of the .mll extension";
      "-mly-synonym", Arg.String(add_to_synonym_list mly_synonyms),
        "<e>  Consider <e> as a synonym of the .mly extension";
      "-modules", Arg.Set raw_dependencies,
        " Print module dependencies in raw form (not suitable for make)";
      "-native", Arg.Set native_only,
        " Generate dependencies for native-code only (no .cmo files)";
      "-bytecode", Arg.Set bytecode_only,
        " Generate dependencies for bytecode-code only (no .cmx files)";
      "-one-line", Arg.Set one_line,
        " Output one line per file, regardless of the length";
      "-open", Arg.String (prepend_to_list Clflags.open_modules),
        "<module>  Opens the module <module> before typing";
      "-plugin", Arg.String(fun _p -> Clflags.plugin := true),
        "<plugin>  (no longer supported)";
      "-pp", Arg.String(fun s -> Clflags.preprocessor := Some s),
        "<cmd>  Pipe sources through preprocessor <cmd>";
      "-ppx", Arg.String (prepend_to_list Compenv.first_ppx),
        "<cmd>  Pipe abstract syntax trees through preprocessor <cmd>";
      "-shared", Arg.Set shared,
        " Generate dependencies for native plugin files (.cmxs targets)";
      "-slash", Arg.Set Clflags.force_slash,
        " (Windows) Use forward slash / instead of backslash \\ in file paths";
      "-no-slash", Arg.Clear Clflags.force_slash,
        " (Windows) Preserve any backslash \\ in file paths";
      "-sort", Arg.Set sort_files,
        " Sort files according to their dependencies";
      "-version", Arg.Unit print_version,
        " Print version and exit";
      "-vnum", Arg.Unit print_version_num,
        " Print version number and exit";
      "-args", Arg.Expand Arg.read_arg,
        "<file> Read additional newline separated command line arguments \n\
        \      from <file>";
      "-args0", Arg.Expand Arg.read_arg0,
        "<file> Read additional NUL separated command line arguments from \n\
        \      <file>"
    ];
    let program = Filename.basename Sys.argv.(0) in
    Compenv.parse_arguments (ref argv)
      (add_dep_arg (fun f -> Src (f, None))) program;
    process_dep_args (List.rev !dep_args_rev);
    Compenv.readenv stderr Before_link;
    if !sort_files then sort_files_by_dependencies !files
    else List.iter print_file_dependencies (List.sort compare !files);
    (if Error_occurred.get () then 2 else 0)
  with
  | Compenv.Exit_with_status n ->
      n
  | exn ->
      Location.report_exception stderr exn;
      2


let main () =
  exit (run_main Sys.argv)

let main_from_option () =
  if Sys.argv.(1) <> "-depend" then begin
    Printf.eprintf
      "Fatal error: argument -depend must be used as first argument.\n%!";
    exit 2;
  end;
  let args =
    Array.concat [ [| Sys.argv.(0) ^ " -depend" |];
                   Array.sub Sys.argv 2 (Array.length Sys.argv - 2) ] in
  Sys.argv.(0) <- args.(0);
  exit (run_main args)

} (* End of trailer *)
