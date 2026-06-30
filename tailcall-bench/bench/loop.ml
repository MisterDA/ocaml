(* Tight integer loop: the purest measure of raw opcode dispatch overhead.
   Almost every cycle is interpreter dispatch + ALU. *)
let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 100_000_000 in
  let s = ref 0 in
  for i = 1 to n do
    s := !s + (i * 3) - (i land 7)
  done;
  Printf.printf "loop = %d\n" (Sys.opaque_identity !s)
