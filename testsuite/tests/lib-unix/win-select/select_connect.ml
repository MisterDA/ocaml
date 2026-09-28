(* TEST
 include unix;
 hasunix;
 target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* On Windows, Unix.select falls back to a worker thread emulation as soon as
   the descriptor set is not made exclusively of sockets. Sockets are then
   watched with WSAEventSelect, whose events must be translated into the
   results that the classic select would return. *)

open Unix

let with_pipe f =
  let pr, pw = pipe () in
  Fun.protect ~finally:(fun () -> close pr; close pw) (fun () -> f pr)

let show name s (r, w, e) =
  let mem l = if List.memq s l then "yes" else "no" in
  Printf.printf "%s: read=%s write=%s except=%s\n" name (mem r) (mem w) (mem e)

let connected_pair () =
  let server = socket PF_INET SOCK_STREAM 0 in
  bind server (ADDR_INET (inet_addr_loopback, 0));
  listen server 1;
  let client = socket PF_INET SOCK_STREAM 0 in
  connect client (getsockname server);
  let accepted, _ = accept server in
  close server;
  client, accepted

(* WSAEventSelect raises FD_CONNECT immediately for a socket that is already
   connected. It must not be taken for writability. *)
let () =
  let client, accepted = connected_pair () in
  set_nonblock client;
  let buf = Bytes.create 65536 in
  begin try
    while true do ignore (single_write client buf 0 (Bytes.length buf)) done
  with Unix_error ((EWOULDBLOCK | EAGAIN), _, _) -> ()
  end;
  with_pipe (fun pr ->
    show "full send buffer" client (select [pr] [client] [] 0.5));
  close client;
  close accepted
