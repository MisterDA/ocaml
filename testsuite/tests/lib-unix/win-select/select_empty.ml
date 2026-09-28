(* TEST
 include systhreads;
 include unix;
 hasunix;
 target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* Without any descriptor, a negative timeout means waiting forever, like
   when there are descriptors. *)

let () =
  let returned = Atomic.make false in
  let _ : Thread.t = Thread.create (fun () ->
      ignore (Unix.select [] [] [] (-1.0));
      Atomic.set returned true) ()
  in
  Thread.delay 0.5;
  if Atomic.get returned then
    print_endline "FAIL: select returned"
  else
    print_endline "OK";
  (* The thread is still blocked in select *)
  exit 0
