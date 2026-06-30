(* Ackermann: enormous recursion depth and call count, integer-only.
   Dispatch-bound. *)
let rec ack m n =
  if m = 0 then n + 1
  else if n = 0 then ack (m - 1) 1
  else ack (m - 1) (ack m (n - 1))

let () =
  let m = try int_of_string Sys.argv.(1) with _ -> 3 in
  let n = try int_of_string Sys.argv.(2) with _ -> 8 in
  Printf.printf "ack %d %d = %d\n" m n (Sys.opaque_identity (ack m n))
