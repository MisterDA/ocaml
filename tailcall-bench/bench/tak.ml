(* Takeuchi function: deeply nested non-tail recursion, integer-only.
   Dispatch-bound. *)
let rec tak x y z =
  if x <= y then z
  else tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)

let () =
  let m = try int_of_string Sys.argv.(1) with _ -> 8 in
  Printf.printf "tak %d = %d\n" m (Sys.opaque_identity (tak (3 * m) (2 * m) m))
