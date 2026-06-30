(* Naive Fibonacci: pure recursion. Dispatch-bound: APPLY/RETURN/GRAB +
   integer arithmetic, almost no allocation. Best case for the TC interp. *)
let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 35 in
  Printf.printf "fib %d = %d\n" n (Sys.opaque_identity (fib n))
