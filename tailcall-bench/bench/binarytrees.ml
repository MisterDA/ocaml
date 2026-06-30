(* Binary-trees (allocation-heavy): builds and walks many short-lived trees.
   Mixed and largely GC-bound -- shows the *realistic floor* of the speedup,
   where dispatch is a smaller fraction of total time. *)
type tree = Leaf | Node of tree * tree

let rec make d = if d = 0 then Node (Leaf, Leaf) else Node (make (d - 1), make (d - 1))
let rec check = function Leaf -> 0 | Node (l, r) -> 1 + check l + check r

let () =
  let max_depth = try int_of_string Sys.argv.(1) with _ -> 16 in
  let min_depth = 4 in
  let acc = ref 0 in
  let d = ref min_depth in
  while !d <= max_depth do
    let iter = 1 lsl (max_depth - !d + min_depth) in
    let c = ref 0 in
    for _ = 1 to iter do c := !c + check (make !d) done;
    acc := !acc + !c;
    d := !d + 2
  done;
  Printf.printf "btrees = %d\n" (Sys.opaque_identity !acc)
