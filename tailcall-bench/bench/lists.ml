(* List map/filter/fold with many small closures: exercises closure
   application, allocation, and polymorphic comparison-free list code.
   Mixed: dispatch + minor-heap allocation. *)
let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 2000 in
  let reps = try int_of_string Sys.argv.(2) with _ -> 800 in
  let acc = ref 0 in
  for _ = 1 to reps do
    let l = List.init n (fun i -> i) in
    let l = List.map (fun x -> (x * 2) + 1) l in
    let l = List.filter (fun x -> x land 3 <> 0) l in
    acc := List.fold_left ( + ) 0 l
  done;
  Printf.printf "lists = %d\n" (Sys.opaque_identity !acc)
