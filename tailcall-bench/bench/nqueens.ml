(* N-queens by bitmask backtracking: recursion + tight inner loop + branchy
   integer logic. Dispatch-bound. *)
let queens n =
  let rec place row cols diag1 diag2 =
    if row = n then 1
    else begin
      let count = ref 0 in
      for col = 0 to n - 1 do
        let c = 1 lsl col
        and d1 = 1 lsl (row + col)
        and d2 = 1 lsl (row - col + n - 1) in
        if cols land c = 0 && diag1 land d1 = 0 && diag2 land d2 = 0 then
          count := !count + place (row + 1) (cols lor c) (diag1 lor d1) (diag2 lor d2)
      done;
      !count
    end
  in
  place 0 0 0 0

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 11 in
  Printf.printf "queens %d = %d\n" n (Sys.opaque_identity (queens n))
