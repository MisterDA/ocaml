(* Spectral-norm: float-array tight loops. Mixed: dispatch + float arithmetic
   primitives + unboxed float-array access. *)
let a i j = 1. /. float_of_int (((i + j) * (i + j + 1) / 2) + i + 1)

let mult_av n v av =
  for i = 0 to n - 1 do
    let s = ref 0. in
    for j = 0 to n - 1 do s := !s +. (a i j *. v.(j)) done;
    av.(i) <- !s
  done

let mult_atv n v atv =
  for i = 0 to n - 1 do
    let s = ref 0. in
    for j = 0 to n - 1 do s := !s +. (a j i *. v.(j)) done;
    atv.(i) <- !s
  done

let mult_atav n v atav u =
  mult_av n v u;
  mult_atv n u atav

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 500 in
  let u = Array.make n 1. and v = Array.make n 0. and t = Array.make n 0. in
  for _ = 0 to 9 do
    mult_atav n u v t;
    mult_atav n v u t
  done;
  let vbv = ref 0. and vv = ref 0. in
  for i = 0 to n - 1 do
    vbv := !vbv +. (u.(i) *. v.(i));
    vv := !vv +. (v.(i) *. v.(i))
  done;
  Printf.printf "spectralnorm = %.9f\n" (Sys.opaque_identity (sqrt (!vbv /. !vv)))
