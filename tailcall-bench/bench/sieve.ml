(* Sieve of Eratosthenes over a Bytes buffer: heavy on bounds-checked
   array/bytes access opcodes and tight inner loops. *)
let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 2_000_000 in
  let iters = try int_of_string Sys.argv.(2) with _ -> 20 in
  let total = ref 0 in
  for _ = 1 to iters do
    let is_prime = Bytes.make (n + 1) '\001' in
    let count = ref 0 in
    for i = 2 to n do
      if Bytes.get is_prime i = '\001' then begin
        incr count;
        let j = ref (i * 2) in
        while !j <= n do
          Bytes.set is_prime !j '\000';
          j := !j + i
        done
      end
    done;
    total := !count
  done;
  Printf.printf "sieve primes <= %d : %d\n" n (Sys.opaque_identity !total)
