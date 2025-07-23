
let run (input : Common.input) : Common.output =
  let open Common in
  let n = List.length input in
  let total_stake = List.fold_left (fun acc (_, s) -> acc + s) 0 input in
  let delegates = Array.of_list (List.map fst input) in
  let norm = Array.of_list (List.map (fun (_, s) -> float s /. float total_stake) input) in
  let entitlements = Array.map (fun s -> float blocks_per_cycle *. s) norm in
  let cE = Array.make (n + 1) 0.0 in
  for i = 0 to n - 1 do cE.(i + 1) <- cE.(i) +. entitlements.(i) done;
  let u = Random.float 1.0 in
  let slots = Array.make n 0 in
  for i = 0 to n - 1 do
    let l = floor (u +. cE.(i)) in
    let r = floor (u +. cE.(i + 1)) in
    slots.(i) <- int_of_float (r -. l)
  done;
  let result = Array.make blocks_per_cycle "" in
  let idx = ref 0 in
  for i = 0 to n - 1 do
    for _ = 1 to slots.(i) do
      if !idx < blocks_per_cycle then (result.(!idx) <- delegates.(i); incr idx)
    done
  done;
  result
