
let run (input : Common.input) : Common.output =
  let open Common in

  (* Number of delegates *)
  let n = List.length input in
  
  (* Calculate total stake from all delegates *)
  let total_stake = List.fold_left (fun acc (_, s) -> acc + s) 0 input in
  
  (* Extract delegate identifiers into an array *)
  let delegates = Array.of_list (List.map fst input) in
  
  (* Normalize each delegate's stake relative to the total stake *)
  let norm = Array.of_list (List.map (fun (_, s) -> float s /. float total_stake) input) in
  
  (* Calculate expected number of slots for each delegate *)
  let entitlements = Array.map (fun s -> float blocks_per_cycle *. s) norm in
  
  (* Cumulative entitlement array used for allocation boundaries *)
  let cE = Array.make (n + 1) 0.0 in
  for i = 0 to n - 1 do cE.(i + 1) <- cE.(i) +. entitlements.(i) done;

  (* Random offset for initial allocation to ensure fairness *)
  let u = Random.float 1.0 in

  (* Slots allocated per delegate based on cumulative boundaries *)
  let slots =
  Array.init n (fun i ->
    let l = floor (u +. cE.(i)) in
    let r = floor (u +. cE.(i + 1)) in
    int_of_float (r -. l)
  ) in


  (* Build the final result array assigning delegates to slots sequentially *)
  let result =
  Array.concat (
    Array.to_list (Array.mapi (fun i count -> Array.make count delegates.(i)) slots)
  )
  |> fun arr -> Array.sub arr 0 blocks_per_cycle
  in
  result
