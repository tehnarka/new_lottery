
let run (input : Common.input) : Common.output =
  let open Common in
  let n = List.length input in
  (* Extract delegates and their corresponding stakes *)
  let delegates = Array.of_list (List.map fst input) in
  let powers = Array.of_list (List.map snd input) in

  (* Calculate the total sum of stakes for normalization *)
  let total_power = Array.fold_left (+) 0 powers in

  (* Compute normalized weights for each delegate *)
  let w = Array.map (fun p -> float_of_int p /. float_of_int total_power) powers in

  (* Scale normalized weights by the number of delegates *)
  let q = Array.map (fun x -> float_of_int n *. x) w in

  (* Initialize arrays: 'prob' stores probabilities, 'alias' stores alias indices *)
  let prob = Array.make n 0. in
  let alias = Array.make n 0 in

  (* Queues to categorize delegates by probability values into 'small' (<1) and 'large' (≥1) *)
  let small, large = Queue.create (), Queue.create () in

  (* Assign delegates to 'small' or 'large' queues based on scaled probabilities 'q' *)
  Array.iteri (fun i qi ->
    if qi < 1.0 then Queue.push i small else Queue.push i large
    ) q;
  
  (* Process delegates to fill alias and probability tables *)
  while not (Queue.is_empty small || Queue.is_empty large) do
    let i = Queue.pop small in
    let j = Queue.pop large in
    prob.(i) <- q.(i); (* Set probability for delegate 'i' *)
    alias.(i) <- j;    (* Set alias delegate for 'i' to delegate 'j' *)

    (* Update scaled probability for delegate 'j' after transferring probability to 'i' *)
    q.(j) <- q.(j) -. (1.0 -. q.(i));
    (* Re-categorize the adjusted delegate *)
    if q.(j) < 1.0 then Queue.push j small else Queue.push j large
  done;

  (* Handle remaining delegates by assigning a probability of 1 *)
  Queue.iter (fun k -> prob.(k) <- 1.0; alias.(k) <- k) small;
  Queue.iter (fun k -> prob.(k) <- 1.0; alias.(k) <- k) large;

  (* Generate block assignments using the alias method *)
  Array.init blocks_per_cycle (fun _ ->
    let j = Random.int n in                                           (* Randomly select a delegate index *)
    let u = Random.float 1.0 in                                       (* Random float in [0, 1) for alias decision *)
    if u < prob.(j) then delegates.(j) else delegates.(alias.(j))     (* Final delegate selection *)
  )
