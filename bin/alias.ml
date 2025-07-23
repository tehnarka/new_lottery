
let run (input : Common.input) : Common.output =
  let open Common in
  let n = List.length input in
  let delegates = Array.of_list (List.map fst input) in
  let powers = Array.of_list (List.map snd input) in
  let total_power = Array.fold_left (+) 0 powers in
  let w = Array.map (fun p -> float_of_int p /. float_of_int total_power) powers in
  let q = Array.map (fun x -> float_of_int n *. x) w in

  let prob = Array.make n 0. in
  let alias = Array.make n 0 in
  let small, large = Queue.create (), Queue.create () in

  Array.iteri (fun i qi -> if qi < 1.0 then Queue.push i small else Queue.push i large) q;

  while not (Queue.is_empty small || Queue.is_empty large) do
    let i = Queue.pop small in
    let j = Queue.pop large in
    prob.(i) <- q.(i); 
    alias.(i) <- j;
    q.(j) <- q.(j) -. (1.0 -. q.(i));
    if q.(j) < 1.0 then Queue.push j small else Queue.push j large
  done;

  Queue.iter (fun k -> prob.(k) <- 1.0; alias.(k) <- k) small;
  Queue.iter (fun k -> prob.(k) <- 1.0; alias.(k) <- k) large;

  Array.init blocks_per_cycle (fun _ ->
    let j = Random.int n in
    let u = Random.float 1.0 in
    if u < prob.(j) then delegates.(j) else delegates.(alias.(j))
  )
