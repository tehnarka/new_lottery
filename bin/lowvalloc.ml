let run (input : Common.input) : Common.output =
  let open Common in
  let total = List.fold_left (fun acc (_, x) -> acc + x) 0 input in
  let n = List.length input in
  let output = Array.make blocks_per_cycle "" in
  let acc = ref 0 in

  (* index *)
  let input_arr = Array.of_list input in
  let c = Array.make n 0.0 in

  (* int part *)
  Array.iteri (fun i (d, s) ->
    let e_i = float s *. float blocks_per_cycle /. float total in
    let g_i = int_of_float (floor e_i) in
    for j = !acc to !acc + g_i - 1 do
      if j < blocks_per_cycle then output.(j) <- d
    done;
    acc := !acc + g_i;
    c.(i) <- e_i -. float g_i
  ) input_arr;

  let remaining = blocks_per_cycle - !acc in

  (* fractional *)
  for _ = 1 to remaining do
    let i_star = ref 0 in
    for i = 1 to n - 1 do
      if c.(i) > c.(!i_star) then i_star := i
    done;
    let (d, _) = input_arr.(!i_star) in
    if !acc < blocks_per_cycle then output.(!acc) <- d;
    c.(!i_star) <- 0.0;
    incr acc
  done;

  output

