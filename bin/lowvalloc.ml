let run (input : Common.input) : Common.output =
  let open Common in

  (* Compute the total stake across all delegates *)
  let total = List.fold_left (fun acc (_, x) -> acc + x) 0 input in

  (* Determine the number of delegates *)
  let n = List.length input in

   (* Initialize the output array with the specified number of blocks per cycle *)
  let output = Array.make blocks_per_cycle "" in

  (* Counter for tracking the current index position in the output array *)
  let acc = ref 0 in

  (* Convert input list to array for efficient indexing *)
  let input_arr = Array.of_list input in

  (* Array to store fractional parts of slot allocations *)
  let c = Array.make n 0.0 in

  (* Assign integer part of the expected slots to each delegate *)
  Array.iteri (fun i (d, s) ->
    (* Calculate exact expected slots for delegate 'd' *)
    let e_i = float s *. float blocks_per_cycle /. float total in

     (* Compute integer part of expected slots (rounded down) *)
    let g_i = int_of_float (floor e_i) in

    (* Assign 'g_i' slots consecutively to delegate 'd' in output array *)
    for j = !acc to !acc + g_i - 1 do
      if j < blocks_per_cycle then output.(j) <- d
    done;

    (* Update the accumulated slot count *)
    acc := !acc + g_i;

    (* Store fractional remainder for later distribution *)
    c.(i) <- e_i -. float g_i
  ) input_arr;
  
  (* Calculate remaining unallocated slots *)
  let remaining = blocks_per_cycle - !acc in

  (* Allocate remaining slots based on largest fractional remainders *)
  for _ = 1 to remaining do
    (* Find delegate with the maximum fractional remainder *)
    let i_star = ref 0 in
    for i = 1 to n - 1 do
      if c.(i) > c.(!i_star) then i_star := i
    done;

    (* Assign one additional slot to the delegate identified above *)
    let (d, _) = input_arr.(!i_star) in
    if !acc < blocks_per_cycle then output.(!acc) <- d;

    (* Set fractional remainder to zero to avoid re-selection *)
    c.(!i_star) <- 0.0;

    (* Increment the accumulated slot count *)
    incr acc
  done;

  output

