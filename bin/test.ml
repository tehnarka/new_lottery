open Common

let run_method method_id input n_cycles =
  match method_id with
  | 1 ->  (* Alias *)
    for cycle = 1 to n_cycles do
      Format.printf "@.Cycle #%d (Alias method)@." cycle;
      let output = Alias.run input in
      verify_distribution input output blocks_per_cycle
    done
  | 2 ->  (* LowVAlloc *)
    for cycle = 1 to n_cycles do
      Format.printf "@.Cycle #%d (LowVAlloc)@." cycle;
      let output = Lowvalloc.run input in
      verify_distribution input output blocks_per_cycle
    done
  | 3 ->  (* MinVAlloc *)
    for cycle = 1 to n_cycles do
      Format.printf "@.Cycle #%d (MinVAlloc)@." cycle;
      let output = Minvalloc.run input in
      verify_distribution input output blocks_per_cycle
    done
  | 4 ->  (* SWRR with memory *)
    let inputs = [input_cycle_1; input_cycle_2; input_cycle_3] in
    let state = ref (Swr.init_state input) in
    List.iteri (fun i input ->
      Format.printf "@.Cycle #%d (SWRR with memory)@." (i + 1);
      let output, new_state = Swr.run_with_state input !state in
      state := new_state;
      verify_distribution input output blocks_per_cycle
    ) inputs
  | 5 ->  (* SSSA after MinVAlloc *)
    for cycle = 1 to n_cycles do
      Format.printf "@.Cycle #%d (SSSA after MinVAlloc)@." cycle;
      let raw = Minvalloc.run input in
      let output = Shift.run raw in
      verify_distribution input output blocks_per_cycle;
      Array.iteri (fun i d ->
        Format.printf "Block %04d: %s@." i d
      ) output
    done
  | _ -> failwith "Invalid method id"

let () =
  Format.printf "Choose allocation method:@.";
  Format.printf "1 - Alias method@.";
  Format.printf "2 - LowVAlloc@.";
  Format.printf "3 - MinVAlloc@.";
  Format.printf "4 - Smooth Weighted Round Robin (SWRR with memory - DATA ONLY FOR 3 CYCLES AVAILABLE). @.";
  Format.printf "5 - SSSA after MinVAlloc@.";
  Format.printf "> %!";
  try
    let method_id = read_int () in
    Format.printf "How many cycles? > %!";
    let n_cycles = read_int () in
    run_method method_id input_example n_cycles
  with _ ->
    Format.printf "Invalid input. Please enter valid numbers.@."
