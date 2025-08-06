open Common
(* Dispatches execution based on the selected allocation method *)
let run_method method_id input n_cycles =
  match method_id with
  | 1 ->  (* Alias method *)
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
  | 4 ->  (* SWRR with memory: Stateful Smooth Weighted Round Robin *)
    let inputs = [input_cycle_1; input_cycle_2; input_cycle_3] in
    let state = ref (Swrr.init_state input) in
    List.iteri (fun i input ->
      Format.printf "@.Cycle #%d (SWRR with memory)@." (i + 1);
      let output, new_state = Swrr.run_with_state input !state in
      state := new_state;
      verify_distribution input output blocks_per_cycle
    ) inputs
  | 5 ->  (* SSSA: Sorted Stride-based Slot Allocation after MinVAlloc *)
    for cycle = 1 to n_cycles do
      Format.printf "@.Cycle #%d (SSSA after MinVAlloc)@." cycle;
      let raw = Minvalloc.run input in (* Initial allocation *)
      let output = Shift.run raw in (* Slot distribution refinement *)
      verify_distribution input output blocks_per_cycle;
      (* Log each block allocation clearly *)
      Array.iteri (fun i d ->
        Format.printf "Block %04d: %s@." i d
      ) output
    done
  | 6 ->  (* SWRR via stream (lazy generation, memory-aware) *)
    let inputs = [input_cycle_1; input_cycle_2; input_cycle_3] in
    let state = ref Swrr_map.DelegateMap.empty in
    List.iteri (fun cycle input ->
      Format.printf "@.Cycle #%d (SWRR via stream with memory)@." (cycle + 1);
      (* Updating the status for the new one input *)
      state := Swrr_map.update_state input !state;

      (* Generate a sequence with the current input and updated state *)
      let output = Swrr_map.stream input !state
                   |> Seq.take blocks_per_cycle
                   |> Array.of_seq
      in

      (* Update the state after N stream steps *)
      let _, final_state =
        Array.fold_left
          (fun (s, acc_state) _ ->
             let _, new_state = Swrr_map.iteration input acc_state in
             (s, new_state)
          )
          ((), !state)
          output
      in
      state := final_state;

      verify_distribution input output blocks_per_cycle;

      (* Optional debug print *)
      (* Array.iteri (fun i d -> Format.printf "Block %04d: %s@." i d) output *)
    ) inputs


  | _ -> failwith "Invalid method id"

(* Entry point: handles user input and initiates allocation *)
let () =
  Format.printf "Choose allocation method:@.";
  Format.printf "1 - Alias method@.";
  Format.printf "2 - LowVAlloc - no memory@.";
  Format.printf "3 - MinVAlloc - no memory@.";
  Format.printf "4 - Smooth Weighted Round Robin (SWRR with memory - DATA ONLY FOR 3 CYCLES AVAILABLE). @.";
  Format.printf "5 - SSSA after MinVAlloc@.";
  Format.printf "6 - SWRR using map (3 cycles memory)@.";
  Format.printf "> %!";
  try
    let method_id = read_int () in
    Format.printf "How many cycles? > %!";
    let n_cycles = read_int () in
    run_method method_id input_example n_cycles
  with _ ->
    Format.printf "Invalid input. Please enter valid numbers.@."
