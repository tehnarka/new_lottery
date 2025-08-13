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
    let inputs = [input_big_small; input_big_small; input_big_small; input_big_small; input_big_small; input_big_small; input_big_small; input_big_small; input_big_small; input_big_small] in
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

      (* verify_distribution input output blocks_per_cycle; *)

      
      Array.iter(fun d -> Format.printf "%s "  d) output 
    ) inputs
    
  | 7 ->  (* Lazy SWRR with memory (timestamp-based) *)
  let inputs = [input_cycle_1; input_cycle_2; input_cycle_3] in

  let rec run_cycles inputs state cycle =
    match inputs with
    | [] -> ()
    | input :: rest ->
        Format.printf "@.Cycle #%d (Lazy SWRR with memory)@." cycle;

        (* 1. Start of cycle: update stakes *)
        let state_updated = Swrr_lazy.update_state input state in

        (* 2. Inside cycle: generate blocks with iteration *)
        let rec generate n acc st =
          match n with
          | 0 -> List.rev acc, st
          | _ ->
              let winner, next_state = Swrr_lazy.iteration input st in
              generate (n - 1) (winner :: acc) next_state
        in
        let output_list, state_after_iter = generate blocks_per_cycle [] state_updated in
        let output = Array.of_list output_list in

        (* 3. Check distribution *)
        verify_distribution input output blocks_per_cycle;

        (* 4. Show result *)
        (* Array.iteri (fun i d ->
          Format.printf "Block %04d: %s@." i d
        ) output; *)

        (* 5. finish cycle: materalised credits *)
        let finalized = Swrr_lazy.finalize_cycle state_after_iter in

        (* 6. next cycle *)
        run_cycles rest finalized (cycle + 1)
  in

  let empty_state = { Swrr_lazy.delegates = Swrr_lazy.DelegateMap.empty; step = 0 } in
  let initial_state = Swrr_lazy.update_state (List.hd inputs) empty_state in
  run_cycles (List.tl inputs) initial_state 1

  (* Test case for method_id = 8: verify that only one delegate is updated per iteration *)

  (* Test case for method_id = 8: count actual map updates across all operations *)

(* Test case for method_id = 8: count actual map updates across all operations *)

| 8 ->  (* Lazy SWRR internal map update test with multiple cycles *)
  let input = [
    ("A", 1);
    ("B", 3);
    ("C", 2)
  ] in
  let empty_state = { Swrr_lazy.delegates = Swrr_lazy.DelegateMap.empty; step = 0 } in

  let count_changed_entries (before : Swrr_lazy.state) (after : Swrr_lazy.state) : int =
    Swrr_lazy.DelegateMap.fold (fun d before_d acc ->
      match Swrr_lazy.DelegateMap.find_opt d after.Swrr_lazy.delegates with
      | Some after_d ->
        let open Swrr_lazy in
        let changed =
          before_d.stake <> after_d.stake ||
          before_d.credit <> after_d.credit ||
          before_d.last_updated <> after_d.last_updated
        in
        if changed then acc + 1 else acc
      | None -> acc + 1
    ) before.Swrr_lazy.delegates 0
  in

  let print_changed_delegates (before : Swrr_lazy.state) (after : Swrr_lazy.state) : unit =
    Swrr_lazy.DelegateMap.iter (fun d before_d ->
      match Swrr_lazy.DelegateMap.find_opt d after.Swrr_lazy.delegates with
      | Some after_d when before_d <> after_d ->
        Format.printf "  Changed: %s@." d
      | _ -> ()
    ) before.Swrr_lazy.delegates
  in

  Format.printf "How many cycles? > %!";
  let cycles = read_int () in

  let rec run_cycles n st =
    match n with
    | 0 -> ()
    | _ ->
      Format.printf "@.=== Cycle %d ===@." (cycles - n + 1);
      let state0 = Swrr_lazy.update_state input st in
      let initial_changes = count_changed_entries st state0 in
      Format.printf "update_state changed %d delegates (should equal total: %d)@."
        initial_changes (List.length input);

      let rec test_steps m st count total_changes =
        match m with
        | 0 ->
            Format.printf "Total steps: %d@." count;
            Format.printf "Total updates to map (excluding finalize/update_state): %d@." total_changes;
            let ratio = float_of_int total_changes /. float_of_int count in
            Format.printf "Average updates per step (iteration only): %.4f@." ratio;
            st
        | _ ->
            let _, st' = Swrr_lazy.iteration input st in
            let changed = count_changed_entries st st' in
            if changed <> 1 then (
              Format.printf "Warning: step %d had %d changes@." st.Swrr_lazy.step changed;
              print_changed_delegates st st';
            );
            test_steps (m - 1) st' (count + 1) (total_changes + changed)
      in

      let last_state = test_steps (blocks_per_cycle - 1) state0 0 0 in
      let final_state = Swrr_lazy.finalize_cycle last_state in
      let changed = count_changed_entries last_state final_state in
      Format.printf "finalize_cycle changed %d delegates (should equal total count: %d)@."
        changed (List.length input);

      run_cycles (n - 1) final_state
  in

  run_cycles cycles empty_state

  | _ -> failwith "Invalid method id"

(* Entry point: handles user input and initiates allocation *)
let () =
  Format.printf "Choose allocation method:@.";
  Format.printf "1 - Alias method@.";
  Format.printf "2 - LowVAlloc - no memory@.";
  Format.printf "3 - MinVAlloc - no memory@.";
  Format.printf "4 - Smooth Weighted Round Robin (SWRR with memory - DATA ONLY FOR 3 CYCLES AVAILABLE). @.";
  Format.printf "5 - SSSA after MinVAlloc@.";
  Format.printf "6 - SWRR using map (fixed nb of cycles)@.";
  Format.printf "7 - lazy SWRR using map@.";
  Format.printf "8 - test lazy SWRR @.";
  Format.printf "> %!";
  try
    let method_id = read_int () in
    Format.printf "How many cycles? > %!";
    let n_cycles = read_int () in
    run_method method_id input_example n_cycles
  with _ ->
    Format.printf "Invalid input. Please enter valid numbers.@."
