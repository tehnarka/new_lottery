(** 
  Stochastic Weighted Round Robin (SWRR) implementation using OCaml's standard Map module.

  Each delegate is associated with an integer credit value stored in an immutable Map (string → int),
  and selection proceeds by increasing credits proportionally to stake and choosing the highest.

  This implementation is:
  - Functional and pure (no mutation)
  - Memory-aware across steps
  - Compatible with lazy stream generation (via [stream])
*)
open Common

module DelegateMap = Map.Make(String) (* keys type string *)
type state = int DelegateMap.t

(** [init_state input] initializes the internal SWRR credit state for a given input list of (delegate * stake).
    Each delegate starts with 0 credits. 
*)
let init_state (input : input) : state =
  List.fold_left
    (fun acc (d, _) -> DelegateMap.add d 0 acc)
    DelegateMap.empty
    input


(** [iteration input state] performs a single SWRR step:
    - Increases each delegate's credit by its stake
    - Selects the delegate with the highest credit
    - Deducts the total stake from the winner's credit
    Returns the selected delegate and the updated state 
*)
let iteration (input : input) (state : state) : delegate * state =
  let total_stake = List.fold_left(fun acc (_, s) -> acc + s) 0 input in
  let updated_state = 
    List.fold_left
    (fun acc (d, s) -> 
    let c = DelegateMap.find d acc in
    DelegateMap.add d (c + s) acc 
    )
    state 
    input 
  in
  
  let best_delegate =
    DelegateMap.fold
      (fun d credit (best_d, best_c) ->
        if credit > best_c then (d, credit) else (best_d, best_c)
      )
      updated_state
      ("", min_int)
  in
let winner = fst best_delegate in
let winner_credit = DelegateMap.find winner updated_state in
let final_state = DelegateMap.add winner (winner_credit - total_stake) updated_state in
(winner, final_state)

(** [stream input state] creates a lazy, infinite sequence of SWRR delegates.
    Each element in the sequence corresponds to one step of the SWRR algorithm,
    starting from the given state. This allows step-by-step consumption without
    precomputing all slots in advance.
*)
let rec stream (input : input) (state : state) () : delegate Seq.node =
  let (delegate, next_state) = iteration input state in
  Seq.Cons (delegate, stream input next_state)