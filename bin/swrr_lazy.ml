(**
  Lazy SWRR variants

  This module describes two functionally equivalent, lazily-updated variants of
  Smooth Weighted Round Robin (SWRR). The goal in both cases is to avoid
  updating the credit state of every delegate at every block,
  while preserving the deterministic and fair behavior of classic SWRR.

  Both versions assume a fixed stake per delegate during a cycle and require
  a global "now" counter (representing the current block index within the cycle).

  Before updating delegate stakes between cycles, you must first convert all 
  implicit credits into explicit ones—by computing each delegate’s current 
  credit—so that the new stakes do not break the correctness of credit-based 
  selection in both lazy variants.
  
  -------------------------------------------------------------------------------
  Variant A — Algebraic Lazy SWRR (using `times_chosen`)
  -------------------------------------------------------------------------------

  In this formulation, each delegate tracks only two values:

    - [stake]         : their current stake (weight)
    - [times_chosen]  : how many times they have been selected so far in the cycle

  The effective credit for a delegate at round [now] is computed as:

       [credit = stake × now − total_stake × times_chosen]

  This formula reflects the net credit that would have accumulated in classic SWRR:
  stake is added each round, and total_stake is subtracted whenever the delegate wins.

  Only the winning delegate is updated (by incrementing [times_chosen]).
  All others remain unchanged between rounds.

  This is the most minimal and purely lazy implementation:
    - No explicit credit tracking
    - No need for last-updated timestamps
    - No map-wide mutations
    - Fully deterministic and fair

  -------------------------------------------------------------------------------
  Variant B — Timestamp-Based Lazy SWRR (using `last_updated`)
  -------------------------------------------------------------------------------

  This variant is closer to the classical SWRR style. Each delegate stores:

    - [stake]          : their current stake
    - [credit]         : their last known explicit credit
    - [last_updated]   : the round at which [credit] was last updated

  The effective credit at round [now] is calculated as:

      [credit + stake × (now − last_updated)]

  At each block:
    - We compute the effective credit for all delegates to select the winner.
    - The winner's credit is recomputed and decreased by total_stake.
    - The winner’s [credit] and [last_updated] are updated accordingly.

  This implementation still avoids full-map traversal for updates, but it involves
  more state per delegate and slightly more mutation logic.

  -------------------------------------------------------------------------------
  Summary Comparison

    - Both versions yield identical selection sequences to classical SWRR.
    - Variant A is cleaner, more compact, and easier to reason about.
    - Variant B is closer to the textbook model and allows partial precomputation.

*)

(* state: number of iteration which consist of rounds and levels
example: level 1 round 3 --> state = 4 *)

open Common

module DelegateMap = Map.Make(String)

(* structure for state for one delegqte*)
type delegate_state = {
  stake : int;
  credit : int;
  last_updated : int
}

type state = { 
  delegates : delegate_state DelegateMap.t;
  step : int
}

(** [update_state input state] applies a new set of stakes for the next cycle.
    - Keeps [credit] unchanged.
    - Sets [stake] from [input].
    - Resets [last_updated := 0] for all delegates.
    - Adds new delegates with [credit = 0].
    - Removes delegates not present in the new [input].
    - Resets [step] to 0 to start the new cycle cleanly.
    This function must be called after [finalize_cycle]. *)
let update_state (input : input) (state : state) : state =
  let new_delegates =
    List.fold_left
      (fun acc (d, new_stake) ->
        DelegateMap.update d
          (function
            | Some old -> Some { old with stake = new_stake; last_updated = 0 }
            | None -> Some { stake = new_stake; credit = 0; last_updated = 0 }
          )
          acc
      )
      DelegateMap.empty
      input
  in
  { delegates = new_delegates; step = 0 }


  (** [finalize_cycle state] materializes all virtual credit by
    computing the accumulated credit from [stake × (step − last_updated)]
    for each delegate. It then sets [last_updated := step] to freeze this state.
    This must be called at the end of every cycle, before [update_state]. *)
let finalize_cycle (state : state) : state =
  let updated_delegates =
    DelegateMap.map
      (fun { stake; credit; last_updated } ->
        let steps_passed = state.step - last_updated in
        {
          stake;
          credit = credit + stake * steps_passed;
          last_updated = state.step;
        }
      )
      state.delegates
  in
  { state with delegates = updated_delegates }


  (** [iteration input state] performs a single Lazy SWRR step:
    - Computes [effective_credit = credit + stake × (step − last_updated)] for each delegate;
    - Selects the delegate with the highest effective credit;
    - Deducts [total_stake] from the winner’s credit;
    - Updates the winner’s [credit] and [last_updated := step];
    - Increments [step] by 1.
    Returns the selected delegate and the updated state. *)
let iteration (input : input) (state : state) : delegate * state =
  let total_stake = List.fold_left (fun acc (_, s) -> acc + s) 0 input in

  (* Find the delegate with the highest effective credit *)
  let winner, _ =
    DelegateMap.fold
      (fun d { stake; credit; last_updated } (best_d, best_score) ->
        let effective = credit + stake * (state.step - last_updated) in
        if effective > best_score then (d, effective) else (best_d, best_score)
      )
      state.delegates
      ("", min_int)
  in

  (* Update only the winner *)
  let updated_delegates =
    DelegateMap.update winner
      (function
        | Some { stake; credit; last_updated } ->
            let effective = credit + stake * (state.step - last_updated) in
            Some {
              stake;
              credit = effective - total_stake;
              last_updated = state.step;
            }
        | None -> None (* Should not happen *)
      )
      state.delegates
  in

  (winner, { delegates = updated_delegates; step = state.step + 1 })