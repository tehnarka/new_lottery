open Common

(** SWRR (Smooth Weighted Round Robin) implementation using imperative Hashtbl.
    Each delegate accumulates "credit" proportional to its stake.
    In each slot, the delegate with the highest credit is selected and debited. *)

type state = (delegate, int) Hashtbl.t

(** [init_state input] initializes a new credit table where all delegates start with 0 credit.
    Used at the beginning of a cycle. *)
let init_state (input : input) : state =
  let tbl = Hashtbl.create (List.length input) in
  List.iter (fun (d, _) -> Hashtbl.add tbl d 0) input;
  tbl

  (** [update_state prev_tbl new_input] creates a fresh credit table for a new input list.
    Preserves existing credit for delegates already present in [prev_tbl],
    initializes new ones to 0. Used to support updated delegate sets between cycles. *)
let update_state (prev_tbl : state) (new_input : input) : state =
  let new_tbl = Hashtbl.create (List.length new_input) in
  List.iter (fun (d, _) ->
    let prev_credit = Hashtbl.find_opt prev_tbl d |> Option.value ~default:0 in
    Hashtbl.add new_tbl d prev_credit
  ) new_input; 
  new_tbl

(** [run_with_state input prev_credit_tbl] simulates a full cycle of slot assignments (e.g., 10800 blocks).
    It uses a mutable credit table and returns:
    - [output]: an array of delegates assigned per slot
    - [state]: updated credit table, to be reused in the next cycle
    If a participant is absent one cycle, their credit will expire and they will be issued zero credit upon return.
*)
let run_with_state (input : input) (prev_credit_tbl : state) : output * state =
  let credit_tbl = update_state prev_credit_tbl input in
  let total_stake = List.fold_left (fun acc (_, s) -> acc + s) 0 input in
  let arr = Array.make blocks_per_cycle "" in

  for t = 0 to blocks_per_cycle - 1 do
    (* 1. Accumulate credit for each delegate based on their stake *)
    List.iter (fun (d, s) ->
      let c = Hashtbl.find credit_tbl d in
      Hashtbl.replace credit_tbl d (c + s)
    ) input;

    (* 2. Select delegate with maximum accumulated credit using Hashtbl.fold; updated due to Julien *)
    let best, _ =
      Hashtbl.fold
        (fun delegate credit (max_delegate, max_credit) ->
           if credit > max_credit then (delegate, credit)
           else (max_delegate, max_credit))
        credit_tbl
        ("", min_int)
    in

    (* 3. Assign selected delegate to current slot *)
    arr.(t) <- best;

    (* 4. Deduct total stake from winner's credit *)
    let c = Hashtbl.find credit_tbl best in
    Hashtbl.replace credit_tbl best (c - total_stake)
  done;

  arr, credit_tbl


