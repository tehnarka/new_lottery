(* list -> *)
open Common

module DelegateMap = Map.Make(String) (* keys type string *)
type state = int DelegateMap.t


let init_state (input : input) : state =
  List.fold_left
    (fun acc (d, _) -> DelegateMap.add d 0 acc)
    DelegateMap.empty
    input


(* update_state *)

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
