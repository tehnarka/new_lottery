open Common

let available_list r =
  let rec go i acc = match i = r with true -> List.rev acc | false -> go (i+1) (i::acc) in
  go 0 []

let count_slots (out:output) =
  let rec loop i m =
    match i = Array.length out with
    | true -> m
    | false ->
        let d = Array.unsafe_get out i in
        let v = match StrMap.find_opt d m with Some x -> x | None -> 0 in
        loop (i+1) (StrMap.add d (v+1) m)
  in
  StrMap.bindings (loop 0 StrMap.empty)

(* remove k-th by rank; returns (elem, list_without, new_size) *)
let select_remove_by_rank (lst:int list) (k:int) (n:int) =
  let km = match n = 0 with true -> 0 | false -> mod_pos k n in
  let rec split i left rest =
    match i, rest with
    | 0, x::xs -> (x, List.rev_append left xs, n-1)
    | _, x::xs -> split (i-1) (x::left) xs
    | _, []    -> (0, List.rev left, 0)
  in
  split km [] lst

let run (out:output) : output =
  let r = Array.length out in
  match r = 0 with
  | true -> [||]
  | false ->
    let bakers =
      List.sort
        (fun (d1,q1) (d2,q2) -> match Stdlib.compare q2 q1 with 0 -> Stdlib.compare d1 d2 | k -> k)
        (count_slots out)
    in
    
    let assign_baker d q a n acc =
      match q = 0 with
      | true  -> (acc, a, n)
      | false ->
        let s = (float_of_int n) /. (float_of_int q) in
        let o = Random.float s in
        let rec step j a n accj =
          match j = q with
          | true  -> (accj, a, n)
          | false ->
            let p = int_of_float (floor (o +. (float_of_int j) *. s)) in
            let idx, a', n' = select_remove_by_rank a p n in
            step (j+1) a' n' ((idx, d) :: accj)
        in
        step 0 a n acc
    in
    let rec assign_all bs a n acc =
      match bs with
      | [] -> acc
      | (d,q)::tl ->
          let pos, a', n' = assign_baker d q a n [] in
          assign_all tl a' n' (pos @ acc)
    in
    let pairs = assign_all bakers (available_list r) r [] in
    let first = Array.unsafe_get out 0 in
    let res = Array.make r first in
    let rec fill = function [] -> () | (i,d)::tl -> Array.unsafe_set res i d; fill tl in
    fill pairs; res
