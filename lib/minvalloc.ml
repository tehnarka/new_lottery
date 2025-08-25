(* lib/minvalloc.ml *)
open Common

module Make (N : NUMBER) = struct
  (* MinVAlloc — 1:1 with pseudocode.
     s       : normalized stake vector
     r       : total rights per cycle (R in pseudocode)
     e       : entitlements = r * s
     cE      : cumulative_sum(e) with cE[0] = 0 and cE[n] forced to r (removes FP drift)
     u       : uniform shift in [0,1)
     r_alloc ~ 𝓡 : counts per delegate; then expanded into an array of length r.
  *)

  let run_with_u (u : float) (input : (delegate * N.t) list) : output =
    match input with
    | [] -> [||]
    | _  ->
      (* r ≡ R *)
      let r = blocks_per_cycle in

      (* delegates and weights (float) *)
      let delegates = input |> List.map fst |> Array.of_list in
      let w_float   = input |> List.map (fun (_, w) -> N.to_float w) in

      (* s: normalized weights *)
      let total = List.fold_left ( +. ) 0.0 w_float in
      let s =
        match Stdlib.compare total 0.0 with
        | 0 -> List.map (fun _ -> 0.0) w_float
        | _ -> List.map (fun x -> x /. total) w_float
      in

      (* e = r * s *)
      let rf = float_of_int r in
      let e  = List.map (fun si -> rf *. si) s in

      (* cE: cumulative_sum(e) with cE[0] = 0; last element forced to r *)
      let rec cum sum acc = function
        | []      -> List.rev acc
        | x :: tl ->
          let sum' = sum +. x in
          cum sum' (sum' :: acc) tl
      in
      let cE_tail = cum 0.0 [] e in              (* [e1; e1+e2; ...; ≈ r] *)
      let cE_raw  = 0.0 :: cE_tail in            (* length = n+1 *)

      let rec replace_last lst =
        match lst with
        | []      -> [rf]
        | [_]     -> [rf]
        | h :: tl -> h :: replace_last tl
      in
      let cE_list = replace_last cE_raw in       (* [0; ...; r] exactly *)
      let cE      = Array.of_list cE_list in

      (* 𝓡[i] = floor(u + cE[i]) - floor(u + cE[i-1]) for i=1..n *)
      let n = Array.length delegates in
      let rec counts_loop i acc =
        match Stdlib.compare i n with
        | 1 -> List.rev acc
        | _ ->
          let ci   = Array.unsafe_get cE i in
          let cim1 = Array.unsafe_get cE (i - 1) in
          let lb   = int_of_float (Stdlib.floor (u +. ci)) in
          let la   = int_of_float (Stdlib.floor (u +. cim1)) in
          counts_loop (i + 1) ((lb - la) :: acc)
      in
      let r_alloc = counts_loop 1 [] in  (* length = n *)

      (* Expand counts into an array of rights.
         Order is not important; if you use Shift/SSSA afterwards,
         it will evenly spread over [0, r). *)
      let rec expand i cs acc =
        match cs with
        | [] -> Array.concat (List.rev acc)
        | c :: tl ->
          let d = Array.unsafe_get delegates i in
          let chunk =
            match Stdlib.compare c 0 with
            | -1 | 0 -> [||]
            | _      -> Array.make c d
          in
          expand (i + 1) tl (chunk :: acc)
      in
      let out = expand 0 r_alloc [] in
      (* To guard against FP rounding issues in floor(...): adjust length to r *)
      take_array out r

  let run input = run_with_u (Random.float 1.0) input
end
