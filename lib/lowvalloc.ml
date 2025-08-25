open Common
module Make (N : NUMBER) = struct
  module M = Map.Make(String)

  let run (input : (delegate * N.t) list) : output =
    match input with
    | [] -> [||]
    | _ ->
      let r = blocks_per_cycle in
      (* Ei, gi=floor(Ei), fi=Ei-gi (steps 6–10) *)
      let total_f =
        List.fold_left (fun acc (_, s) -> acc +. N.to_float s) 0.0 input
      in
      let triples =
        List.map
          (fun (d, s) ->
             let ei = (N.to_float s) *. float_of_int r /. total_f in
             let gi = int_of_float (floor ei) in
             let fi = ei -. floor ei in
             (d, gi, fi))
          input
      in
      let sum_base = List.fold_left (fun acc (_, g, _) -> acc + g) 0 triples in
      let remaining = r - sum_base in

      (* winners: top `remaining` by fi, one per delegate — steps 13–16 *)
      let sorted =
        List.sort (fun (_,_,f1) (_,_,f2) -> Stdlib.compare f2 f1) triples
      in
      let rec take k lst acc =
        match k, lst with
        | 0, _ -> List.rev acc
        | _, [] -> List.rev acc
        | _, (d, _, _) :: tl -> take (k - 1) tl (d :: acc)
      in
      let winners = take remaining sorted [] in

      (* Phase 1 (lines 5–11): contiguous blocks in the order of input *)
      let base_chunks =
        let rec loop acc = function
          | [] -> List.rev acc
          | (d, g, _) :: tl ->
              let g' = Common.clamp_nonneg g in
              let chunk =
                match g' = 0 with
                | true -> [||]
                | false -> Array.make g' d
              in
              loop (chunk :: acc) tl
        in
        loop [] triples
      in

      (* Phase 2 (lines 13–18): remaining single rights in the order of winners *)
      let residual_chunk =
        let k = List.length winners in
        match k = 0 with
        | true -> [||]
        | false ->
            let d0 = match input with (d, _) :: _ -> d | [] -> "" in
            let a = Array.make k d0 in
            let rec fill i ws =
              match ws with
              | [] -> a
              | d :: tl -> Array.unsafe_set a i d; fill (i + 1) tl
            in
            fill 0 winners
      in

      let all = Array.concat (base_chunks @ [residual_chunk]) in
      take_array all r
end
