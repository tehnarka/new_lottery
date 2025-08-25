open Common

module type S = sig
  module DelegateMap : Map.S with type key = string

  module Lazy : sig
    type delegate_state = { stake : int; credit : int; last_updated : int }
    type state = { delegates : delegate_state DelegateMap.t; step : int }

    val finalize_cycle : state -> state
    val update_state   : input -> state -> state
    val iteration      : input -> state -> delegate * state
    val run            : input -> output
  end

  module Eager : sig
    type state
    val run : input -> output
  end

  type delegate_state = Lazy.delegate_state
  type state          = Lazy.state
  val finalize_cycle  : state -> state
  val update_state    : input -> state -> state
  val iteration       : input -> state -> delegate * state
  val run             : input -> output
end

module Impl : S = struct
  module DelegateMap = Map.Make (String)

  module Lazy = struct
    type delegate_state = { stake : int; credit : int; last_updated : int }
    type state = { delegates : delegate_state DelegateMap.t; step : int }

    let clamp_nonneg x =
      match Stdlib.compare x 0 with
      | -1 -> 0
      | _  -> x

    let finalize_cycle state =
      let ds =
        DelegateMap.map
          (fun { stake; credit; last_updated } ->
             let steps = clamp_nonneg (state.step - last_updated) in
             { stake; credit = credit + stake * steps; last_updated = state.step })
          state.delegates
      in
      { state with delegates = ds }

    let update_state input state =
      let prev = state.delegates in
      let ds =
        List.fold_left
          (fun acc (d, new_stake) ->
             let credit =
               match DelegateMap.find_opt d prev with
               | Some { credit; _ } -> credit
               | None -> 0
             in
             DelegateMap.add d { stake = new_stake; credit; last_updated = 0 } acc)
          DelegateMap.empty input
      in
      { delegates = ds; step = 0 }

    let iteration input state =
      let total = List.fold_left (fun a (_, s) -> a + s) 0 input in
      let winner, eff, wds =
        DelegateMap.fold
          (fun d ({ stake; credit; last_updated } as ds) (bd, bc, bds) ->
             let e = credit + stake * (state.step - last_updated) in
             match Stdlib.compare e bc with
             | 1 -> (d, e, ds)
             | _ -> (bd, bc, bds))
          state.delegates ("", min_int, { stake = 0; credit = min_int; last_updated = 0 })
      in
      let delegates =
        DelegateMap.add winner
          { wds with credit = eff - total; last_updated = state.step }
          state.delegates
      in
      (winner, { delegates; step = state.step + 1 })

    let run input =
      match input with
      | [] -> [||]
      | (d0, _) :: _ ->
        let r = blocks_per_cycle in
        let out = Array.make r d0 in
        let st0 =
          {
            delegates =
              List.fold_left
                (fun m (d, stake) ->
                   DelegateMap.add d { stake; credit = 0; last_updated = 0 } m)
                DelegateMap.empty input;
            step = 0;
          }
        in
        let rec loop i st =
          match Stdlib.compare i r with
          | 0 -> out
          | _ ->
            let (w, st') = iteration input st in
            Array.unsafe_set out i w;
            loop (i + 1) st'
        in
        loop 0 st0
  end

  module Eager = struct
    module M = Map.Make (String)
    type state = { credits : int M.t; total : int }

    let init input =
      let total = List.fold_left (fun a (_, w) -> a + w) 0 input in
      let credits = List.fold_left (fun m (d, _) -> M.add d 0 m) M.empty input in
      { credits; total }

    let step input st =
      let inc =
        List.fold_left
          (fun m (d, w) ->
             let c = match M.find_opt d m with Some v -> v | None -> 0 in
             M.add d (c + w) m)
          st.credits input
      in
      let best =
        M.fold
          (fun d c acc ->
             match acc with
             | None -> Some (d, c)
             | Some (_, bc) ->
                 (match Stdlib.compare c bc with 1 -> Some (d, c) | _ -> acc))
          inc None
      in
      match best with
      | None -> ("", st)
      | Some (d, c) ->
          let credits' = M.add d (c - st.total) inc in
          (d, { st with credits = credits' })

    let run input =
      match input with
      | [] -> [||]
      | (d0, _) :: _ ->
        let r = blocks_per_cycle in
        let out = Array.make r d0 in
        let rec loop i st =
          match Stdlib.compare i r with
          | 0 -> out
          | _ ->
            let (w, st') = step input st in
            Array.unsafe_set out i w;
            loop (i + 1) st'
        in
        loop 0 (init input)
  end

  type delegate_state = Lazy.delegate_state
  type state          = Lazy.state
  let finalize_cycle  = Lazy.finalize_cycle
  let update_state    = Lazy.update_state
  let iteration       = Lazy.iteration
  let run             = Lazy.run
end

include Impl
