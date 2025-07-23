open Common

type state = (delegate, int) Hashtbl.t

let init_state (input : input) : state =
  let tbl = Hashtbl.create (List.length input) in
  List.iter (fun (d, _) -> Hashtbl.add tbl d 0) input;
  tbl

let run_with_state (input : input) (credit_tbl : state) : output * state =
  let total_stake = List.fold_left (fun acc (_, s) -> acc + s) 0 input in
  let arr = Array.make blocks_per_cycle "" in

  for t = 0 to blocks_per_cycle - 1 do
    (* Increase credit of all delegates by their stake *)
    List.iter (fun (d, s) ->
      let c = Hashtbl.find credit_tbl d in
      Hashtbl.replace credit_tbl d (c + s)
    ) input;

    (* Select delegate with max credit *)
    let best, _ = List.fold_left (fun (md, mc) (d, _) ->
      let c = Hashtbl.find credit_tbl d in
      if c > mc then (d, c) else (md, mc)
    ) ("", min_int) input in

    arr.(t) <- best;

    (* Reduce credit of winner by total stake *)
    let c = Hashtbl.find credit_tbl best in
    Hashtbl.replace credit_tbl best (c - total_stake)
  done;

  arr, credit_tbl
