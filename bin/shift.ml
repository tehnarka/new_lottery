open Common

(* Counts how many slots are assigned to each delegate in the given output array *)
let count_slots (output : output) : (delegate * int) list =
  let tbl = Hashtbl.create 128 in
  Array.iter (fun d ->
    let c = Hashtbl.find_opt tbl d |> Option.value ~default:0 in
    Hashtbl.replace tbl d (c + 1)
  ) output;
  Hashtbl.to_seq tbl |> List.of_seq

(* Main function Sorted Stride-Based Slot Allocation *)
let run (output : output) : output =
  let r = Array.length output in

  (* Count the number of slots each delegate currently holds *)
  let counts = count_slots output in

  (* Sort delegates by descending order of slot counts (quota) *)
  let sorted = List.sort (fun (_, q1) (_, q2) -> compare q2 q1) counts in

  (* Initialize list of available slot indices *)
  let available = ref (List.init r Fun.id) in

  (* Table to store assigned slot positions for each delegate *)
  let assigned = Hashtbl.create 128 in

  (* Distribute slots to delegates using stride-based allocation *)
  List.iter (fun (delegate, quota) ->
    let n = List.length !available in

    (* Compute stride (s) based on available slots and delegate's quota *)
    let s = float n /. float quota in

    (* Random offset to evenly distribute delegate's slots *)
    let o = Random.float s in

    (* Compute and assign positions for the delegate *)
    let positions = List.init quota (fun j ->
      let pos = int_of_float (o +. float j *. s) in
      List.nth !available (pos mod n)
    ) in

    (* Record assigned positions and update available slots *)
    Hashtbl.add assigned delegate positions;
    available := List.filter (fun x -> not (List.mem x positions)) !available
  ) sorted;

  (* Construct the final result array by assigning delegates to their slots *)
  let result = Array.make r "" in
  Hashtbl.iter (fun d slots ->
    List.iter (fun idx -> result.(idx) <- d) slots
  ) assigned;
  result

