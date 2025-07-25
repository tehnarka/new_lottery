open Common

(* Повертає кількість слотів для кожного бейкера *)
let count_slots (output : output) : (delegate * int) list =
  let tbl = Hashtbl.create 128 in
  Array.iter (fun d ->
    let c = Hashtbl.find_opt tbl d |> Option.value ~default:0 in
    Hashtbl.replace tbl d (c + 1)
  ) output;
  Hashtbl.to_seq tbl |> List.of_seq

(* Головна функція Sorted Stride-Based Slot Allocation *)
let run (output : output) : output =
  let r = Array.length output in
  let counts = count_slots output in
  let sorted = List.sort (fun (_, q1) (_, q2) -> compare q2 q1) counts in
  let available = ref (List.init r Fun.id) in
  let assigned = Hashtbl.create 128 in

  List.iter (fun (delegate, quota) ->
    let n = List.length !available in
    let s = float n /. float quota in
    let o = Random.float s in
    let positions = List.init quota (fun j ->
      let pos = int_of_float (o +. float j *. s) in
      List.nth !available (pos mod n)
    ) in
    Hashtbl.add assigned delegate positions;
    available := List.filter (fun x -> not (List.mem x positions)) !available
  ) sorted;

  (* Побудова фінального масиву *)
  let result = Array.make r "" in
  Hashtbl.iter (fun d slots ->
    List.iter (fun idx -> result.(idx) <- d) slots
  ) assigned;
  result

