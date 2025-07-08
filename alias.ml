[@@@ocaml.warning "-34-32"]

(* Types *)

type delegate = string
type delegate_stake = (delegate * int)
type input = delegate_stake list
type output = delegate array

(* Pretty-printing helpers *)

let pp_delegate = Format.pp_print_string
let pp_delegate_stake fmt (delegate, stake) =
  Format.fprintf fmt "%a: %d" pp_delegate delegate stake

let pp_input fmt input =
  Format.(fprintf fmt "@[<v 2>Delegate stakes:@,%a@]"
            (pp_print_list pp_delegate_stake) input)

let pp_output fmt output =
  let open Format in
  fprintf fmt "@[<v 2>Computed rights:@,";
  Array.iteri (fun i delegate -> fprintf fmt "L%d: %a@," i pp_delegate delegate) output;
  fprintf fmt "@]"

let blocks_per_cycle = 10800

(* Alias function *)

let build_alias_table input =
  let n = List.length input in
  let delegates = Array.of_list (List.map fst input) in
  let powers = Array.of_list (List.map snd input) in
  let total_power = Array.fold_left (+) 0 powers in
  let w = Array.map (fun p -> float_of_int p /. float_of_int total_power) powers in
  let q = Array.map (fun x -> float_of_int n *. x) w in

  let prob = Array.make n 0. in
  let alias = Array.make n 0 in

  let small = Queue.create () in
  let large = Queue.create () in

  Array.iteri (fun i qi ->
    if qi < 1.0 then Queue.push i small
    else Queue.push i large
  ) q;

  while not (Queue.is_empty small || Queue.is_empty large) do
    let i = Queue.pop small in
    let j = Queue.pop large in
    prob.(i) <- q.(i);
    alias.(i) <- j;
    q.(j) <- q.(j) -. (1.0 -. q.(i));
    if q.(j) < 1.0 then Queue.push j small else Queue.push j large
  done;

  Queue.iter (fun k -> prob.(k) <- 1.0; alias.(k) <- k) small;
  Queue.iter (fun k -> prob.(k) <- 1.0; alias.(k) <- k) large;

  (delegates, prob, alias)

(* RNG module abstraction *)
module type RNG = sig
  val uint32 : unit -> int
  val mass : unit -> float
end

module DefaultRng : RNG = struct
  let uint32 () = Random.bits ()
  let mass () = Random.float 1.0
end

let sample_baker (delegates, prob, alias) (module R : RNG) =
  let n = Array.length delegates in
  let j = R.uint32 () mod n in
  let u = R.mass () in
  if u < prob.(j) then delegates.(j)
  else delegates.(alias.(j))


(* Allocation verification *)

let verify_distribution_alias (input : input) (output : output) (blocks_per_cycle : int) : unit =
  (* Compute total stake *)
  let total_stake = List.fold_left (fun acc (_, stake) -> acc + stake) 0 input in

  (* Count actual selections *)
  let counts = Hashtbl.create (List.length input) in
  Array.iter (fun delegate ->
    let count = Hashtbl.find_opt counts delegate |> Option.value ~default:0 in
    Hashtbl.replace counts delegate (count + 1)
  ) output;

  (* Sort input for readability *)
  let sorted_input = List.sort (fun (d1, _) (d2, _) -> compare d1 d2) input in

  Format.printf "@[<v 2>Alias method distribution check:@,%10s  %10s  %10s  %10s@,"
    "Delegate" "Expected" "Actual" "Error";

  List.iter (fun (delegate, stake) ->
    let expected = float_of_int stake *. float_of_int blocks_per_cycle /. float_of_int total_stake in
    let actual = Hashtbl.find_opt counts delegate |> Option.value ~default:0 in
    let error = float_of_int actual -. expected in
    Format.printf "%10s  %10.3f  %10d  %+10.3f@," delegate expected actual error
  ) sorted_input;

  Format.printf "@]@."



let input_example = [
  ("A", 405104);
  ("B", 11780);
  ("C", 17839);
  ("D", 266224);
  ("E", 405526);
  ("F", 210556);
  ("G", 125431);
  ("H", 57887);
  ("I", 367611);
  ("J", 50197);
  ("K", 95404);
  ("L", 33182);
  ("M", 1577474);
  ("N", 340158);
  ("O", 53601);
  ("P", 93952);
  ("Q", 11423);
  ("R", 284139);
  ("S", 169104);
  ("T", 70642);
  ("U", 392941);
  ("V", 36202);
  ("W", 2050839);
  ("X", 1946154);
  ("Y", 486086);
  ("Z", 2528851);
  ("AA", 131949);
  ("AB", 4997937);
  ("AC", 81058);
  ("AD", 15648882);
  ("AE", 81982);
  ("AF", 87698);
  ("AG", 57577);
  ("AH", 408462);
  ("AI", 795986);
  ("AJ", 436610);
  ("AK", 659822);
  ("AL", 54752461);
  ("AM", 10235);
  ("AN", 293025);
  ("AO", 23615);
  ("AP", 2626463);
  ("AQ", 1244436);
  ("AR", 669273);
  ("AS", 5122226);
  ("AT", 32881);
  ("AU", 106049);
  ("AV", 1628729);
  ("AW", 21634);
  ("AX", 1510898);
  ("AY", 668011);
  ("AZ", 1067247);
  ("BA", 2585245);
  ("BB", 396958);
  ("BC", 6190);
  ("BD", 199373);
  ("BE", 6714238);
  ("BF", 185329);
  ("BG", 248124);
  ("BH", 5929984);
  ("BI", 379414);
  ("BJ", 1513374);
  ("BK", 194877);
  ("BL", 22378588);
  ("BM", 7064083);
  ("BN", 1562343);
  ("BO", 756535);
  ("BP", 14441);
  ("BQ", 27315);
  ("BR", 356170);
  ("BS", 80431);
  ("BT", 287033);
  ("BU", 8305935);
  ("BV", 850836);
  ("BW", 105068122);
  ("BX", 7191);
  ("BY", 922965);
  ("BZ", 346062);
  ("CA", 1831482);
  ("CB", 109728);
  ("CC", 84495);
  ("CD", 1320201);
  ("CE", 10122111);
  ("CF", 279409);
  ("CG", 65217);
  ("CH", 131639);
  ("CI", 51618);
  ("CJ", 147640);
  ("CK", 67123);
  ("CL", 23118);
  ("CM", 989839);
  ("CN", 610568);
  ("CO", 66367);
  ("CP", 904405);
  ("CQ", 2449692);
  ("CR", 1761790);
  ("CS", 32707173);
  ("CT", 81898);
  ("CU", 408790);
  ("CV", 2260694);
]

let () = Format.printf "%a@." pp_input input_example

let (delegates, prob, alias) = build_alias_table input_example

let output_computed =
  Array.init blocks_per_cycle (fun _ ->
    sample_baker (delegates, prob, alias) (module DefaultRng))

let () = Format.printf "%a@." pp_output output_computed

let () = verify_distribution_alias input_example output_computed blocks_per_cycle
