(* do not warn on unused value*)
[@@@ocaml.warning "-34-32"]

type delegate = string 
type delegate_stake = (delegate * int) 
type input = delegate_stake list
type output = delegate array 

let blocks_per_cycle = 10800

(* Pretty-printing helpers *)

let pp_delegate = Format.pp_print_string
let pp_delegate_stake fmt (delegate,stake) = Format.fprintf fmt "%a: %d" pp_delegate delegate stake
let pp_input fmt input = Format.(fprintf fmt "@[<v 2>Delegate stakes:@,%a@]" (pp_print_list pp_delegate_stake) input)

let pp_output fmt output =
  let open Format in 
  fprintf fmt "@[<v 2>Computed rights:@,";
  Array.iteri (fun i delegate -> fprintf fmt "L%d: %a@," i pp_delegate delegate) output;
  fprintf fmt "@]"


(* SWRR function *)

let compute_rights input : output =
  let total_stake = List.fold_left (fun acc (_, stake) -> acc + stake) 0 input in
  let credit_tbl = Hashtbl.create (List.length input) in
  List.iter (fun (d, _) -> Hashtbl.add credit_tbl d 0) input;

  let arr = Array.make blocks_per_cycle "" in
  
  for t = 0 to blocks_per_cycle - 1 do
    List.iter (fun (d, stake) ->
    let c = Hashtbl.find credit_tbl d in
    Hashtbl.replace credit_tbl d (c + stake)
    ) input;
    let (best_delegate, _) = 
    List.fold_left (fun (max_d, max_c) (d, _) ->
      let c = Hashtbl.find credit_tbl d in
      if c > max_c then (d,c) else (max_d, max_c)
      ) ("", min_int) input
    in

  arr.(t) <- best_delegate;

    let c = Hashtbl.find credit_tbl best_delegate in
    Hashtbl.replace credit_tbl best_delegate (c - total_stake)
  done;

  arr

let verify_swright_distribution (input : input) (output : output) (blocks_per_cycle : int) : unit =
  let total_stake = List.fold_left (fun acc (_, stake) -> acc + stake) 0 input in

  (* Count actual occurrences *)
  let counts = Hashtbl.create (List.length input) in
  Array.iter (fun delegate ->
    let c = Hashtbl.find_opt counts delegate |> Option.value ~default:0 in
    Hashtbl.replace counts delegate (c + 1)
  ) output;

  (* Sort for nice display *)
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) input in

  Format.printf "@[<v 2>SWRR distribution check:@,%10s  %10s  %10s  %10s@,"
    "Delegate" "Expected" "Actual" "Error";

  List.iter (fun (delegate, stake) ->
    let expected = float_of_int stake *. float_of_int blocks_per_cycle /. float_of_int total_stake in
    let actual = Hashtbl.find_opt counts delegate |> Option.value ~default:0 in
    let error = float_of_int actual -. expected in
    Format.printf "%10s  %10.3f  %10d  %+10.3f@," delegate expected actual error
  ) sorted;

  Format.printf "@]@."


  (*let input_example = [("A",3);("B",6);("C",4); ("D",5);("E",3)]


   let output_example = [|"B";"A";"B";"C"|]


  let () =Format.printf "Hello@." 
  let () =Format.printf "%d %s %a@." 100 "my string" Format.pp_print_int 25 *) 

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
  (*let () = Format.printf "%a@." pp_output output_example*)
  
  let output_computed = compute_rights input_example;;
  let () = Format.printf "%a@." pp_output output_computed
  let () = verify_swright_distribution input_example output_computed blocks_per_cycle


