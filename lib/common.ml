
type delegate = string
type delegate_stake = delegate * int
type input = delegate_stake list
type output = delegate array

let blocks_per_cycle = 10800

let pp_delegate = Format.pp_print_string


let verify_distribution input output blocks =
  let total_stake = List.fold_left (fun acc (_, s) -> acc + s) 0 input in
  let count_tbl = Hashtbl.create (List.length input) in
  Array.iter (fun d ->
    let c = Hashtbl.find_opt count_tbl d |> Option.value ~default:0 in
    Hashtbl.replace count_tbl d (c + 1)
  ) output;
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) input in
  Format.printf "@[<v 2>Distribution:@,%10s %10s %10s %10s@," "Delegate" "Expected" "Actual" "Error";
  List.iter (fun (d, s) ->
    let expected = float s *. float blocks /. float total_stake in
    let actual = Hashtbl.find_opt count_tbl d |> Option.value ~default:0 in
    let error = float actual -. expected in
    Format.printf "%10s %10.2f %10d %+10.2f@," d expected actual error
  ) sorted;
  Format.printf "@]@."

let input_example = [
  ("A", 405104); ("B", 11780); ("C", 17839); ("D", 266224);  ("E", 405526); 
  ("F", 210556); ("G", 125431); ("H", 57887); ("I", 367611); ("J", 50197);
  ("K", 95404); ("L", 33182); ("M", 1577474); ("N", 340158); ("O", 53601);
  ("P", 93952); ("Q", 11423); ("R", 284139); ("S", 169104); ("T", 70642);
  ("U", 392941); ("V", 36202); ("W", 2050839); ("X", 1946154); ("Y", 486086);
  ("Z", 2528851); ("AA", 131949); ("AB", 4997937); ("AC", 81058); ("AD", 15648882);
  ("AE", 81982); ("AF", 87698); ("AG", 57577); ("AH", 408462); ("AI", 795986);
  ("AJ", 436610); ("AK", 659822); ("AL", 54752461); ("AM", 10235); ("AN", 293025);
  ("AO", 23615); ("AP", 2626463); ("AQ", 1244436); ("AR", 669273); ("AS", 5122226);
  ("AT", 32881); ("AU", 106049); ("AV", 1628729); ("AW", 21634); ("AX", 1510898);
  ("AY", 668011); ("AZ", 1067247); ("BA", 2585245); ("BB", 396958); ("BC", 6190);
  ("BD", 199373); ("BE", 6714238); ("BF", 185329); ("BG", 248124); ("BH", 5929984);
  ("BI", 379414); ("BJ", 1513374); ("BK", 194877); ("BL", 22378588); ("BM", 7064083);
  ("BN", 1562343); ("BO", 756535); ("BP", 14441); ("BQ", 27315); ("BR", 356170);
  ("BS", 80431); ("BT", 287033); ("BU", 8305935); ("BV", 850836); ("BW", 105068122);
  ("BX", 7191); ("BY", 922965); ("BZ", 346062); ("CA", 1831482); ("CB", 109728);
  ("CC", 84495); ("CD", 1320201); ("CE", 10122111); ("CF", 279409); ("CG", 65217);
  ("CH", 131639); ("CI", 51618); ("CJ", 147640); ("CK", 67123); ("CL", 23118);
  ("CM", 989839); ("CN", 610568); ("CO", 66367); ("CP", 904405); ("CQ", 2449692);
  ("CR", 1761790); ("CS", 32707173); ("CT", 81898); ("CU", 408790); ("CV", 2260694);
]
