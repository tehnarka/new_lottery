(* test.ml *)
open Common

module MinvallocInt = Minvalloc.Make (IntN)
module LowvallocInt  = Lowvalloc.Make (IntN)

let rounds = 10

(* round 0 (base) *)
let base_minvalloc (inp : (delegate * int) list) : output =
  MinvallocInt.run inp |> Shift.run

let base_lowvalloc (inp : (delegate * int) list) : output =
  LowvallocInt.run inp |> Shift.run

let base_swrr (inp : (delegate * int) list) : output =
  Swrr.run inp


let build_matrix_minvalloc (inp : (delegate * int) list) : output array =
  base_minvalloc inp |> fun base -> rounds_matrix_right base rounds 3

let build_matrix_lowvalloc (inp : (delegate * int) list) : output array =
  base_lowvalloc inp |> fun base -> rounds_matrix_right base rounds 3

let build_matrix_swrr (inp : (delegate * int) list) : output array =
  base_swrr inp |> fun base -> rounds_matrix_left base rounds 2


let input_int : (string * int) list = [
  ("A", 405104); ("B", 11780); ("C", 17839); ("D", 266224); ("E", 405526); ("F", 210556); 
  ("G", 125431); ("H", 57887); ("I", 367611); ("J", 50197); ("K", 95404); ("L", 33182); 
  ("M", 1577474); ("N", 340158); ("O", 53601); ("P", 93952); ("Q", 11423); ("R", 284139); 
  ("S", 169104); ("T", 70642); ("U", 392941); ("V", 36202); ("W", 2050839); ("X", 1946154); 
  ("Y", 486086); ("Z", 2528851); ("AA", 131949); ("AB", 4997937); ("AC", 81058); ("AD", 15648882); 
  ("AE", 81982); ("AF", 87698); ("AG", 57577); ("AH", 408462); ("AI", 795986); ("AJ", 436610); 
  ("AK", 659822); ("AL", 54752461); ("AM", 10235); ("AN", 293025); ("AO", 23615); ("AP", 2626463); 
  ("AQ", 1244436); ("AR", 669273); ("AS", 5122226); ("AT", 32881); ("AU", 106049); ("AV", 1628729); 
  ("AW", 21634); ("AX", 1510898); ("AY", 668011); ("AZ", 1067247); ("BA", 2585245); ("BB", 396958); 
  ("BC", 6190); ("BD", 199373); ("BE", 6714238); ("BF", 185329); ("BG", 248124); ("BH", 5929984); 
  ("BI", 379414); ("BJ", 1513374); ("BK", 194877); ("BL", 22378588); ("BM", 7064083); ("BN", 1562343); 
  ("BO", 756535); ("BP", 14441); ("BQ", 27315); ("BR", 356170); ("BS", 80431); ("BT", 287033); ("BU", 8305935); 
  ("BV", 850836); ("BW", 105068122); ("BX", 7191); ("BY", 922965); ("BZ", 346062); ("CA", 1831482); ("CB", 109728); 
  ("CC", 84495); ("CD", 1320201); ("CE", 10122111); ("CF", 279409); ("CG", 65217); ("CH", 131639); ("CI", 51618); 
  ("CJ", 147640); ("CK", 67123); ("CL", 23118); ("CM", 989839); ("CN", 610568); ("CO", 66367); ("CP", 904405); 
  ("CQ", 2449692); ("CR", 1761790); ("CS", 32707173); ("CT", 81898); ("CU", 408790); ("CV", 2260694);
]

let () =
  let () = Random.self_init () in

  (* round 0: length = blocks_per_cycle *)
  let b_min = base_minvalloc input_int in
  let b_low = base_lowvalloc  input_int in
  let b_swr = base_swrr       input_int in


  let m_min = build_matrix_minvalloc input_int in
  let m_low = build_matrix_lowvalloc  input_int in
  let m_swr = build_matrix_swrr       input_int in

  Format.printf "@.=== Matrices built (rounds=%d × levels=%d) ===@."
    rounds blocks_per_cycle;

  let start = 0 in
  let count = 30 in
  Common.print_rounds_rows_levels_cols_window "MinVAlloc + SSSA (→3)" m_min start count;
  Common.print_rounds_rows_levels_cols_window "LowVAlloc + SSSA (→3)" m_low start count;
  Common.print_rounds_rows_levels_cols_window "SWRR (←2)"             m_swr start count;

  (* check only for round 0 *)
  Format.printf "@.=== Verify round 0 (all %d levels) ===@." Common.blocks_per_cycle;

  Format.printf "@.MinVAlloc + SSSA (round 0)@.";
  Common.verify_distribution input_int b_min Common.blocks_per_cycle;

  Format.printf "@.LowVAlloc + SSSA (round 0)@.";
  Common.verify_distribution input_int b_low Common.blocks_per_cycle;

  Format.printf "@.SWRR (round 0)@.";
  Common.verify_distribution input_int b_swr Common.blocks_per_cycle;
  ()

