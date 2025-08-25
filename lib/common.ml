[@@@ocaml.warning "-32-34"]

type delegate = string
type delegate_stake = delegate * int
type input = delegate_stake list
type output = delegate array

let blocks_per_cycle : int = 10800

module StrMap = Map.Make (String)

module type NUMBER = sig
  type t
  val zero : t
  val succ : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val compare : t -> t -> int
  val to_float : t -> float
end

module IntN : NUMBER with type t = int = struct
  type t = int
  let zero = 0
  let succ x = x + 1
  let add (a:int) (b:int) = a + b
  let mul (a:int) (b:int) = a * b
  let compare = Stdlib.compare
  let to_float = float_of_int
end

module Int64N : NUMBER with type t = int64 = struct
  type t = int64
  let zero = 0L
  let succ x = Int64.succ x
  let add = Int64.add
  let mul = Int64.mul
  let compare = Int64.compare
  let to_float = Int64.to_float
end

let clamp_nonneg (x:int) : int =
  match Stdlib.compare x 0 with
  | -1 -> 0
  | _  -> x

let rec mod_pos a n =
  match Stdlib.compare a 0 with
  | -1 -> mod_pos (a + n) n
  | 0  -> 0
  | _  ->
    match Stdlib.compare a n with
    | -1 -> a
    | 0  -> 0
    | _  -> mod_pos (a - n) n

let rotate_right (arr:output) (k:int) : output =
  let n = Array.length arr in
  match n = 0 with
  | true  -> [||]
  | false ->
    let s = mod_pos k n in
    let first = Array.unsafe_get arr 0 in
    let b = Array.make n first in
    let rec fill i =
      match i = n with
      | true  -> b
      | false ->
        let src = mod_pos (i - s) n in
        Array.unsafe_set b i (Array.unsafe_get arr src);
        fill (i + 1)
    in
    fill 0

let rotate_left (arr:output) (k:int) : output =
  let n = Array.length arr in
  rotate_right arr (n - (mod_pos k n))

let take_array (arr:output) (m:int) : output =
  let n = Array.length arr in
  match n = 0 with
  | true  -> [||]
  | false ->
    let m0  = clamp_nonneg m in
    let len = match Stdlib.compare m0 n with -1 -> m0 | 0 -> m0 | _ -> n in
    let first = Array.unsafe_get arr 0 in
    let b = Array.make len first in
    let rec loop i =
      match i = len with
      | true  -> b
      | false -> Array.unsafe_set b i (Array.unsafe_get arr i); loop (i + 1)
    in
    loop 0

(* build 10×(levels) matrices *)
let rounds_matrix_right (base:output) (rounds:int) (step:int) : output array =
  let rec build r acc =
    match r = rounds with
    | true  -> Array.of_list (List.rev acc)
    | false ->
      let arr = rotate_right base (r * step) in
      build (r + 1) (arr :: acc)
  in
  build 0 []

let rounds_matrix_left (base:output) (rounds:int) (step:int) : output array =
  let rec build r acc =
    match r = rounds with
    | true  -> Array.of_list (List.rev acc)
    | false ->
      let arr = rotate_left base (r * step) in
      build (r + 1) (arr :: acc)
  in
  build 0 []

(* print: rows = rounds, cols = levels (window over columns) *)
let print_rounds_rows_levels_cols_window (name:string) (m:output array) (start:int) (count:int) =
  let rounds = Array.length m in
  let cols   = if rounds = 0 then 0 else Array.length m.(0) in
  let s      = if start < 0 then 0 else start in
  let c      = if count < 0 then 0 else count in
  let last   = let u = s + c in if u <= cols then u else cols in
  let cellw  = 2 in
  Printf.printf "%s (rounds=%d, levels [%d..%d)):\n" name rounds s last;
  let rec loop_r r =
    match r = rounds with
    | true  -> ()
    | false ->
      let row = m.(r) in
      let rec loop_c j =
        match j = last with
        | true  -> Printf.printf "\n"
        | false ->
          Printf.printf "%-*s " cellw row.(j);
          loop_c (j+1)
      in
      loop_c s; loop_r (r+1)
  in
  loop_r 0


  (* ===== Verification for a single output (round 0) ===== *)

let strmap_inc (k:string) (v:int) (m:int StrMap.t) : int StrMap.t =
  let old = StrMap.find_opt k m |> Option.value ~default:0 in
  StrMap.add k (old + v) m

let counts_of_output (out:output) : int StrMap.t =
  let n = Array.length out in
  match n = 0 with
  | true  -> StrMap.empty
  | false ->
    let rec loop i acc =
      match i = n with
      | true  -> acc
      | false ->
        let d = Array.unsafe_get out i in
        loop (i + 1) (strmap_inc d 1 acc)
    in
    loop 0 StrMap.empty

let verify_distribution (input:input) (out:output) (blocks:int) : unit =
  let total_stake = List.fold_left (fun acc (_, s) -> acc + s) 0 input in
  let counts = counts_of_output out in
  let sorted = List.sort (fun (a,_) (b,_) -> Stdlib.compare a b) input in
  Format.printf "@[<v 2>Distribution:@,%10s %10s %10s %10s@,"
    "Delegate" "Expected" "Actual" "Error";
  let rec print = function
    | [] -> Format.printf "@]@."
    | (d, s)::tl ->
      let expected =
        match Stdlib.compare total_stake 0 with
        | 0 -> 0.0
        | _ -> (float_of_int s) *. (float_of_int blocks) /. (float_of_int total_stake)
      in
      let actual = StrMap.find_opt d counts |> Option.value ~default:0 in
      let error = (float_of_int actual) -. expected in
      Format.printf "%10s %10.2f %10d %+10.2f@," d expected actual error;
      print tl
  in
  print sorted
