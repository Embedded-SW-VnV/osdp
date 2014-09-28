module IntMap =  Map.Make (struct type t = int let compare = compare end)

let fprintf_list ~sep f fmt l =
  let rec aux fmt = function
    | []   -> ()
    | [e]  -> f fmt e
    | x::r -> Format.fprintf fmt "%a%(%)%a" f x sep aux r in
  aux fmt l

let fprintf_array ~sep f fmt a =
  if Array.length a >= 1 then begin
    Format.fprintf fmt "%a" f a.(0);
    for i = 1 to Array.length a - 1 do
      Format.fprintf fmt "%(%)%a" sep f a.(i)
    done
  end

let profile c =
  let be = Unix.gettimeofday () in
  let r = Lazy.force c in
  let en = Unix.gettimeofday () in
  let elapsed_time = en -. be in
  r, elapsed_time

let num_of_float f = match classify_float f with
  | FP_normal ->
     let f64 = Int64.bits_of_float f in
     let exp =
       let bits = (Int64.to_int (Int64.shift_right f64 52)) land 2047 in
       Num.num_of_int (bits - 1023 - 52) in
     let mant =
       let l = (Int64.to_int f64) land 67108863 in
       let h = ((Int64.to_int (Int64.shift_right f64 26)) land 67108863)
               lor 67108864 in
       Num.add_num
         (Num.mult_num (Num.num_of_int h) (Num.num_of_int 67108864))
         (Num.num_of_int l) in
     let abs = Num.mult_num mant (Num.power_num (Num.num_of_int 2) exp) in
     if f > 0. then abs else Num.minus_num abs
  | FP_subnormal ->
     let f64 = Int64.bits_of_float f in
     let exp = Num.num_of_int ((-1023) - 52) in
     let mant = 
       let l = (Int64.to_int f64) land 67108863 in
       let h = ((Int64.to_int (Int64.shift_right f64 26)) land 67108863) in
       Num.add_num
         (Num.mult_num (Num.num_of_int h) (Num.num_of_int 67108864))
         (Num.num_of_int l) in
     let abs = Num.mult_num mant (Num.power_num (Num.num_of_int 2) exp) in
     if f > 0. then abs else Num.minus_num abs
  | FP_zero -> Num.num_of_int 0
  | FP_infinite
  | FP_nan -> assert false
		    
let merge_sorted_lists cmp f l1 l2 =
  let rec aux l1 l2 = 
    match l1, l2 with 
    | [], l2 -> l2
    | l1, [] -> l1
    | h1::t1, h2::t2 ->
       let comp = cmp h1 h2 in
       if comp < 0 then 
	h1 :: (aux t1 l2)
       else if comp = 0 then
	 match f h1 h2 with
	 | Some r -> r::(aux t1 t2)
	 | None -> aux t1 t2
       else h2 :: (aux l1 t2) in
  aux l1 l2
