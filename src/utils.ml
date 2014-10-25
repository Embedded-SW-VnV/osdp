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

let fprintf_matrix ~begl ~endl ~sepl ~sepc f =
  let print_line fmt l =
    Format.fprintf fmt "%(%)%a%(%)" begl (fprintf_array ~sep:sepc f) l endl in
  fprintf_array ~sep:sepl print_line

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
