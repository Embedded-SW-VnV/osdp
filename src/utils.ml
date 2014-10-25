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
