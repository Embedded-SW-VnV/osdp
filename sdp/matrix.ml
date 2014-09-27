module type ElemType = sig 
  type t
  val of_int: int -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mult: t -> t -> t
  val div: t -> t -> t
  val sign: t -> int
  val pp: Format.formatter -> t -> unit
  val to_float: t -> float
  val of_float: float -> t
  val compare: t -> t -> int
  val eq: t -> t -> bool
  val is_zero: t -> bool
  val to_string: t -> string
end

module type S = sig
  module Elem: ElemType
  type elt = Elem.t
  type t

  exception Matrix_dimension_error
    
  val matrix_of_list_list : elt list list -> t
  val matrix_to_list_list : t -> elt list list
    
  val matrix_of_array_array : elt array array -> t
  val matrix_to_array_array : t -> elt array array
    
  (* block A B C D = [A, B; C, D] *)
  val block_matrix : t -> t -> t -> t -> t
    
  val zeros_matrix : int -> int -> t
    
  val ident_matrix : int -> t
  val kronecker_sym_matrix: int -> int -> int -> t

  val ( ~:. ) : t -> t
  val transpose_matrix : t -> t
    
  (* uminus A = -A *)
  val ( ~: ) : t -> t
  val minus_matrix : t -> t
    
  val ( */: ) : elt -> t -> t
  val mult_scalar_matrix : elt -> t -> t

  val ( +: ) : t -> t -> t
  val add_matrix : t -> t -> t
    
  val ( -: ) : t -> t -> t
  val sub_matrix : t -> t -> t
    
  val ( *: ) : t -> t -> t
  val mult_matrix : t -> t -> t
    
(* returns the same matrix, without its rows and columns containing only 0 *)
  val matrix_remove_0_row_cols : t -> t

  val copy: t -> t
  val transpose: t -> t

  val pp_matrix : Format.formatter -> t -> unit
  val pp_elem : Format.formatter -> elt -> unit
  val nb_lines: t -> int
  val nb_cols: t -> int

  (** gauss_split m for a matrix m of size l x c returns (n, m1, m2) where n is
      the rank of the input matrix (n <= l), m1 its row space, ie. a matrix of
      size l' x c where l' <= l characterizing the same space as m but with no
      linear dependencies, and m2 a matrix of size l x l' mapping original
      dimensions in m to the ones of m1 *)
  val gauss_split: t -> int * t * t

  val lift_block: t -> int -> int -> int -> int -> t
end

module Make = functor (BT : ElemType) -> (
  struct 
    module Elem = BT
    type elt = Elem.t
    type t = { line : int; col : int; content : elt array array }

exception Matrix_dimension_error

let copy m =
  let m' = Array.make_matrix m.line m.col (BT.of_int 0) in
  for i = 0 to m.line -1  do
    for j = 0 to m.col - 1 do
      m'.(i).(j) <- m.content.(i).(j)
    done
  done;
  { line = m.line;
    col = m.col;
    content = m'    
  }

let transpose m =
  let m' = Array.make_matrix m.col m.line (BT.of_int 0) in
  for i = 0 to m.line - 1do
    for j = 0 to m.col - 1 do
      m'.(j).(i) <- m.content.(i).(j)
    done
  done;
   { line = m.col;
    col = m.line;
    content = m'    
  }

let matrix_of_list_list = function
  | [] -> { line = 0; col = 0; content = [||] }
  | l::_ as ll ->
    let line = List.length ll in
    let col = List.length l in
    let content = Array.make_matrix line col (BT.of_int 0) in
    let _ = List.fold_left
      (fun i l ->
        let c = List.fold_left
          (fun j e -> content.(i).(j) <- e; j + 1)
          0
          l in
        if c <> col then raise Matrix_dimension_error;
        i + 1)
      0
      ll in
    { line = line;
      col = col;
      content = content }

let matrix_to_list_list m =
  Array.fold_right
    (fun e l -> Array.to_list e :: l)
    m.content
    []

let matrix_of_array_array a =
  { line = Array.length a;
    col = Array.length a.(0);
    content = a }

let matrix_to_array_array m = m.content

let block_matrix a b c d =
  if a.line <> b.line || c.line <> d.line
    || a.col <> c.col || b.col <> d.col then
    raise Matrix_dimension_error;
  let co = Array.make_matrix (a.line + c.line) (a.col + b.col) (BT.of_int 0) in
  for i = 0 to a.line - 1 do
    for j = 0 to a.col - 1 do
      co.(i).(j) <- a.content.(i).(j)
    done;
    for j = 0 to b.col - 1 do
      co.(i).(a.col + j) <- b.content.(i).(j)
    done;
  done;
  for i = 0 to c.line - 1 do
    for j = 0 to c.col - 1 do
      co.(a.line + i).(j) <- c.content.(i).(j)
    done;
    for j = 0 to d.col - 1 do
      co.(a.line + i).(c.col + j) <- d.content.(i).(j)
    done;
  done;
  { line = a.line + c.line;
    col = a.col + b.col;
    content = co }

let zeros_matrix l c =
  { line = l; col = c; content = Array.make_matrix l c (BT.of_int 0) }

let ident_matrix k =
  let c = Array.make_matrix k k (BT.of_int 0) in
  for i = 0 to k - 1 do c.(i).(i) <- BT.of_int 1 done;
  { line = k; col = k; content = c }

let kronecker_sym_matrix dim i j =
  let mat = Array.make_matrix dim dim (BT.of_int 0) in
  mat.(i).(j) <- (BT.of_int 1);
  if i != j then mat.(j).(i) <- (BT.of_int 1);
  matrix_of_array_array mat

let transpose_matrix m =
  let c = Array.make_matrix m.col m.line (BT.of_int 0) in
  for i = 0 to m.col - 1 do
    for j = 0 to m.line - 1 do
      c.(i).(j) <- m.content.(j).(i)
    done
  done;
  { line = m.col; col = m.line; content = c }
let ( ~:. ) = transpose_matrix

let minus_matrix m =
  { m with content = Array.map
      (Array.map (fun x -> BT.mult (BT.of_int (-1)) x))
      m.content }
let ( ~: ) = minus_matrix

let mult_scalar_matrix s m =
  { m with content = Array.map (Array.map (BT.mult s)) m.content }
let ( */: ) = mult_scalar_matrix

let map2_matrix op m1 m2 =
  if m1.line >= m2.line then
    if m1.col >= m2.col then begin
      let m = Array.make_matrix m1.line m1.col (BT.of_int 0) in
      for i = 0 to m2.line - 1 do
        for j = 0 to m2.col - 1 do
          m.(i).(j) <- op m1.content.(i).(j) m2.content.(i).(j)
        done;
        for j = m2.col to m1.col - 1 do
          m.(i).(j) <- m1.content.(i).(j)
        done
      done;
      for i = m2.line to m1.line -1 do
        for j = 0 to m1.col - 1 do
          m.(i).(j) <- m1.content.(i).(j)
        done
      done;
      { line = m1.line; col = m1.col; content = m }
    end else begin
      let m = Array.make_matrix m1.line m2.col (BT.of_int 0) in
      for i = 0 to m2.line - 1 do
        for j = 0 to m1.col - 1 do
          m.(i).(j) <- op m1.content.(i).(j) m2.content.(i).(j)
        done;
        for j = m1.col to m2.col - 1 do
          m.(i).(j) <- m2.content.(i).(j)
        done
      done;
      for i = m2.line to m1.line -1 do
        for j = 0 to m1.col - 1 do
          m.(i).(j) <- m1.content.(i).(j)
        done
      done;
      { line = m1.line; col = m2.col; content = m }
    end
  else
    if m1.col >= m2.col then begin
      let m = Array.make_matrix m2.line m1.col (BT.of_int 0) in
      for i = 0 to m1.line - 1 do
        for j = 0 to m2.col - 1 do
          m.(i).(j) <- op m1.content.(i).(j) m2.content.(i).(j)
        done;
        for j = m2.col to m1.col - 1 do
          m.(i).(j) <- m1.content.(i).(j)
        done
      done;
      for i = m1.line to m2.line -1 do
        for j = 0 to m2.col - 1 do
          m.(i).(j) <- m2.content.(i).(j)
        done
      done;
      { line = m2.line; col = m1.col; content = m }
    end else begin
      let m = Array.make_matrix m2.line m2.col (BT.of_int 0) in
      for i = 0 to m1.line - 1 do
        for j = 0 to m1.col - 1 do
          m.(i).(j) <- op m1.content.(i).(j) m2.content.(i).(j)
        done;
        for j = m1.col to m2.col - 1 do
          m.(i).(j) <- m2.content.(i).(j)
        done
      done;
      for i = m1.line to m2.line -1 do
        for j = 0 to m2.col - 1 do
          m.(i).(j) <- m2.content.(i).(j)
        done
      done;
      { line = m2.line; col = m2.col; content = m }
    end

let add_matrix = map2_matrix BT.add
let ( +: ) = add_matrix

let sub_matrix = map2_matrix (fun x y -> BT.add x (BT.mult (BT.of_int (-1)) y))
let ( -: ) = sub_matrix

let mult_matrix m1 m2 =
  if m1.col <> m2.line then
    raise Matrix_dimension_error;
  let c = Array.make_matrix m1.line m2.col (BT.of_int 0) in
  for i = 0 to m1.line - 1 do
    for j = 0 to m2.col - 1 do
      for k = 0 to m1.col - 1 do
        c.(i).(j) <- BT.add c.(i).(j)
          (BT.mult m1.content.(i).(k) m2.content.(k).(j))
      done
    done
  done;
  { line = m1.line; col = m2.col; content = c }
let ( *: ) = mult_matrix

let matrix_remove_0_row_cols m =
  let empty_r = Array.make m.line true in
  let empty_c = Array.make m.col true in
  for i = 0 to m.line -1 do
    for j = 0 to m.col - 1 do
      if BT.sign m.content.(i).(j) <> 0 then begin
        empty_r.(i) <- false;
        empty_c.(j) <- false;
      end
    done
  done;
  let move_r = Array.make m.line (-1) in
  let nb_r = ref 0 in
  for i = 0 to m.line - 1 do
    if not empty_r.(i) then begin
      move_r.(i) <- !nb_r;
      incr nb_r
    end
  done;
  let move_c = Array.make m.col (-1) in
  let nb_c = ref 0 in
  for i = 0 to m.col - 1 do
    if not empty_c.(i) then begin
      move_c.(i) <- !nb_c;
      incr nb_c
    end
  done;
  let m' = Array.make_matrix !nb_r !nb_c (BT.of_int 0) in
  for i = 0 to m.line - 1 do
    for j = 0 to m.col - 1 do
      if move_r.(i) >= 0 && move_c.(j) >= 0 then
        m'.(move_r.(i)).(move_c.(j)) <- m.content.(i).(j)
    done
  done;
  { line = !nb_r; col = !nb_c; content = m' }
    
let pp_matrix fmt m =
  Format.fprintf fmt "[%a]" (Utils.fprintf_list ~sep:"; " (Utils.fprintf_list ~sep:", " BT.pp)) (matrix_to_list_list m)
    
let pp_elem = BT.pp
let nb_lines m = m.line
let nb_cols m = m.col


(* Local functions for gauss algorithm: careful perform local updates *)
(** switch lines i and j in matrix m. Inplace modif *)
let switch_lines m i j =
  if i = j then () 
  else
  let tmp = m.content.(i) in
  m.content.(i) <- m.content.(j);
  m.content.(j) <- tmp

(** rewrite_lin m c i j : replace m.(j) by m.(j) + c * m.(i). Inplace modif *)
let rewrite_lin m c r1 r2 =
  for i = 0 to m.col - 1 do 
    m.content.(r2).(i) <- BT.add m.content.(r2).(i) (BT.mult c m.content.(r1).(i)) 
  done

let find_non_nul col line m =
  let rec aux cpt =
    if cpt >= m.line then 
      (* we reached the end of the matrix without finding a non null value for
	 column i *)
      -1 
    else
      if BT.sign m.content.(cpt).(col) != 0 then cpt else aux (cpt+1)
  in
  aux line

let gauss_split m = 
  (* We build a default base changeof size l x l *)
  let base_change = ident_matrix m.line in
  let m = copy m in
  let current_line = ref 0 in
  for col = 0 to min (m.line - 1) (m.col - 1) do
    (* Find a line below or equal i with a non null value at idx current_col *)
    let pivot_line = find_non_nul col !current_line m in
    if pivot_line = -1 then
      () (* We stop the processing for idx i, current_line is not increased 
	    Question for Pierre or Assalé: should we keep the column or remove it ?
	 *)
    else begin
      switch_lines m !current_line pivot_line;
      switch_lines base_change !current_line pivot_line;
      (* Make sure that we have 0 for idx i for all lines j > i *)
      for j = !current_line+1 to m.line -1 do
  	if BT.sign m.content.(j).(col) != 0 then
  	  let coeff = BT.mult (BT.of_int (-1)) (BT.div m.content.(j).(col)  m.content.(!current_line).(col)) in
  	  rewrite_lin m coeff !current_line j;
  	  rewrite_lin base_change coeff !current_line j
      done;
      incr current_line
    end
  done;
  (* We clean the matrix for empty lines *)
  let m_list = matrix_to_list_list m in
  let base_change_list = matrix_to_list_list base_change in
  let paired = List.map2 (fun x y -> x, y) m_list base_change_list in  
  let rank, filtered = List.fold_right (fun (row, row2) (rank, reduced) -> 
    if List.exists (fun el -> BT.sign el != 0) row then
      (* We keep the line *)
      rank, (row, row2)::reduced
    else (* We remove it, the rank decreases *)
      rank - 1, reduced
  ) paired (min m.line m.col, [])  in
  let m_list, base_change_list = List.split filtered in
  let m = matrix_of_list_list m_list in
  let base_change = matrix_of_list_list base_change_list in
  rank, m, base_change

let lift_block mat n m pos_l pos_c =
  (* Format.eprintf "lifting mat of size %i x %i to a new mat of size %i x %i, the initial mat is located at indices %i,%i:@.%a@."  *)
  (*     mat.line mat.col *)
  (*   n m pos_l pos_c pp_matrix mat; *)
  try
  let res = zeros_matrix n m in
  for i = 0 to nb_lines mat - 1 do
    for j = 0 to nb_cols mat - 1 do
      res.content.(pos_l + i).(pos_c + j) <- mat.content.(i).(j) 
    done;
  done;
  (* Format.eprintf "Lifted mat: %a@." pp_matrix res; *)
  res
  with Invalid_argument id -> (
    Format.eprintf "lifting mat of size %i x %i to a new mat of size %i x %i, the initial mat is located at indices %i,%i:@.%a@."
      mat.line mat.col
      n m pos_l pos_c pp_matrix mat;
    raise (Failure (id ^ " mat_la"))
  )

  

end: S with type Elem.t = BT.t)



module Num_mat = Make(struct 
  type t = Num.num 
  let of_int = Num.num_of_int 
  let add = Num.add_num
  let sub = Num.sub_num
  let mult = Num.mult_num
  let div = Num.div_num
  let sign = Num.sign_num
  let to_float = Num.float_of_num
  let of_float = Utils.num_of_float
  let pp fmt num = Format.fprintf fmt "%s" (Num.string_of_num num) 
  let compare = Num.compare_num
  let eq = Num.eq_num
  let is_zero x = eq x (of_int 0)
  let to_string = Utils.string_of_num

end)

module Float = Make(struct
  type t = float 
  let of_int = float_of_int
  let add = (+.)
  let sub = (-.)
  let mult = ( *. )
  let div = (/.)
  let sign x = if x < 0. then -1 else if x = 0. then 0 else 1
  let to_float x = x
  let of_float x = x
  let pp = Format.pp_print_float
  let compare = compare
  let eq a b = a = b
  let is_zero x = eq x (of_int 0)
  let to_string = Utils.string_of_float
end)

(* Local Variables: *)
(* compile-command:"make -C ../.." *)
(* End: *)
