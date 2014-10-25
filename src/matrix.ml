module type S = sig
  module Coeff : Scalar.S
  type t
  exception Dimension_error
  val of_list_list : Coeff.t list list -> t
  val to_list_list : t -> Coeff.t list list
  val of_array_array : Coeff.t array array -> t
  val to_array_array : t -> Coeff.t array array
  val zeros : int -> int -> t
  val eye : int -> t
  val kronecker_sym: int -> int -> int -> t
  val block : t array array -> t
  val lift_block : t -> int -> int -> int -> int -> t
  val transpose : t -> t
  val minus : t -> t
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  module Infix : sig
    val ( ~:. ) : t -> t
    val ( ~: ) : t -> t
    val ( */: ) : Coeff.t -> t -> t
    val ( +: ) : t -> t -> t
    val ( -: ) : t -> t -> t
    val ( *: ) : t -> t -> t
  end
  val nb_lines : t -> int
  val nb_cols : t -> int
  val remove_0_row_cols : t -> t
  val gauss_split : t -> int * t * t
  val pp : Format.formatter -> t -> unit
end

module Make (ET : Scalar.S) : S with module Coeff = ET = struct
  module Coeff = ET

  type t = { line : int; col : int; content : Coeff.t array array }

  exception Dimension_error

  let of_list_list = function
    | [] -> { line = 0; col = 0; content = [||] }
    | l::_ as ll ->
       let line = List.length ll in
       let col = List.length l in
       let content = Array.make_matrix line col ET.zero in
       let _ = List.fold_left
                 (fun i l ->
                  let c = List.fold_left
                            (fun j e -> content.(i).(j) <- e; j + 1)
                            0
                            l in
                  if c <> col then raise Dimension_error;
                  i + 1)
                 0
                 ll in
       { line = line;
         col = col;
         content = content }
          
  let to_list_list m =
    Array.fold_right
      (fun e l -> Array.to_list e :: l)
      m.content
      []
      
  let of_array_array a =
    { line = Array.length a;
      col = Array.length a.(0);
      content = Array.map Array.copy a }
      
  let to_array_array m = Array.map Array.copy m.content

  let pp fmt m =
    Format.fprintf fmt "[@[%a@]]"
                   (Utils.fprintf_array ~sep:";@ "
                      (fun fmt -> Format.fprintf fmt "@[%a@]"
                         (Utils.fprintf_array ~sep:",@ " ET.pp))) m.content
    
  let zeros l c =
    { line = l; col = c; content = Array.make_matrix l c ET.zero }

  let eye k =
    let c = Array.make_matrix k k ET.zero in
    for i = 0 to k - 1 do c.(i).(i) <- ET.one done;
    { line = k; col = k; content = c }

  let kronecker_sym dim i j =
    let c = Array.make_matrix dim dim ET.zero in
    c.(i).(j) <- ET.one;
    c.(j).(i) <- ET.one;
    { line = dim; col = dim; content = c }

  let block a =
    if Array.length a <= 0 || Array.length a.(0) <= 0 then
      raise Dimension_error
    else begin
      (* All lines must have the same number of blocks. *)
      let col_blocks = Array.length a.(0) in
      for i = 1 to Array.length a - 1 do
        if Array.length a.(i) <> col_blocks then
          raise Dimension_error
      done;
      (* All matrices in each line must have the same number of rows. *)
      for i = 0 to Array.length a - 1 do
        let rows = a.(i).(0).line in
        for j = 1 to Array.length a.(i) - 1 do
          if a.(i).(j).line <> rows then
            raise Dimension_error
        done
      done;
      (* All matrices in each column must have the same number of columns. *)
      for j = 0 to Array.length a.(0) -1 do
        let cols = a.(0).(j).col in
        for i = 1 to Array.length a -1  do
          if a.(i).(j).col <> cols then
            raise Dimension_error
        done
      done;
      let total_rows = Array.fold_left (fun c m -> c + m.(0).line) 0 a in
      let total_cols = Array.fold_left (fun c m -> c + m.col) 0 a.(0) in
      let c = Array.make_matrix total_rows total_cols ET.zero in
      let cur_r = ref 0 in
      for i = 0 to Array.length a - 1 do
        let cur_c = ref 0 in
        for j = 0 to Array.length a.(0) - 1 do
          for k = 0 to a.(i).(j).line - 1 do
            for l = 0 to a.(i).(j).col - 1 do
              c.(!cur_r + k).(!cur_c + l) <- a.(i).(j).content.(k).(l)
            done
          done;
          cur_c := !cur_c + a.(i).(j).col
        done;
        cur_r := !cur_r + a.(i).(0).line
      done;
      { line = total_rows; col = total_cols; content = c }
    end      

  let lift_block mat n m pos_l pos_c =
    try
      let res = zeros n m in
      for i = 0 to mat.line - 1 do
        for j = 0 to mat.col - 1 do
          res.content.(pos_l + i).(pos_c + j) <- mat.content.(i).(j) 
        done;
      done;
      (* Format.eprintf "Lifted mat: %a@." pp res; *)
      res
    with Invalid_argument _ ->
      Format.eprintf "lifting mat of size %i x %i to a new mat of size %i x %i, \
                      the initial mat is located at indices %i, %i:@ %a@."
                     mat.line mat.col
                     n m pos_l pos_c pp mat;
      raise Dimension_error

  let transpose m =
    let c = Array.make_matrix m.col m.line ET.zero in
    for i = 0 to m.col - 1 do
      for j = 0 to m.line - 1 do
        c.(i).(j) <- m.content.(j).(i)
      done
    done;
    { line = m.col; col = m.line; content = c }

  let minus m =
    { m with content = Array.map
                         (Array.map (fun x -> ET.sub ET.zero x))
                         m.content }

  let mult_scalar s m =
    { m with content = Array.map (Array.map (ET.mult s)) m.content }

  let map2_matrix op m1 m2 =
    if m1.line <> m2.line || m1.col <> m2.col then
      raise Dimension_error
    else
      let c = Array.make_matrix m1.line m1.col ET.zero in
      for i = 0 to m1.line - 1 do
        for j = 0 to m1.col - 1 do
          c.(i).(j) <- op m1.content.(i).(j) m2.content.(i).(j)
        done
      done;
      { line = m1.line; col = m1.col; content = c }

  let add = map2_matrix ET.add

  let sub = map2_matrix ET.sub

  let mult m1 m2 =
    if m1.col <> m2.line then
      raise Dimension_error;
    let c = Array.make_matrix m1.line m2.col ET.zero in
    for i = 0 to m1.line - 1 do
      for j = 0 to m2.col - 1 do
        for k = 0 to m1.col - 1 do
          c.(i).(j) <- ET.add c.(i).(j)
                         (ET.mult m1.content.(i).(k) m2.content.(k).(j))
        done
      done
    done;
  { line = m1.line; col = m2.col; content = c }

  module Infix = struct
  let ( ~:. ) = transpose
  let ( ~: ) = minus
  let ( */: ) = mult_scalar
  let ( +: ) = add
  let ( -: ) = sub
  let ( *: ) = mult
  end

  let nb_lines m = m.line
  let nb_cols m = m.col

  let remove_0_row_cols m =
    let empty_r = Array.make m.line true in
    let empty_c = Array.make m.col true in
    for i = 0 to m.line -1 do
      for j = 0 to m.col - 1 do
        if not (ET.is_zero m.content.(i).(j)) then begin
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
    let m' = Array.make_matrix !nb_r !nb_c ET.zero in
    for i = 0 to m.line - 1 do
      for j = 0 to m.col - 1 do
        if move_r.(i) >= 0 && move_c.(j) >= 0 then
          m'.(move_r.(i)).(move_c.(j)) <- m.content.(i).(j)
      done
    done;
    { line = !nb_r; col = !nb_c; content = m' }
    
  let gauss_split m = 
    (* Local functions for gauss algorithm: careful perform local updates *)

    (* switch lines i and j in matrix m.
       Inplace modif *)
    let switch_lines m i j =
      if i = j then
        () 
      else
        let tmp = m.content.(i) in
        m.content.(i) <- m.content.(j);
        m.content.(j) <- tmp in

    (* rewrite_lin m c i j : replace m.(j) by m.(j) + c * m.(i).
       Inplace modif *)
    let rewrite_lin m c r1 r2 =
      for i = 0 to m.col - 1 do 
        m.content.(r2).(i) <- ET.add
                                m.content.(r2).(i)
                                (ET.mult c m.content.(r1).(i)) 
      done in

    let find_non_nul col line m =
      let rec aux cpt =
        if cpt >= m.line then 
          (* we reached the end of the matrix without finding a non null value for
	     column i *)
          -1
        else if not (ET.is_zero m.content.(cpt).(col)) then
          cpt
        else
          aux (cpt+1) in
      aux line in

    (* first copy the matrix m *)
    let m =
      { line = m.line;
        col = m.col;
        content = Array.map Array.copy m.content } in
    (* We build a default base change of size l x l. *)
    let base_change = eye m.line in
    let current_line = ref 0 in
    for col = 0 to min (m.line - 1) (m.col - 1) do
      (* Find a line below or equal i with a non null value at idx current_col *)
      let pivot_line = find_non_nul col !current_line m in
      if pivot_line = -1 then
        () (* We stop the processing for idx i, current_line is not increased 
	      Question for Pierre or Assalé: should we keep the column or remove it ? *)
      else begin
        switch_lines m !current_line pivot_line;
        switch_lines base_change !current_line pivot_line;
        (* Make sure that we have 0 for idx i for all lines j > i *)
        for j = !current_line+1 to m.line -1 do
  	  if not (ET.is_zero m.content.(j).(col)) then
  	    let coeff = ET.sub ET.zero
                               (ET.div m.content.(j).(col)
                                       m.content.(!current_line).(col)) in
  	    rewrite_lin m coeff !current_line j;
  	    rewrite_lin base_change coeff !current_line j
        done;
        incr current_line
      end
    done;
    (* We clean the matrix for empty lines *)
    let m_list = to_list_list m in
    let base_change_list = to_list_list base_change in
    let paired = List.combine m_list base_change_list in  
    let rank, filtered =
      List.fold_right
        (fun (row, row2) (rank, reduced) -> 
           if List.exists (fun el -> not (ET.is_zero el)) row then
             (* We keep the line *)
             rank, (row, row2)::reduced
           else (* We remove it, the rank decreases *)
             rank - 1, reduced)
        paired (min m.line m.col, [])  in
    let m_list, base_change_list = List.split filtered in
    let m = of_list_list m_list in
    let base_change = of_list_list base_change_list in
    rank, m, base_change
end

module Q = Make (Scalar.Q) 

module Float = Make (Scalar.Float)
