(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module type S = sig
  module Coeff : Scalar.S
  type t
  exception Dimension_error of string
  val of_list_list : Coeff.t list list -> t
  val to_list_list : t -> Coeff.t list list
  val of_array_array : Coeff.t array array -> t
  val to_array_array : t -> Coeff.t array array
  val zeros : int -> int -> t
  val eye : int -> t
  val kron : int -> int -> int -> t
  val kron_sym : int -> int -> int -> t
  val block : t array array -> t
  val lift_block : t -> int -> int -> int -> int -> t
  val transpose : t -> t
  val minus : t -> t
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val power : t -> int -> t
  val nb_lines : t -> int
  val nb_cols : t -> int
  val is_symmetric : t -> bool
  val remove_0_row_cols : t -> t
  val gauss_split : t -> int * t * t
  val ( ~: ) : t -> t
  val ( ~- ) : t -> t
  val ( *. ) : Coeff.t -> t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ** ) : t -> int -> t
  val pp : Format.formatter -> t -> unit
end

module Make (ET : Scalar.S) : S with module Coeff = ET = struct
  module Coeff = ET

  type t = { line : int; col : int; content : ET.t array array }

  exception Dimension_error of string

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
                  if c = col then i + 1 else
                    let s = Format.sprintf "%dth line of %d elements in \
                                            matrix with %d columns" i c col in
                    raise (Dimension_error ("of_list_list (" ^ s ^ ")")))
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

  (** Function largely inspired from Pietro Abate tutorial on http://mancoosi.org/~abate/ocaml-format-module *)
  let pp fmt m =
    let sep i j =
      if j < m.col - 1 then "," else if i < m.line - 1 then ";" else "" in
    let pp_row widths fmt i row =
      if i = 0 then
        Array.iteri (fun j width ->
          Format.pp_set_tab fmt ();
          (* The string cell is filled with the string contained in first row *)
          Format.fprintf Format.str_formatter "%a%s" ET.pp row.(j) (sep i j);
          let str = Format.flush_str_formatter () in
          let strcell = String.make (width + 1 - String.length str) ' ' in
          Format.fprintf fmt "%s%s" str strcell
        ) widths
      else
        Array.iteri (fun j cell ->
          Format.pp_print_tab fmt ();
          Format.fprintf fmt "%a%s" ET.pp cell (sep i j)
        ) row
    in
    let compute_widths table =
      (* we build with the largest length of each column of the
       * table and header *)
      let widths = Array.make m.col 0 in
      Array.iteri (fun i row ->
        Array.iteri (fun j cell ->
          Format.fprintf Format.str_formatter "%a%s" ET.pp cell (sep i j);
          let str = Format.flush_str_formatter () in
          widths.(j) <- max (String.length str) widths.(j)
        ) row
      ) table;
      widths
    in
    let widths = compute_widths m.content in
    Format.pp_print_string fmt "[";
    (* open the table box *)
    Format.pp_open_tbox fmt ();
    (* print the table *)
    Array.iteri (pp_row widths fmt) m.content;
    (* close the box *)
    Format.pp_close_tbox fmt ();
    Format.pp_print_string fmt "]"

  (* Old version *)
  (* Format.fprintf fmt "[@[%a@]]" *)
  (*   (Utils.fprintf_array ~sep:";@ " *)
  (*      (fun fmt -> Format.fprintf fmt "@[%a@]" *)
  (*        (Utils.fprintf_array ~sep:",@ " ET.pp))) m.content *)

  let zeros l c =
    { line = l; col = c; content = Array.make_matrix l c ET.zero }

  let eye k =
    let c = Array.make_matrix k k ET.zero in
    for i = 0 to k - 1 do c.(i).(i) <- ET.one done;
    { line = k; col = k; content = c }

  let kron dim i j =
    let c = Array.make_matrix dim dim ET.zero in
    c.(i).(j) <- ET.one;
    { line = dim; col = dim; content = c }

  let kron_sym dim i j =
    let c = Array.make_matrix dim dim ET.zero in
    c.(i).(j) <- ET.one;
    c.(j).(i) <- ET.one;
    { line = dim; col = dim; content = c }

  let block a =
    if Array.length a <= 0 || Array.length a.(0) <= 0 then
      raise (Dimension_error "block (empty block)")
    else begin
      (* All lines must have the same number of blocks. *)
      let col_blocks = Array.length a.(0) in
      for i = 1 to Array.length a - 1 do
        if Array.length a.(i) <> col_blocks then
          let s = Format.sprintf "%dth line of %d elements in block matrix \
                                  with %d columns"
                                 i (Array.length a.(i)) col_blocks in
          raise (Dimension_error ("block (" ^ s ^ ")"))
      done;
      (* All matrices in each line must have the same number of rows. *)
      for i = 0 to Array.length a - 1 do
        let rows = a.(i).(0).line in
        for j = 1 to Array.length a.(i) - 1 do
          if a.(i).(j).line <> rows then
            let s = Format.sprintf "block %d, %d has %d lines whereas block \
                                    %d, 0 has %d" i j a.(i).(j).line i rows in
            raise (Dimension_error ("block (" ^ s ^ ")"))
        done
      done;
      (* All matrices in each column must have the same number of columns. *)
      for j = 0 to Array.length a.(0) -1 do
        let cols = a.(0).(j).col in
        for i = 1 to Array.length a -1  do
          if a.(i).(j).col <> cols then
            let s = Format.sprintf "block %d, %d has %d columns whereas block \
                                    0, %d has %d" i j a.(i).(j).col j cols in
            raise (Dimension_error ("block (" ^ s ^ ")"))
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
      res
    with Invalid_argument _ ->
      let s = Format.sprintf "lifting mat of size %d x %d at %d, %d into a new \
                              mat of size %d x %d"
                             mat.line mat.col pos_l pos_c n m in
      raise (Dimension_error ("lift_block (" ^ s ^ ")"))

  let transpose m =
    let c = Array.make_matrix m.col m.line ET.zero in
    for i = 0 to m.col - 1 do
      for j = 0 to m.line - 1 do
        c.(i).(j) <- m.content.(j).(i)
      done
    done;
    { line = m.col; col = m.line; content = c }

  let minus m = { m with content = Array.map (Array.map ET.neg) m.content }

  let mult_scalar s m =
    { m with content = Array.map (Array.map (ET.mult s)) m.content }

  let map2_matrix op sop m1 m2 =
    if m1.line <> m2.line || m1.col <> m2.col then
      let s = Format.sprintf "operands of different sizes (%d x %d and %d x %d)"
                             m1.line m1.col m2.line m2.col in
      raise (Dimension_error (sop ^ " (" ^ s ^ ")"))
    else
      let c = Array.make_matrix m1.line m1.col ET.zero in
      for i = 0 to m1.line - 1 do
        for j = 0 to m1.col - 1 do
          c.(i).(j) <- op m1.content.(i).(j) m2.content.(i).(j)
        done
      done;
      { line = m1.line; col = m1.col; content = c }

  let add = map2_matrix ET.add "add"

  let sub = map2_matrix ET.sub "sub"

  let mult m1 m2 =
    if m1.col <> m2.line then
      let s = Format.sprintf "first operand has %d columns and second has %d \
                              lines" m1.col m2.line in
      raise (Dimension_error ("mult (" ^ s ^ ")"))
    else
      let c = Array.make_matrix m1.line m2.col ET.zero in
      for i = 0 to m1.line - 1 do
        for j = 0 to m2.col - 1 do
          for k = 0 to m1.col - 1 do
            c.(i).(j) <- ET.(c.(i).(j)
                             + m1.content.(i).(k) * m2.content.(k).(j))
          done
        done
      done;
      { line = m1.line; col = m2.col; content = c }

  let rec power m n =
    if n <= 0 then eye m.line
    else
      let m' = power m (n / 2) in
      if n mod 2 = 0 then mult m' m'
      else mult m (mult m' m')

  let nb_lines m = m.line
  let nb_cols m = m.col

  let is_symmetric m =
    if m.line <> m.col then false else
      try
        for i = 1 to m.line - 1 do
          for j = 0 to i - 1 do
            if ET.(m.content.(i).(j) <> m.content.(j).(i)) then
              raise Exit
          done
        done;
        true
      with Exit -> false

  let remove_0_row_cols m =
    let empty_r = Array.make m.line true in
    let empty_c = Array.make m.col true in
    for i = 0 to m.line -1 do
      for j = 0 to m.col - 1 do
        if ET.(m.content.(i).(j) <> zero) then begin
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
        m.content.(r2).(i) <- ET.(m.content.(r2).(i)
                                  + (c * m.content.(r1).(i)))
      done in

    let find_non_nul col line m =
      let rec aux cpt =
        if cpt >= m.line then
          (* we reached the end of the matrix without finding a non null value for
             column i *)
          -1
        else if ET.(m.content.(cpt).(col) <> zero) then
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
          if ET.(m.content.(j).(col) <> zero) then
            let coeff = ET.(~- (m.content.(j).(col))
                            / m.content.(!current_line).(col)) in
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
           if List.exists ET.(( <> ) zero) row then
             (* We keep the line *)
             rank, (row, row2)::reduced
           else (* We remove it, the rank decreases *)
             rank - 1, reduced)
        paired (min m.line m.col, [])  in
    let m_list, base_change_list = List.split filtered in
    let m = of_list_list m_list in
    let base_change = of_list_list base_change_list in
    rank, m, base_change

  let ( ~: ) = transpose
  let ( ~- ) = minus
  let ( *. ) = mult_scalar
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mult
  let ( ** ) = power
end

module Q = Make (Scalar.Q)

module Float = Make (Scalar.Float)
