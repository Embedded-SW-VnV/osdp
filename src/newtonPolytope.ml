(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2016  P. Roux and P.L. Garoche
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

module Var = struct
  type t = string
  let print = Format.pp_print_string
  let compare = String.compare
  let is_int _ = false
end

(*
module Float = struct
  type t = float
  let add = ( +. )
  let mult = ( *. )
  let compare = Pervasives.compare
  let equal = ( = )
  let zero = 0.
  let one = 1.
  let m_one = -1.
  let is_zero n = n = zero
  let to_string = string_of_float
  let print fmt t = Format.fprintf fmt "%g" t
  let is_int _ = false
  let div = ( /. )
  let sub = ( -. )
  let is_one v = v = 1.
  let is_m_one v = v = -1.
  let sign x = if x > 0. then 1 else if x < 0. then -1 else 0
  let min = min
  let abs = abs_float
  let minus = ( ~-. )
end
*)

module Rat = struct
  type t = Q.t
  let add = Q.add
  let mult = Q.mul
  let compare = Q.compare
  let equal = Q.equal
  let zero = Q.zero
  let one = Q.one
  let m_one = Q.minus_one
  let is_zero n = equal n zero
  let to_string = Q.to_string
  let print = Q.pp_print
  let is_int _ = false
  let div = Q.div
  let sub = Q.sub
  let is_one v = equal v one
  let is_m_one v = equal v m_one
  let sign = Q.sign
  let min = Q.min
  let abs = Q.abs
  let minus = Q.neg
end

module Ex = struct
  type t = unit
  let empty = ()
  let union _ _ = ()
  let print _ _ = ()
end

module Sim = OcplibSimplex.Basic.Make (Var) (Rat) (Ex)

let find_separating_plane p si =
  let p_of_list l =
    List.mapi (fun n c -> "x" ^ string_of_int n, Q.of_float c) l in
  let nb = ref 0 in
  let add_cstr b large sim c =
    let large = if large then Rat.m_one else Rat.zero in
    match List.filter (fun (_, c) -> not Rat.(equal c zero)) c with
    | [] -> sim
    | [v, c] ->
       let l, u =
         if Rat.sign c > 0 then None, Some (Rat.div b c, large)
         else Some (Rat.div b c, Rat.minus large), None in
       Sim.Assert.var sim v l () u ()
    | _ ->
       let s = "s" ^ string_of_int !nb in
       incr nb;
       let c = Sim.Core.P.from_list c in
       Sim.Assert.poly sim c s None () (Some (b, large)) () in
  let n =
    let f n l = max n (List.length l) in
    List.fold_left f 0 p in
  let center =
    let rec sub x y = match x, y with
      | _, [] -> List.map float_of_int x
      | [], _ -> sub [0] y
      | hx :: tx, hy :: ty -> float_of_int (hx - hy) :: sub tx ty in
    let o = match p with [] -> [] | h :: _ -> h in
    fun x ->
    let a = Array.of_list (sub x o) in
    Array.append a (Array.make (n - Array.length a) 0.) in
  let center' x = Array.to_list (center x) in
  let z = si |> List.map (( * ) 2) |> center' |> p_of_list in
  let m_z = List.map (fun (v, c) -> v, Rat.minus c) z in
  let cstrs = p |> List.map center' |> List.map p_of_list in
  let sim = Sim.Core.empty ~is_int:false ~check_invs:false ~debug:0 in
  let sim = add_cstr Rat.m_one false sim m_z in
  let sim = List.fold_left (add_cstr Rat.one true) sim cstrs in
  let sim, opt = Sim.Solve.maximize sim (Sim.Core.P.from_list z) in
  match Sim.Result.get opt sim with
  | (Sim.Core.Unknown | Sim.Core.Unsat _) -> None
  | (Sim.Core.Sat sol | Sim.Core.Unbounded sol | Sim.Core.Max (_, sol)) ->
     let a = Array.make n Rat.zero in
     let { Sim.Core.main_vars; _ } = Lazy.force sol in
     List.iter
       (fun (v, c) ->
         let i = int_of_string (String.sub v 1 (String.length v - 1)) in
         a.(i) <- c)
       main_vars;
     Some a

let filter_newton_polytope_ocplib_simplex s p =
  (* keep monomials s_i of s such that 2 s_i is in p *)
  let skeep, sfilter =
    let rec inter x y = match x, y with
      | [], _ | _, [] -> [], x
      | hx :: tx, hy :: ty ->
         let c = compare (List.map (( * ) 2) hx) hy in
         if c = 0 then let k, f = inter tx ty in hx :: k, f
         else if c < 0 then let k, f = inter tx y in k, hx :: f
         else (* c > 0 *) let k, f = inter x ty in k, f in
    let ls = List.sort Monomial.compare s in
    let lp = List.sort Monomial.compare p in
    inter (List.map Monomial.to_list ls) (List.map Monomial.to_list lp) in
  (* look for separating hyperplane to rule out monomials not in the
     Newton polynomial *)
  let s =
    let center =
      let rec sub x y = match x, y with
        | _, [] -> List.map float_of_int x
        | [], _ -> sub [0] y
        | hx :: tx, hy :: ty -> float_of_int (hx - hy) :: sub tx ty in
      let o = match p with [] -> [] | h :: _ -> Monomial.to_list h in
      fun x ->
      Array.of_list (sub x o) in
    let rec filter s = function
      | [] -> s
      | si :: sfilter ->
         match find_separating_plane (List.map Monomial.to_list p) si with
         | None -> filter (si :: s) sfilter
         | Some a ->
            let test sj =
              let sj = center sj in
              let obj = ref Rat.zero in
              for i = 0 to min (Array.length a) (Array.length sj) - 1 do
                obj := Rat.(add !obj (mult a.(i) (Q.of_float sj.(i))))
              done;
              Rat.(compare !obj one) <= 0 in
            filter s (List.filter test sfilter) in
    List.sort compare (filter skeep sfilter) in
  s

let filter s p =
  Format.printf
    "@[<2>%d monomials before filtering:@ @[%a@]@]@."
    (List.length s) (Utils.pp_list ~sep:",@ " Monomial.pp) s;
  Format.printf
    "@[<2>filtering by:@ @[%a@]@]@."
    (Utils.pp_list ~sep:",@ " Monomial.pp) p;
  (* bounding box of the Newton polytope: min_i p_ji <= 2 s_j <= max_i p_ji *)
  let s =
    let mi, ma = match p with
      | [] -> Monomial.(one, one)
      | m :: p ->
         List.fold_left
           (fun (mi, ma) m -> Monomial.(gcd mi m, lcm ma m))
           (m, m) p in
    List.filter
      (fun m ->
       let m = Monomial.mult m m in
       Monomial.divide mi m && Monomial.divide m ma) s in
  let res = filter_newton_polytope_ocplib_simplex s p in
  let res = List.map Monomial.of_list res in
  Format.printf
    "@[<2>%d monomials after filtering:@ @[%a@]@]@."
    (List.length s) (Utils.pp_list ~sep:",@ " Monomial.pp) res;
  res
