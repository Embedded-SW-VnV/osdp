(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type t = int
type ident = t

let name_hash : (string, bool) Hashtbl.t = Hashtbl.create 31
let id_hash : (int, string) Hashtbl.t = Hashtbl.create 31

let create =
  let cpt = ref (-1) in
  let rec create name nb =
    let name_nb = if nb < 0 then name else name ^ string_of_int nb in
    if Hashtbl.mem name_hash name_nb then
      create name (nb + 1)
    else begin
      incr cpt;
      Hashtbl.add id_hash !cpt name_nb;
      Hashtbl.add name_hash name_nb true;
      !cpt
    end in
  fun name -> create (if name = "" then "x" else name) (-1)

let compare = compare

let pp fmt id = Format.fprintf fmt "%s" (Hashtbl.find id_hash id)

module Set = Set.Make (struct type t = ident let compare = compare end)

module Map = Map.Make (struct type t = ident let compare = compare end)
