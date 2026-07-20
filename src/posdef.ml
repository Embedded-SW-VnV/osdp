(*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
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

external check_itv : (float * float) array array -> bool = "posdef_check"

let check m = check_itv (Array.map (Array.map Utils.itv_float_of_q) m)

let check_complete m =
  let sz = Array.length m in
  (* Format.printf "Posdef.check_complete: sz = %d@." sz; *)
  try
    for j = 0 to sz - 1 do
      for i = 0 to j - 1 do
        if not (Q.equal m.(i).(j) m.(j).(i)) then raise Exit
      done
    done;
    let l = Array.make_matrix sz sz Q.zero in
    let d = Array.make sz Q.zero in
    for j = 0 to sz - 1 do
      (* Format.printf "Posdef.check_complete: j = %d@." j; *)
      for i = 0 to j - 1 do
        let s = ref m.(i).(j) in
        for k = 0 to i - 1 do
          s := Q.(!s - l.(k).(i) * l.(k).(j) * d.(k))
        done;
        l.(i).(j) <- Q.(!s / d.(i))
      done;
      let s = ref m.(j).(j) in
      for k = 0 to j - 1 do
        s := Q.(!s - l.(k).(j) * l.(k).(j) * d.(k))
      done;
      if Q.leq !s Q.zero then raise Exit;
      d.(j) <- !s
    done;
    true
  with Exit -> false

let check_PSD m =
  let sz = Array.length m in
  (* Format.printf "Posdef.check_PSD: sz = %d@." sz; *)
  try
    for j = 0 to sz - 1 do
      for i = 0 to j - 1 do
        if not (Q.equal m.(i).(j) m.(j).(i)) then raise Exit
      done
    done;
    let l = Array.(map copy m) in
    for i = 0 to sz - 1 do
      let a = l.(i).(i) in
      if Q.lt a Q.zero then raise Exit;
      if Q.equal a Q.zero then
        for j = i + 1 to sz - 1 do
          if not (Q.equal l.(i).(j) Q.zero) then raise Exit
        done
      else
        for j = i + 1 to sz - 1 do
          for k = j to sz - 1 do
            l.(j).(k) <- Q.(l.(j).(k) - l.(i).(j) * l.(i).(k) / a)
          done
        done
    done;
    true
  with Exit -> false

external string_of_float_bin : float -> string = "string_of_float_bin"
