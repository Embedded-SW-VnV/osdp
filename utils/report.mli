(*
 * SMT-AI: an abstract interpreter to be used by a k-induction model checker
 * Copyright (C) 2010  P.L. Garoche and P. Roux
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
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

exception Error

(** Print a log message (level 1) or not according to Global.verbosity. *)
val log : ?kind:string -> string Lazy.t -> unit

(** Print a debug message (level 2) or not according to Global.verbosity. *)
val debug : ?kind:string -> string Lazy.t -> unit

(** Print a log message (level 1) or not according to Global.verbosity. The argument must print the message on the given formatter. *)
val logf : ?level:int -> ?kind:string -> (Format.formatter -> unit) -> unit

(** Print a debug message (level 2) or not according to Global.verbosity. The argument must print the message on the given formatter. *)
val debugf : ?level:int -> ?kind:string -> (Format.formatter -> unit) -> unit

(** debug_indent i s prints a debug message of level i+2 (requires i > 0)
    (or not according to verbosity) with string s indented i times. *)
val debug_indent : ?kind:string -> int -> string Lazy.t -> unit

(** Print error message along with location and raise Error. *)
val error_loc : Location.t -> string -> 'a

(** Print error message along with location in another file than Global.filename
    and raise Error. *)
val error_loc_file : Location.t -> string -> string -> 'a

(** Print error message along with location and a message
    "previous occurence was here" along with second location
    and eventually raise Error. *)
val error_loc_previous : Location.t -> Location.t -> string -> 'a
