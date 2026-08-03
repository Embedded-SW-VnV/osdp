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

(** Minimal, osdp-agnostic structured logging.

    This module knows nothing about {!Sos}, {!Sdp} or {!SosBasis}: it is a
    generic key/value row logger, so any project can log whatever fields
    make sense to it -- osdp's own solver/basis/padding sweeps, or a
    downstream project's domain-specific context (an example name, a box,
    a confidence level, ...) -- into the same accumulating file, with no
    coupling between what osdp records about a solve and what a consumer
    records about its own use of that solve.

    Rows accumulate: nothing already present in a log file is ever read,
    rewritten or truncated by {!record}, so results from repeated runs --
    across solvers, options, or entirely different consumer projects
    writing to the same file -- build up into a history over time rather
    than a single latest snapshot.

    A companion report generator ([gen_report.py], schema-agnostic: it
    renders whatever fields are present in the log, rather than a fixed
    column set) is installed alongside this library and reachable via
    [opam var osdp:share]. *)

(** [record ~log fields] appends one row to [log] as a single JSON
    object, one per line ({{:https://jsonlines.org/}JSON Lines}),
    creating the file (and its immediate parent directory, if missing)
    if needed. An ["timestamp"] field (UTC, ISO 8601, millisecond
    precision) is added automatically; [fields] should not include one.

    Safe for concurrent appenders and for repeated invocations spread
    over time: uses an exclusive lock around the write so rows from
    different processes (or different projects logging to the same
    file) are never interleaved or corrupted. *)
val record : log:string -> (string * string) list -> unit
