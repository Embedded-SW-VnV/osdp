external check_c : float array array -> bool = "posdef_check"

let check m =
  (* TODO: This relies on the assumption that Scalar.Q.to_float
     returns a closest floating point value. But that could be wrong
     in some cases. *)
  let m = Array.map (Array.map Scalar.Q.to_float) m in
  check_c m

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
