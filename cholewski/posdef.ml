external check_c : float array array -> bool = "posdef_check"

let check m =
  (* TODO: This relies on the assumption that Num.float_of_num
   * returns the closest floating point value. But there is not really any
   * guarantee of that. *)
  let m = Array.map (Array.map Num.float_of_num) m in
  check_c m

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
