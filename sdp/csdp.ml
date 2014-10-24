type matrix = float array array
type block_diag_matrix = matrix list

external solve : block_diag_matrix -> (block_diag_matrix * float) list ->
                 SdpRet.t * (float * float) * (block_diag_matrix * float array) =
  "csdp_solve"

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
