type matrix = (int * int * float) list
type block_diag_matrix = (int * matrix) list

external solve : block_diag_matrix -> (block_diag_matrix * float) list ->
  float * (float array array list * float array) =
    "moseksdp_solve"

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
