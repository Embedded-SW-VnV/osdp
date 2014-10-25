type matrix = (int * int * float) list
type block_diag_matrix = (int * matrix) list

external solve : block_diag_matrix -> (block_diag_matrix * float) list ->
                 SdpRet.t * (float * float) * (float array array list * float array) =
  "moseksdp_solve"
