(** Interface towards a C code proving positive definiteness of
    matrices. *)

(** Takes as input a square matrix of Num of size nxn and returns 1 if it
    manages to prove that the matrix is symmetric positive definite and 0
    otherwise (i.e. the matrix is either not symmetric positive definite
    or its smallest eigenvalue is too small for the proof to succeed). *)
val check : Num.num array array -> bool

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
