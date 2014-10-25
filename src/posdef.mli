(** Interface towards a C code proving positive definiteness of
    matrices. *)

(** Takes as input a square matrix of Q.t of size nxn and returns 1 if it
    manages to prove that the matrix is symmetric positive definite and 0
    otherwise (i.e. the matrix is either not symmetric positive definite
    or its smallest eigenvalue is too small for the proof to succeed). *)
val check : Q.t array array -> bool
