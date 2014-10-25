val fprintf_list :
  sep:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e list -> unit

val fprintf_array :
  sep:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e array -> unit

val fprintf_matrix :
  begl:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  endl:('e, 'f, 'g, 'h, 'h, 'e) format6 ->
  sepl:('i, 'j, 'k, 'l, 'l, 'i) format6 ->
  sepc:('m, 'n, 'o, 'p, 'p, 'm) format6 ->
  (Format.formatter -> 'q -> unit) -> Format.formatter -> 'q array array -> unit

val profile : 'a Lazy.t -> 'a * float

val num_of_float : float -> Num.num
