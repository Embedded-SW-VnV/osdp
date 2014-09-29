module IntMap : Map.S with type key = int

val fprintf_list :
  sep:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e list -> unit

val fprintf_array :
  sep:('a, 'b, 'c, 'd, 'd, 'a) format6 ->
  (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e array -> unit

val profile : 'a Lazy.t -> 'a * float

val num_of_float : float -> Num.num

val merge_sorted_lists :
  ('a -> 'a -> int) ->
  ('a -> 'a -> 'a option) -> 'a list -> 'a list -> 'a list
