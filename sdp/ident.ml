type t = int
type ident = t

let name_hash : (string, bool) Hashtbl.t = Hashtbl.create 31
let id_hash : (int, string) Hashtbl.t = Hashtbl.create 31

let create =
  let cpt = ref (-1) in
  let rec create name nb =
    let name_nb = if nb < 0 then name else name ^ string_of_int nb in
    if Hashtbl.mem name_hash name_nb then
      create name (nb + 1)
    else begin
      incr cpt;
      Hashtbl.add id_hash !cpt name_nb;
      Hashtbl.add name_hash name_nb true;
      !cpt
    end in
  fun name -> create (if name = "" then "x" else name) (-1)

let compare = compare

let pp fmt id = Format.fprintf fmt "%s" (Hashtbl.find id_hash id)

module Set = Set.Make (struct type t = ident let compare = compare end)

module Map = Map.Make (struct type t = ident let compare = compare end)
