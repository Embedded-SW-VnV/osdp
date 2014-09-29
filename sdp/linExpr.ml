module type S = sig
  module  Coeff : Scalar.S
  type t
  val of_list : (Ident.t * Coeff.t) list -> Coeff.t -> t
  val to_list : t -> (Ident.t * Coeff.t) list * Coeff.t
  val const : Coeff.t -> t
  val var : Ident.t -> t
  val mult_scalar : Coeff.t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val eq : t -> t -> bool
  val is_const : t -> bool
  val pp : Format.formatter -> t -> unit
end

module Make (SC : Scalar.S) : S with module Coeff = SC = struct
  module Coeff = SC

  (* type invariant: lin doesn't contain any zero coefficient *)
  type t = { const : Coeff.t; lin : Coeff.t Ident.Map.t }

  let of_list l c =
    let lin = List.fold_left
                (fun m (x, a) ->
                   if Coeff.is_zero a then m else Ident.Map.add x a m)
                Ident.Map.empty l in
    { const = c; lin = lin }

  let to_list a = Ident.Map.bindings a.lin, a.const

  let const c = { const = c; lin = Ident.Map.empty }

  let var id = { const = Coeff.zero; lin = Ident.Map.singleton id Coeff.one }

  let mult_scalar s a =
    if Coeff.is_zero s then
      const Coeff.zero
    else
      { const = Coeff.mult s a.const;
        lin = Ident.Map.map (Coeff.mult s) a.lin }

  let add a1 a2 =
    let lin = Ident.Map.merge
      (fun _ s1 s2 ->
         match s1, s2 with
         | None, None -> None
         | None, Some _ -> s2
         | Some _, None -> s1
         | Some s1, Some s2 ->
            let s = Coeff.add s1 s2 in if Coeff.is_zero s then None else Some s)
      a1.lin a2.lin in
    { const = Coeff.add a1.const a2.const; lin = lin }

  let sub a1 a2 =
    let lin = Ident.Map.merge
      (fun _ s1 s2 ->
         match s1, s2 with
         | None, None -> None
         | None, Some s2 -> Some (Coeff.sub Coeff.zero s2)
         | Some _, None -> s1
         | Some s1, Some s2 ->
            let s = Coeff.sub s1 s2 in if Coeff.is_zero s then None else Some s)
      a1.lin a2.lin in
    { const = Coeff.sub a1.const a2.const; lin = lin }

  let eq a1 a2 =
    let cmp c1 c2 = Coeff.is_zero (Coeff.sub c1 c2) in
    cmp a1.const a2.const && Ident.Map.equal cmp a1.lin a2.lin

  let is_const a = Ident.Map.is_empty a.lin

  let pp fmt a =
    let pp_coeff fmt (x, a) =
      if Coeff.is_zero (Coeff.sub a Coeff.one) then
        Format.fprintf fmt "%a" Ident.pp x
      else if Coeff.is_zero (Coeff.add a Coeff.one) then
        Format.fprintf fmt "-%a" Ident.pp x
      else
        Format.fprintf fmt "%a %a" Coeff.pp a Ident.pp x in
    if is_const a then
      Format.fprintf fmt "%a" Coeff.pp a.const
    else if Coeff.is_zero a.const then
      Format.fprintf fmt "@[%a@]"
                     (Utils.fprintf_list ~sep:"@ + " pp_coeff)
                     (Ident.Map.bindings a.lin)
    else
      Format.fprintf fmt "@[%a@ + %a@]"
                     (Utils.fprintf_list ~sep:"@ + " pp_coeff)
                     (Ident.Map.bindings a.lin)
                     Coeff.pp a.const
end

module Num = Make (Scalar.Num) 

module Float = Make (Scalar.Float)

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
