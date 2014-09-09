(*
Take a list of expressions that have to be SOS
Each  expression is expressed over sos_unknown of fixed degree in a provided monomial basis
It returns
- a set of sdp vars
- a list of constraints of those
- a list of mapping from sdp vars to SOS

each sos unknown is expressed as a SOS over the monomial basis. 
on deplie les equations sur nos variables SOS : base^t P base 
et on reconstruit une version SOS sur la base

on a besoin pour ca de structures de donnee efficaces pour 
- etant donne un monome, savoir l'exprimer dans la base SOS, par ex. comme expression des variables sdp: monome en x = 2*b, donc si on cree le monome avec le coeff 2: 2*x, on doit avoir b = 1
- etant donne deux expressions, calculer le produit des deux: savoir efficace le deplier en monomes et reidentifier les variables sdp associées.
- si la base en exotique, ca peut demander de renormaliser pour retomber dans la base

it requires a hashtbl from monomial to set of indices ...
*)

let sos_to_sdp expressions vars degree basis =


(*
dans une matrice M sym matrix of dim n *n, appliqué à une base monomiale B^n
le coeff associé au monome x_a * x_b où x_a,x_b \in B^n
est:
si exist x_c \in B^n et k \in [1,n], tq x_a * x_b = x_c^k
alors coeff(x_a * x_b] = [M()
*)



(*
on a deux polynome

*)
