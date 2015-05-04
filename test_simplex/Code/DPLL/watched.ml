			(** CLAUSES AVEC LITTÉRAUX SURVEILLES **)

open General

(* Les clauses avec littéraux surveillés sont toujours implémentées avec des listes : les littéraux surveillés
   d'une clause sont ses deux premiers éléments.
   On traite dans des cas à part les clauses ne possédant qu'un seul littéral.
   Enfin, un invariant des transformations qui suivent est que tous les littéraux évalués à faux d'une clause
   se trouvent au fond de celle-ci.                                                                            *)

open List



(* Indique si le littéral n est faux *)
let is_false n solution =
	n*solution.(abs n) < 0


(* Indique si le littéral n est vrai *)
let is_true n solution =
	n*solution.(abs n) > 0


(* Recule le premier élément de c jusqu'à rencontrer le bloc des littéraux à faux. *)
let rec to_end c solution =
	match c with
	| h::h2::tail when is_false h2 solution -> c
	| h::h2::tail -> h2::(to_end (h::tail) solution)
	| [h] -> c
	| [] -> []


(* Place tous les littéraux faux au fond de la liste *)
let rec all_false_to_end c solution =
	let rec aux c false_list =
		match c with
		| [] -> rev false_list
		| h::c2 when not (is_false h solution) -> h::(aux c2 false_list)
		| h::c2 -> aux c2 (h::false_list)
	in
	aux c []


(* Indique si la clause c est mise à vraie par un de ses littéraux surveillés. *)
let is_w_true c solution =
	match c with
	| h::h2::_ when is_true h solution -> true
	| h::h2::_ when is_true h2 solution -> true
	| h::h2::_ -> false
	| [h] -> is_true h solution
	| [] -> false


(* Indique si la clause est fausse, i.e. si ses deux littéraux surveillés sont à faux avec l'invariant *)
let is_clause_false c solution =
	match c with
	| h::h2::_ when (is_false h2 solution) && (is_false h solution) -> solution.(0) <- -2
	| h::h2::_ -> ()
	| [h] when (is_false h solution) ->
		solution.(0) <- -2
	| _ -> ()

 
(* Change le(s) littéral(aux) surveillé(s) qui a été mis à faux *)
let change_clause c solution =
	all_false_to_end c solution


(* Indique si la clause est unitaire, i.e. s'il ne reste plus qu'un littéral (le premier) à vrai, grâce à l'invariant *)
let is_unit c solution =
	match c with
	| h::h2::_ when (not (is_false h solution)) && (not (is_true h solution)) && (is_false h2 solution) -> true
	| h::h2::_ -> false
	| [h] when (not (is_false h solution)) && (not (is_true h solution)) -> true
	| _ -> false
