		(** HEURISTIQUE VSIDS **)


open General
open Cnf

(* On utilise pour implémenter VSIDS un tableau donnant pour chaque littéral son score,
   et un Set de couples (score, littéral) permettant de les trier et de choisir celui de plus grand score *)


module Lit =
struct
	type t = float*int	(* 1ère composante : le score du littéral en 2e composante *)
	let compare x y = - Pervasives.compare x y	(* On veut trier les littéraux par scores décroissants *)
end

module S = Set.Make(Lit)	(* Ensembles des littéraux pondérés par leur score *)

open S


type struc = {
	scores : float array ;
	mutable sort : S.t
}


let heuristic = 4


(* Donne le score de x *)
let score x h =
	if x > 0 then
		h.scores.(x)
	else
		h.scores.(Array.length h.scores + x)


(* Indique si x est dans le Set h.sort *)
let is_in x h =
	mem (score x h, x) h.sort


(* Ajoute x dans le Set *)
let add_score x h =
	h.sort <- add (score x h, x) h.sort


(* Enlève x du Set *)
let remove_score x h =
	h.sort <- remove (score x h, x) h.sort


(* Initie h.scores et h.vsids *)
let init cnf pos =
	let n = cnf.v_real in
	let h = {
		scores = Array.make (2*n+1) 0. ;
		sort = empty
	} in
	for i = 1 to n do
		(* Le score du littréal x sera le nombre clauses dans lequel il aphît (ou est surveillé pour les WL *)
		h.scores.(i) <- float_of_int (List.length (fst pos.(i))) ;
		h.scores.(2*n-i+1) <- float_of_int (List.length (snd pos.(i))) ;
		add_score i h ;
		add_score (-i) h
	done ;
	h


let update h x =
	remove_score x h ;
	remove_score (-x) h


let backtrack h x =
	add_score x h ;
	add_score (-x) h


(* Attribue à x le score s *)
let modif x s h =
	let b = is_in x h in
	if b then
		remove_score x h ;
	if x > 0 then
		h.scores.(x) <- s
	else
		h.scores.(Array.length h.scores + x) <- s ;
	if b then
		add_score x h


(* Augmente d'un le score du littréal x *)
let incr_lit x h =
	modif x (score x h +. 1.) h


(* Augmente d'un le score des littéraux de la clause c *)
let rec incr c h =
	match c with
	| [] -> ()
	| x::c2 ->
		incr_lit x h ;
		incr c2 h


(* Multiplie tous les scores par 0.9 (à chaque conflit) *)
let decr_scores h =
	for i = 1 to (Array.length h.scores)/2 do
		modif i (0.9 *. score i h) h ;
		modif (-i) (0.9 *. score (-i) h) h
	done


let learning h c =
	decr_scores h ;
	incr c h


(* Donne le littéral de plus gros score (min_elt car on a pris l'opposé de compare dans Types.Lit) *)
let next h pos current wl =
	snd (min_elt h.sort)


let is_instanciation_full h =
	is_empty h.sort
