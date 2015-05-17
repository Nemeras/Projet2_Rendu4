			(** HEURISTIQUE DE BASE **)
			(** Type : Heuristic **)


(* Implémente le choix par défaut du prochain pari : on parie positivement sur la première variable libre *)


(* Ces commentaires complètent ceux faits dans types.ml *)


open General
open Cnf


let heuristic = 0


type struc = {
	taken : (bool*int) array ;	(* fst taken.(i) = true si la variable i est déjà instanciée, et snd taken.(i) est le pari précédent le cas échéant *)
	mutable k : int			(* Première variable libre *)
}


let init cnf pos =
	{
		(* On ajoute à taken une case de plus, qui sert à vérifier si on a instancié toutes les variables *)
		taken = Array.make (cnf.v_real+2) (false, 0) ;
		k = 1
	}


let update h x =
	h.taken.(abs x) <- true, snd h.taken.(abs x) ;
	(* S'il s'agit d'un pari, on met à jour k *)
	if abs x = h.k then
		begin
		while fst h.taken.(h.k) do
			h.k <- h.k + 1
		done ;
		h.taken.(h.k) <- false, abs x
		end


let backtrack h x =
	h.taken.(abs x) <- false, snd h.taken.(abs x) ;
	(* Si le pari annulé était sur une variable d'indice inférieure à l'actuelle, on met à jour k *)
	if abs x = snd h.taken.(h.k) then
		h.k <- abs x


let learning h c =
	()


let next h pos current wl =
	h.k


let is_instanciation_full h =
	h.k = Array.length h.taken - 1
