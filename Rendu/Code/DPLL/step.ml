			(** ETAPES DE L'ALGORITHME **)

open General
open Theories

(* On donne ici les implémentations des fonctions utilisées dans l'algorithme, dans dpll.ml *)


open Types
open Learning
open DynArray
open Print_step


module Step (C : Clauses) (H : Heuristic) (T : Theory) =
struct


module P = Print_step (C)



		(** ETAPES DU BACKTRACK **)



(* Effectue une étape de backtrack *)
let backtrack_step stack solver current pos heuristic solution levels orders k para =
	
	(* Si la valeur de début n'est pas issue d'une boolean constraint propagation, donc pas nécessaire,
	   on peut supposer l'opposé. On arrête alors le backtrack.                                         *)
	if para.nb_back = 0 && abs solution.(abs !k) = 1 then
		begin
		(* Dernier backtrack *)
		solution.(0) <- 0 ;
		para.back <- false ;
		H.backtrack heuristic !k ;
		k := C.backtrack stack current pos solution ;
		T.backtrack solver !k ;
		P.print_backtrack !k solution.(abs !k) para.print ;
		solution.(abs !k) <- 0 ;
		levels.(abs !k) <- 0 ;
		para.level <- para.level - 1 ;
		
		(* S'il n'y a pas de clause learning, on suppose l'opposé en le considérant comme une déduction *)
		if not para.learning then
			begin
			k := - !k ;
			para.level <- para.level + 1 ;
			solution.(abs !k) <- 2*((abs !k)/ !k) ;
			solution.(0) <- T.update solver !k ;
			H.update heuristic !k ;
			let _ = C.update !k stack current pos solution in
			P.print_hyp !k para.print ;
			levels.(abs !k) <- para.level ;
			orders.(abs !k) <- 0
			end
		end
	
	(* Sinon, il faut continuer le backtrack *)
	else
		begin
		if abs solution.(abs !k) = 1 then
			para.level <- para.level - 1 ;	(* Un niveau de décision a été entièrement annulé *)
		P.print_backtrack !k solution.(abs !k) para.print ;
		solution.(abs !k) <- 0 ;
		levels.(abs !k) <- 0 ;
		T.backtrack solver !k ;
		H.backtrack heuristic !k ;
		k := C.backtrack stack current pos solution ;
		k := C.pick stack
		end




		(** ITERATION DE DPLL **)



(* Implémente une itération de la boucle *)
let continue stack solver clauses current pos heuristic origins solution levels orders k para =
	
	(* On vient de découvrir la clause vide : on commence le backtrack *)
	if solution.(0) < 0 && not para.back then
		begin
		
		if !k != 0 then
			k := C.pick stack ;		(* On a besoin de connaître la valeur à dépiler *)
		P.print_new_backtrack para.print ;
		para.back <- true ;
		
		if solution.(0) = -max_int then
			(* La contradiction vient de la théorie *)
			begin
			let new_clause = T.unsat solver in
			let clause_mod = C.maj_cl stack new_clause pos levels current.length in
			DynArray.add clauses new_clause [] ;
			DynArray.add current clause_mod (C.init_value [])
			end
		else
			(* Apprentissage de clause *)
			let module L = Learning (C) (H) (T) in
			L.learning stack solver clauses current pos heuristic solution levels orders k para origins
		
		end
	
	(* Backtracking : on n'a pas encore pu faire de nouvelle hypothèse pour enlever la contradiction *)
	else if para.back then
		begin
		(* On décrémente nb_back si on a passé un niveau de décision *)
		if abs solution.(abs !k) = 1 && para.nb_back > 0 then
			para.nb_back <- para.nb_back - 1 ;
		backtrack_step stack solver current pos heuristic solution levels orders k para
		end
	
	(* S'il n'y a pas de contradiction : on fait appel à l'heuristique pour choisir le prochain pari *)
	else
		begin
		(* On fournit, selon l'heuristique, l'état courant de la CNF (pour MOMS et DLIS), ainsi
		   que la méthode de travail sur les clauses (WL ou non)                                *)
		k := H.next heuristic pos (C.current_clauses current solution H.heuristic) C.wl ;
		if !k <> 0 && solution.(abs !k) = 0 then
			begin
			para.level <- para.level + 1 ;
			P.print_hyp !k para.print ;
			solution.(abs !k) <- (abs !k)/ !k ;
			solution.(0) <- T.update solver !k ;
			H.update heuristic !k ;
			let _ = C.update !k stack current pos solution in
			levels.(abs !k) <- para.level ;
			orders.(abs !k) <- 0 ;
			end
		end



end
