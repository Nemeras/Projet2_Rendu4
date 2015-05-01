		(** INITIALISATION **)

open General
open Heuristics

open Types
open Cnf
open DynArray


module Init (C : Clauses) (H : Heuristic) =
struct


(* Renvoie le tableau current correspondant à la liste de clauses "clauses". *)
let cnf_to_vect cnf solution =
	let clauses = DynArray.make (List.length cnf.clauses) [] in
	let current = DynArray.make (List.length cnf.clauses) (C.init_value []) in
	let pos = Array.make (cnf.v_real+1) ([],[]) in
	let rec aux cl i =
		match cl with
		| [] -> ()
		| []::_ ->
			solution.(0) <- -1-i	(* Clause vide rencontrée : cnf n'est pas satisfiable *)
		| c::tail ->
			current.a.(i) <- (C.init_value c) ;
			clauses.a.(i) <- c ;
			C.activate c pos i ;	(* Mise à jour de pos *)
			aux tail (i+1)
	in
	aux cnf.clauses 0 ;
	clauses, current, pos



(* Initie les variables utilisées dans l'algorithme *)
let init cnf learning draw unsat print =
	
	
	(* Tri des littéraux dans les clauses par indice de variable croissant,
	   élimination des tautologies.                                         *)
	ordo cnf ;
	
	(* Initialisation de current, solution, pos *)
	let solution = Array.make (cnf.v_real+1) 0 in
	let levels = Array.make (cnf.v_real+1) 0 in
	let orders = Array.make (cnf.v_real+1) 0 in
	
	let clauses, current, pos = cnf_to_vect cnf solution in
	let l = current.length in		(* Nombre de clauses dans la CNF initiale *)
	let stack = C.init_stack () in		(* stack contient 0 en fond de pile *)
	
	let origins =
		if unsat then
			DynArray.make clauses.length []
		else
			DynArray.make 0 []
	in
	
	let heuristic = H.init cnf pos in
	
	(* Paramètres des algorithmes de Step *)
	let para = {
		back = false ;
		nb_back = 0 ;
		level = -1 ;
		learning = learning ;
		draw = draw ;
		unsat = unsat ;
		print = print ;
	}
	in
	
	let k = ref (
		if solution.(0) < 0 then	(* Si la clause vide est dans la CNF *)
			0
		else
			1
	) in
	
	let compt = ref 0 in
	
	stack, clauses, current, pos, heuristic, origins, solution, levels, orders, k, para, l, compt

end
