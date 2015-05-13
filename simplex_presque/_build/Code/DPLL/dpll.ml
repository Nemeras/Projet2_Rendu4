			(** ALGORITHME DPLL **)

open General
open Theories

(* La structure principale de l'algorithme DPLL *)

open Types
open Cnf
open Init
open Step
open Propa
open Print_step



(* On manipule les clauses dans le tableau dynamique current, qui est un ensemble de clauses de type C.t         *)

(* Variables utilisées dans la suite :
	stack : pile des affectations.
	k : référence du littéral considéré à chaque étape.
	level : référence du numéro de niveau de décision courant.
	back : référence de booléen valant vrai si l'algorithme est en phase de backtracking, faux sinon.
	nb_back : référence indiquant le nombre obligatoire de niveaux de décision à remonter dans le backtrack.
	learning : vrai si le clause learning est activé, faux sinon.
	draw : (référence) vrai si le mode interactif du clause learning est activé, faux sinon.
	unsat : vrai si l'explication de l'insatisfiabilité est activée, faux sinon.
	print : vrai si l'affichage est activé, faux sinon.
	para : ensemble de paramètres (cf parameters.ml).                                                        *)

(* fst pos.(n) si n > 0, et snd pos.(-n) sinon, contient la liste des clauses dans lesquelles apparaît le
   littéral n (version sans littréaux surveillés), et la liste de celles dans lesquelles le littéral n est
   surveillé (version WL).                                                                                       *)

(* solution désigne dans la suite l'instanciation courante des variables :
	solution.(k) = 1 si la variable k est à vrai suite à un pari / une hypothèse.
	             > 1 si la variable k est à vrai suite à une propagation / déduction.
	solution.(k) = -1 si la variable k est à faux suite à un pari / une hypothèse.
	             < -1 si la variable k est à faux suite à une propagation / déduction.
	solution.(k) = 0 si la valeur de la variable k est indéterminée .
	solution.(0) < 0 si une contradiction a été trouvée ; alors, -solution.(0)-1 est l'indice de la clause
                         correspondante si la contradiction vient de DPLL et pas du solveur de la théorie.
   Si abs solution.(k) > 1, alors abs solution.(k) - 2 désigne la clause qui a entraîné la
   propagation sur la variable k.                                                                                *)

(* levels assigne à chaque variable le niveau de décision auquel elle a été déterminé.
   orders.(k) = 0 si k est un pari, et n si k est la n-ème variable a avoit été déterminée suite à une
   propagation dans le niveau de décision levels.(k).
   origins est un dynarray de longueur égale à current. origins.a.(i) contient la liste des clauses qui ont été
   utilisées pour créer la clause i à partir d'une chaîne de résolution.                                         *)



(* DPLL prend en entrée le type de clauses manipulées (avec littéraux surveillés ou non), et la théorie associée à la CNF *)
module DPLL (C : Clauses) (H : Heuristic) (T : Theory) =
struct


module I = Init (C) (H)
module S = Step (C) (H) (T)
module Bcp = Propa (C) (H) (T)
module P = Print_step (C)



(* Boucle principale de DPLL *)
let loop cnf stack solver clauses current pos heuristic origins solution levels orders k para compt =
	
	while abs !k <= cnf.v_real && !k <> 0 do
		incr compt ;
		
		(* Si on est revenu au début des paris car on a appris une clause unitaire, on réinitialise
		   les valeurs et on reprend l'exécution.                                                   *)
		if solution.(0) = 1 then
			begin
			solution.(0) <- 0 ;
			para.back <- false ;
			para.level <- -1 ;
			k := 0
			end
		;
		
		(* Affichage, si autorisé *)
		if para.print then
			P.print_step current solution para.back !compt ;
		
		(* Détection des clauses unitaires *)
		if solution.(0) = 0 then
			Bcp.propa stack solver current pos heuristic solution levels orders para ;
		
		(* Si toutes les variables ont été instanciées *)
		if H.is_instanciation_full heuristic then
			(* S'il y a contradiction : backtrack *)
			if solution.(0) < 0 then
				S.continue stack solver clauses current pos heuristic origins solution levels orders k para
			(* Sinon : c'est fini *)
			else
				k := cnf.v_real + 1
		
		(* Sinon : on continue *)
		else
			S.continue stack solver clauses current pos heuristic origins solution levels orders k para
	
	done



(* Renvoie le résultat de DPLL *)
let result k l clauses origins solution para =
	if !k = 0 then
		begin
		(* Preuve de l'insatisfiabilité *)
		if para.unsat then
			Unsat.create_unsat origins clauses solution l ;
		False
		end
	else
		True solution



		(** RESOLUTION - STRUCTURE DE DPLL **)



(* Renvoie une solution associée à la CNF cnf donnée en entrée :
	False si cnf n'est pas satisfiable.
	True solution si cnf est satisfiable, avec solution une instanciation qui la satisfait. *)

let solve cnf solver learning draw unsat print =
	
	(* Initialisation des variables *)
	let
		stack,
		clauses, current, pos,
		heuristic,
		origins,
		solution, levels, orders,
		k, para, l, compt
	= I.init cnf learning draw unsat print in
	
	(* Boucle principale *)
	loop cnf stack solver clauses current pos heuristic origins solution levels orders k para compt ;
	
	(* Résultat *)
	result k l clauses origins solution para



end
