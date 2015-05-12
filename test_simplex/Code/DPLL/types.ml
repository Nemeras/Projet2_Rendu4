			(** DEFINIT TOUS LES TYPES UTILISES DANS L'ENSEMBLE DE DPLL **)

open General
open Theories
open Heuristics

open DynArray



		(** ABSTRACTION DES CLAUSES ETUDIEES (NORMALES/WL) **)


(* Tableau stockant les listes de clauses dans lesquelles apparait chaque littéral / chaque littéral surveillé selon la version *)
type pos = ((int list) * (int list)) array

type sol = int array	(* Instanciation courante des variables *)
type lev = int array	(* Niveaux de décisions auxquels les variables ont été instanciées *)



(* Type de module implémentant les fonctions agissant sur le type de clauses sur lesquelles on travaille
   Permet d'implémenter la version de base et la verion littéraux surveillés de DPLL                     *)
module type Clauses =
sig
	
	val wl : bool
	
		(* Type des clauses contenues dans le tableau current *)
	type t
	
		(* Type représentant la pile des instanciations *)
	type stack
	
		(* Initie la pile *)
	val init_stack : unit -> stack
	
		(* Renvoie le littéral en début de pile *)
	val pick : stack -> Cnf.literal
	
		(* Renvoie un clause de type t correspondant à une clause de type Cnf.clause *)
	val init_value : Cnf.clause -> t
	
		(* Rajoute dans pos la clause en entrée pour chaque littéral qu'elle contient *)
	val activate : Cnf.clause -> pos -> int -> unit
	
		(* Place dans une référence la liste des clauses unitaires et des littéraux à mettre à vrai en conséquence *)
	val units : t dynarray -> sol -> (int*int) list ref -> unit
	
		(* Fait toutes les opérations nécessaires pour placer le littéral en entrée à vrai
		   Renvoie la liste des clauses unitaires rencontrées                              *)
	val update : Cnf.literal -> stack -> t dynarray -> pos -> sol -> (int * int) list
	
		(* Fait toutes les opérations nécessaires pour annuler la dernière instanciation *)
	val backtrack : stack -> t dynarray -> pos -> lev -> Cnf.literal
	
		(* Transforme la cause en entrée en une clause de type t, de telle sorte que la clause en sortie soit
		   cohérente avec l'état courant de la pile                                                           *)
	val maj_cl : stack -> Cnf.clause -> pos -> lev -> int -> t
	
		(* Construit le graphe des conflits *)
	val edges : int -> Dot.graph -> t dynarray -> sol -> lev -> int array -> bool array -> Cnf.literal -> int -> int -> unit
	
		(* Fonction d'impression d'une claue *)
	val print_clause : t -> sol -> unit
	
		(* Indique si la clause en entrée est satisfaite *)
	val is_clause_true : t -> sol -> bool
	
	val current_clauses : t dynarray -> sol -> int -> Cnf.clause dynarray
	
end



(* Renvoie le module implémentant les littéraux surveillés si wl est vrai, le module de base sinon *)
let version wl =
	if not wl then
		(module Clauses_basic : Clauses)	(* Version de base *)
	else
		(module Clauses_wl : Clauses)		(* Version WL *)




		(** THEORIES **)


(* Type de module implémentant une théorie *)
module type Theory =
sig
	
		(* Type représentant les atomes des formules de la théorie *)
	type atom
	
		(* Type représentant la structure du solveur *)
	type struc
	
		(* Crée, à partir du nom du fichier, le solveur et la CNF associée à la formule *)
	val create : string -> bool -> Cnf.cnf * struc
	
		(* Met à jour la structure lors de l'instanciation d'un littéral
		   Renvoie 0 si aucune contradiction n'a été détectée, -max_int sinon *)
	val update : struc -> Cnf.literal -> int
	
		(* Annule la dernière instanciation *)
	val backtrack : struc -> Cnf.literal -> unit
	
		(* Renvoie une clause expliquant la contradiction engendrée par le solveur *)
	val unsat : struc -> Cnf.clause
	
		(* Fonction affichant la solution et une instanciation correspondant à l'éventuelle satisfiabilité *)
	val print_solution : Cnf.solution -> struc -> unit
	
end


(* Renvoie la théorie correspondant au numéro *)
let choose_theory theory =
	match theory with
	| 0 -> (module Base : Theory)		(* Théorie de base qui ne concerne que les CNF *)
	| 1 -> (module Empty : Theory)		(* Thérie vide implémentant Tseitin *)
	| 2 -> (module Equality : Theory)	(* Théorie de l'égalité *)
	| 3 -> (module Simplex : Theory)        (*Théorie de l'arithmétique linéaire*)
	| _ -> failwith "Erreur dans le choix de la théorie"





		(** PARAMETRES DES PROGRAMMES **)


(* Evite de donner plus de 10 arguments à une seule fonction *)

type parameters = {
	mutable back : bool ;		(* Indique si on est dans une phase de backtrack *)
	mutable nb_back : int ;		(* Nombre de liveaux de décision à enlever lors du backtrack *)
	mutable level : int ;		(* Niveau de décision courant *)
	learning : bool ;		(* Indique si on doit utiliser le clause learning *)
	mutable draw : bool ;		(* Indique si le mode iteractif du clause learning est activé *)
	unsat : bool ;			(* Indique si l'explication de l'insatisfiabilité est désactivée *)
	print : bool ;			(* Indique si on doit afficher les étapes intermédiaires de DPLL *)
}







module type Heuristic =
sig
	
	type struc
	
	val heuristic : int
	
	val init : Cnf.cnf -> pos -> struc
	
	val update : struc -> Cnf.literal -> unit
	
	val backtrack : struc -> Cnf.literal -> unit
	
	val learning : struc -> Cnf.clause -> unit
	
	val next : struc -> pos -> Cnf.clause dynarray -> bool -> Cnf.literal
	
	val is_instanciation_full : struc -> bool
	
end



(* Renvoie l'heuristique correspondant au numéro *)
let choose_heuristic heuristic =
	match heuristic with
	| 0 -> (module None : Heuristic)
	| 1 -> (module Rand : Heuristic)
	| 2 -> (module Moms : Heuristic)
	| 3 -> (module Dlis : Heuristic)
	| 4 -> (module Vsids : Heuristic)
	| _ -> failwith "Erreur dans le choix de l'heuristique"
