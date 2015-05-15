			(** LECTURE DU FICHIER ET LANCEMENT DE L'ALGORITHME **)

open DPLL
open General

open Cnf
open Types


(* Initie la CNF et le solveur correspondant à la théorie choisie, et lance DPLL(T) *)
module Init_theory (T : Theory) =
struct
	
	let init file wl learning heuristic draw unsat print aff_cnf unsatdpllt=
		
		let cnf, solver = T.create file aff_cnf in
		
		let module C = (val (version wl) : Clauses) in
		let module H = (val (choose_heuristic heuristic) : Heuristic) in
		let module S = Dpll.DPLL (C) (H) (T) in
		
		let res = S.solve cnf solver learning draw unsat print unsatdpllt in
		T.print_solution res solver
	
end

(* Fonction principale *)
let _ =
	
	(* Gestion des arguments et des options *)
	let file = ref "" in		(* Nom du fichier d'entrée *)
	let wl = ref false in		(* True ssi les littéraux surveillés sont activés *)
	let learning = ref false in	(* True ssi le clause learning est activé *)
	let draw = ref false in		(* True ssi le mode interactif est activé *)
	let unsat = ref false in	(* True ssi on veut prouver l'insatisfiabilité de la CNF *)
	let print = ref false in	(* True ssi on active l'affichage *)
	let cnf = ref false in 		(* True ssi on active l'affichage de la cnf étudiée *)
	let unsatdpllt = ref true in

	let theory = ref 0 in		(* Numéro de la théorie utilisée *)
	let heuristic = ref 0 in
	
	let options = [ 
		("-wl", Arg.Set wl, "Active les littéraux surveillés.") ;
		("-cl", Arg.Set learning, "Active l'apprentissage de clauses.") ;
		("-cl-interac", Arg.Unit (fun () -> learning := true ; draw := true), "Active l'apprentissage de clauses et le mode interactif.") ;
		("-rand", Arg.Unit (fun () -> heuristic := 1), "Active le choix du pari aléatoire.") ;
		("-moms", Arg.Unit (fun () -> heuristic := 2), "Active l'heuristique MOMS.") ;
		("-dlis", Arg.Unit (fun () -> heuristic := 3), "Active l'heuristique DLIDS.") ;
		("-vsids", Arg.Unit (fun () -> learning := true ; heuristic := 4), "Active l'heuristique VSIDS.") ;
		("-explainunsat", Arg.Unit (fun () -> learning := true ; unsat := true), "Active la preuve de l'insatisfiabilité.") ;
		("-tseitin", Arg.Unit (fun () -> theory := 1), "Lit une formule logique quelconque.") ;
		("-equality", Arg.Unit (fun () -> theory := 2), "Solveur sur la théorie de l'égalité.") ;
		("-simplex", Arg.Unit (fun () -> theory := 3), "Solveur sur la théorie de l'arithmétique linéaire") ;
		("-print", Arg.Set print, "Active l'affichage des étapes intermédiaires de l'algorithme.") ;
		("-cnf", Arg.Set cnf, "Active l'affichage de la cnf étudiée dans DPLL.")
	] in
	
	Arg.parse options (fun s -> file := s)	"Ce programme résout l'instance de SAT / Tseitin / la théorie de l'égalité donnée dans le fichier en entrée." ;
	
	if ((!theory = 2)||(!theory =4)) && !unsat then
		failwith "L'égalité/les simplex et l'explication de l'insatisfiabilité sont incompatibles dans DPLL(T)" ;
	
	if !heuristic = 1 then
		Random.self_init () ;
	if !theory = 4 then
	  unsatdpllt:= false;

	let module T = (val (choose_theory !theory) : Theory) in	(* Théorie utilisée *)
	let module Launch = Init_theory (T) in
	Launch.init !file !wl !learning !heuristic !draw !unsat !print !cnf !unsatdpllt	(* Lancement de l'algorithme *)
