			(** PREUVE DE L'INSATISFIABILITE **)


open General

open Printf
open DynArray



		(** SOUS-ENSEMBLE INSATISFIABLE **)

(* Place dans le fichier buffer toutes les clauses ayant servi à construire start, et les clauses ayant entraîné,
   avec une propagation, la mise à faux d'un littéral dans la clause start.
   l : taille de la CNF initialement étudiée, avant le clause learning.                                           *)
let rec print_l buffer origins start clauses solution l =
	
	(* Explore toutes les clauses dont les indices sont dans index *)
	let rec explore_clauses index =
		match index with
		| [] -> ()
		| i::tail ->
			print_l buffer origins i clauses solution l ;
			explore_clauses tail
	in
	
	(* Explore tous les littéraux mis à faux dans la clause c, et les clauses qui ont causé cette mise à faux *)
	let rec explore_literals c =
		match c with
		| [] ->
			explore_clauses origins.a.(start) ;	(* On explore les clauses à l'origine de l'apprentisage de c *)
			origins.a.(start) <- [-1]		(* On désactive la clause *)
		| x::c2 when x*solution.(abs x) < 0 ->
			explore_literals c2 ;
			print_l buffer origins ((abs solution.(abs x)) - 2) clauses solution l
		| _::c2 -> explore_literals c2
	in
	
	(* Si la clause n'a pas déjà été visitée et si elle était dans la CNF initiale, on la rajoute au fichier *)
	if start < l && origins.a.(start) <> [-1] then
		fprintf buffer "%s" (Cnf.string_of_clause clauses.a.(start)) ;

	(* Si la clause n'a pas déjà été visitée, on l'explore *)
	if origins.a.(start) <> [-1] then
		explore_literals clauses.a.(start)




		(** ARBRE DE PREUVE **)


(* Convertit une clause c en une expression mathématique LaTeX *)
let rec to_tex c =
	let rec aux c =
		match c with
		| [] -> ""
		| n::c2 when n > 0 -> sprintf "\\lor x_{%d} %s" n (aux c2)
		| n::c2 -> sprintf "\\lor \\lnot x_{%d} %s" (-n) (aux c2)
	in
	if c = [] then
		"$\\bot$"
	else if List.hd c > 0 then
		sprintf "$x_{%d} %s$" (List.hd c) (aux (List.tl c))
	else
		sprintf "$\\lnot x_{%d} %s$" (abs (List.hd c)) (aux (List.tl c))



(* Construit l'arbre de dérivation LaTeX de chacune des clauses dont on aura besoin dans la preuve *)
let rec tree buffer to_build origins clauses seen solution l =
	if to_build != [] then
		begin
		let i = List.hd to_build in
		let plus = ref [] in
		if not seen.(i) && i >= l then
			begin
			fprintf buffer "%s" "\\begin{prooftree}\n\\rootAtTop\n" ;
			let rest = ref (List.rev origins.a.(i)) in
			let c = ref clauses.a.(List.hd !rest) in
			plus := (List.hd !rest) :: !plus ;
			fprintf buffer "\\AxiomC{%s}\n" (to_tex !c) ;
			rest := List.tl !rest ;
			while !rest <> [] do
				c := Cnf.resol !c clauses.a.(List.hd !rest) ;
				plus := (List.hd !rest) :: !plus ;
				fprintf buffer "\\AxiomC{%s}\n\\BinaryInfC{%s}\n" (to_tex clauses.a.(List.hd !rest)) (to_tex !c) ;
				rest := List.tl !rest ;
			done ;
			fprintf buffer "\\end{prooftree}\n" ;
			seen.(i) <- true
			end ;
		tree buffer (!plus@(List.tl to_build)) origins clauses seen solution l
		end


(* Construit la preuve *)
let prove_unsat buffer set_unsat c_start clauses =
	fprintf buffer "%s" "\\begin{prooftree}\n\\rootAtTop\n" ;
	let c = ref c_start in
	fprintf buffer "\\AxiomC{%s}\n" (to_tex !c) ;
	(* On fait des résolutions tant que la clause n'est pas vide *)
	while (!c <> []) do
		let x = List.hd !c in
		let i = ref 0 in
		while (not (List.mem (-x) clauses.a.(set_unsat.(!i)))) do
			incr i
		done ;
		c := Cnf.resol !c clauses.a.(set_unsat.(!i)) ;
		fprintf buffer "\\AxiomC{%s}\n\\BinaryInfC{%s}\n" (to_tex clauses.a.(set_unsat.(!i))) (to_tex !c) ;
	done ;
	fprintf buffer "\\end{prooftree}\n"


(* Construit les arbres LaTeX *)
let build_trees buffer origins start clauses solution l =
	
	let to_build = ref [] in
	(* Explore tous les littéraux mis à faux dans la clause c, et les clauses qui ont causé cette mise à faux *)
	let rec explore_literals c i =
		match c with
		| [] -> to_build := i :: !to_build
		| x::c2 when x*solution.(abs x) < 0 ->
			explore_literals c2 i ;
			explore_literals clauses.a.((abs solution.(abs x)) - 2) ((abs solution.(abs x)) - 2)
		| _::c2 -> explore_literals c2 i
	in
	let seen = Array.make clauses.length false in
	explore_literals clauses.a.(start) start ;
	tree buffer ((clauses.length-1) :: !to_build) origins clauses seen solution l ;
	
	(* Explore tous les littéraux mis à faux dans la clause c, et renvoie la liste des clauses qui ont causé ces mises à faux *)
	let rec explore_literals_uni c =
		match c with
		| [] -> []
		| x::c2 when x*solution.(abs x) < 0 ->
			((abs solution.(abs x)) - 2)
				:: (explore_literals_uni clauses.a.((abs solution.(abs x)) - 2))
				@ (explore_literals_uni c2)
		| _::c2 -> explore_literals_uni c2
	in
	let set_unsat = (clauses.length-1) :: origins.a.(clauses.length-1) @ (explore_literals_uni clauses.a.(start)) in
	prove_unsat buffer (Array.of_list set_unsat) clauses.a.(clauses.length-1) clauses
	





		(** BONUS 2 **)


(* Place dans le fichier unsat.cnf un sous-ensemble de clauses de la CNF étudiée suffisant à dériver une contradiction.
   l : taille de la CNF initialement étudiée, avant le clause learning.                                                 *)
let create_unsat origins clauses solution l =
	
	(* Arbre de preuve *)
	let buffer = open_out "unsat.tex" in
	fprintf buffer "\\documentclass{article}\n\\usepackage[utf8]{inputenc}\n\\usepackage[T1]{fontenc}\n\\usepackage[english]{babel}\n\\usepackage{lscape}\n\\usepackage{bussproofs}\n\\begin{document}\n\\begin{landscape}\n" ;
	build_trees buffer origins (-solution.(0)-1) clauses solution l ;
	fprintf buffer "\\end{landscape}\n\\end{document}" ;
	close_out buffer ;
	
	(* Sous-ensemble insatisfiable *)
	let buffer = open_out "unsat.cnf" in
	fprintf buffer "p cnf 0 0\n";
	print_l buffer origins (-solution.(0)-1) clauses solution l ;
	close_out buffer
