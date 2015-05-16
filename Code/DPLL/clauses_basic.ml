			(** IMPLEMENTE LA GESTION DES CLAUSES "SIMPLES" **)
					(** Type : Clauses **)

open General

open Cnf
open DynArray
open Dot

(* On ouvre Dot et DynArray pour pouvoir utiliser sans avoir de Warning les types enregistrement qui y sont définis *)


(* Les commentaires de cette section complètent les commentaires faits dans la decription du type de module Clauses dans types.ml *)


let wl = false


		(** TYPES / FONCTIONS DE BASE **)


(* On a besoin de connaître :
	- L'état courant de la clause (2e composante)
	- Si la clause est satisfaite (1ère composante)
	- La liste des littéraux mis à faux dans l'ordre chronologique inversé (3e composante) *)
type t = bool * clause * (int list)


(* Chaque étage de la pile contient le littéral mis à vrai, et la liste des clauses mises à vrai lors de cette instanciation *)
type stack = (int * (int list)) list ref


let init_stack () =
	ref [(0,[])]


let pick (stack:stack) =
	fst (List.hd !stack)


let init_value c =
	false, c, []




		(** GESTION DE POS **)


(* Réactualise pos lorsque c est activée = indéterminée. *)
let activate c pos i =
	let rec aux c =
		match c with
		| [] -> ()
		| x::q when x > 0 ->
			pos.(x) <- i::(fst pos.(x)), snd pos.(x) ;
			aux q
		| x::q ->
			pos.(-x) <- fst pos.(-x), i::(snd pos.(-x)) ;
			aux q
	in
	aux c


(* Réactualise pos lorsque c est désactivée = vraie. *)
let inactivate c pos i =
	let rec aux c =
		match c with
		| [] -> ()
		| x::q when x > 0 ->
			pos.(x) <- List.filter (fun y -> y <> i) (fst pos.(x)), snd pos.(x) ;
			aux q
		| x::q ->
			pos.(-x) <- fst pos.(-x), List.filter (fun y -> y <> i) (snd pos.(-x)) ;
			aux q
	in
	aux c




		(** DETECTION DE CLAUSES UNITAIRES **)


(* Indique si la clause d'indice i dans current est unitaire *)
let is_unit current i =
	let b, c, _ = current.a.(i) in
	not b && c <> [] && List.tl c = []


let units current solution uni =
	if solution.(0) = 0 then
		for i = 0 to current.length - 1 do
			if is_unit current i then
				begin
				let _, c, _ = current.a.(i) in
				uni := (List.hd c, i) :: !uni
				end
		done




		(** UPDATE / PUSH **)


(* Supprime les littéraux mis à faux par l'affectation encours dans les clauses correspondantes. *)
let rec update_remove n current solution list_pos uni =
	match list_pos with
	| [] -> !uni
	| h::t ->
		let boole, c, c2 = current.a.(h) in
		let new_c = List.filter (fun i -> i <> n) c in
		current.a.(h) <- boole, new_c, n::c2 ;
		if new_c = [] then
			solution.(0) <- -h-1 ;
		if is_unit current h then
			begin
			let _, c, _ = current.a.(h) in
			uni := (List.hd c, h) :: !uni
			end ;
		update_remove n current solution t uni


(* Désactive les clauses mises à vrai par l'affectation en cours. *)
let rec update_inactivate n current pos list_pos =
	match list_pos with
	| [] -> [] ;
	| h::t ->
		let boole, c, c2 = current.a.(h) in
		if not boole then
			begin
			current.a.(h) <- true, c, c2 ;
			inactivate c pos h
			end
		;
		h::(update_inactivate n current pos t)


(* Place l'affectation n = vrai au début de la pile, et met à jour current et pos.
	list_pos_negative : liste des positions dans current des clauses contenant le littéral -n.
	list_pos : liste des positions dans current des clauses contenant le littéral n.           *)
let update n stack current pos solution =
	let list_pos_negative, list_pos =
		if n > 0 then
			snd pos.(n), fst pos.(n)
		else
			fst pos.(-n), snd pos.(-n)
	in
	let uni = ref [] in
	let changes = update_inactivate n current pos list_pos in
	stack := (n, changes) :: !stack ;
	update_remove (-n) current solution list_pos_negative uni




		(** BACKTRACK / POP **)


(* Réactive les clauses qui avaient été désactivées par l'affectation annulée. *)
let rec backtrack_activate n changes current pos =
	match changes with
	| [] -> ()
	| h::t ->
		let _, c, c2 = current.a.(h) in
		current.a.(h) <- false, c, c2 ;
		activate c pos h ;
		backtrack_activate n t current pos


(* Enlève les littéraux mis à faux au niveau de décision level dans c2 *)
let rec aux_restore c2 levels level =
	match c2 with
	| [] -> []
	| n::_ when levels.(abs n) < level -> c2
	| n::q ->
		aux_restore q levels level


(* Restaure les littéraux qui avaient été supprimés par l'affectation annulée. *)
let rec backtrack_restore n to_restore current levels level =
	match to_restore with
	| [] -> ()
	| h::tail ->
		let boole, c, c2 = current.a.(h) in
		let new_c2 = aux_restore c2 levels level in
		current.a.(h) <- boole, n::c, new_c2 ;
		backtrack_restore n tail current levels level


(* Annule l'affectation en tête de liste, la renvoie, et met à jour current et pos. *)
let backtrack stack current pos levels =
	let content = !stack in
	match content with
	| [] -> failwith "Pile vide"
	| (n, changes)::tail ->
		let level = levels.(abs n) in
		let to_restore =
			if n > 0 then
				snd pos.(n)
			else
				fst pos.(-n)
		in
		backtrack_activate n changes current pos ;
		stack := tail ;
		backtrack_restore (-n) to_restore current levels level ;
		n




		(** MANIPULATION DE LA PILE AVEC CLAUSE LEARNING **)


(* Met à jour pos avec les littéraux de clause, et constuit list_false en respectant l'ordre invers des affectations *)
let rec separate_aux clause list_false v pos pos_c=
	if List.mem (-v) !clause then 
		begin
		if v > 0 then
			pos.(v) <- fst pos.(v), pos_c::(snd pos.(v))
		else
			pos.(-v) <- pos_c::(fst pos.(-v)), snd pos.(-v)
		;
		list_false:= -v :: !list_false ;
		clause := List.filter (fun i -> i <> -v) (!clause)
		end


(* Remonte la pile et fait toutes les opérations correspondantes sur clause et pos *)
let rec separate stack clause list_false pos pos_c =
	match stack with
	| [] -> ()
	| (v, _)::tail ->
		separate_aux clause list_false v pos pos_c ;
		separate tail clause list_false pos pos_c



(* Traite la clause "clause" comme si elle avait été présente depuis le début de l'exécution dans current *)
let maj_cl stack clause pos levels pos_c =
	let stack_rev = List.rev !stack in	(* On va remonter toute la pile d'affectations *)
	let new_clause = ref clause in	
	let list_false = ref [] in		(* Liste ordonnée des littéraux faux de !clause *)
	separate stack_rev new_clause list_false pos pos_c ;
	false, [], !list_false




		(** CONSTRUCTION DU GRAPHE DU CONFLIT **)


(* Construit l'ensemble des arêtes du graphe, et place les couleurs bleu et blanc.
	start : noeud duquel on provient.
	seen : indique, pour chaque variable, si le noeud qui lui correspond a été traité ;
		permet d'éviter des doublons d'arêtes.
	v : nombre total de variables.                                                     *)
let rec edges pos_c graph current solution levels orders seen start level v =
	
	(* Parcourt tous les littéraux de l, qui sont "blancs". *)
	let rec explore_white l start =
		match l with
		| [] -> ()
		| n::q ->
			if not seen.(abs start) then
				begin
				Dot.add_edge graph (-n, start) ;
				Dot.set_color graph (-n) White v ;
				explore_white q start
				end
	in
	
	(* On parcourt les littéraux mis à faux de la clause pos_c *)
	match current.a.(pos_c) with

	| a, b, n::q when levels.(abs n) = level ->	(* Tant qu'on est sur le niveau de décision courant *)
		(* Ajout de -n au graphe *)
		Dot.add_edge graph (-n, start) ;
		Dot.set_color graph (-n) Blue v ;
		current.a.(pos_c) <- a,b,q ;
		
		(* Si -n n'est pas un pari, on peut continuer à explorer les littéraux qui ont causé la mise à faux de n *)
		if abs solution.(abs n) > 1 then
			edges ((abs solution.(abs n)) - 2) graph current solution levels orders seen (-n) level v ;
		edges pos_c graph current solution levels orders seen start level v

	| a, b, q ->				(* Tous les autres littéraux sont "blancs" *)
		explore_white q start ;
		seen.(abs start) <- true




		(** FONCTIONS UTILISÉES DANS L'AFFICHAGE **)


let print_clause (b,c,l) solution =
	print_string (string_of_clause c)


let is_clause_true (b,c,l) solution =
	b




		(** HEURISTIQUES **)


(* Renvoie l'état courant de la CNF en éliminant les littéraux faux et les clauses satisfaites.
   Ne s'exécute qui si MOMS est activé.                                                         *)
let current_clauses current solution h =
	let res = DynArray.make 0 [] in
	if h = 2 then
		for i = 0 to current.length - 1 do
			let b, c, _ = current.a.(i) in
			if not b then
				DynArray.add res c []
		done
	;
	res
