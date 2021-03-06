			(** IMPLEMENTE LA GESTION DES CLAUSES "SIMPLES" **)
					(** Type : Clauses **)

open General

open Cnf
open Watched
open DynArray
open Dot

(* On ouvre Dot et DynArray pour pouvoir utiliser sans avoir de Warning les types enregistrement qui y sont définis *)


(* Les commentaires de cette section complètent les commentaires faits dans la decription du type de module Clauses dans types.ml *)


let wl = true


		(** TYPES / FONCTIONS DE BASE **)


(* Les clauses sur lesquelles on travaille sont des listes de littéraux
   Les deux premiers littéraux sont les littéraux surveillés, et les littéraux mis à faux dans une clauses
   sont tous en fin de liste, ce qui permet des opérations de changement de littéraux surveillés rapides   *)
type t = clause


(* On ne retient dans la pile que le littéral qui a été mis à vrai *)
type stack = int list ref


let init_stack () =
	ref [0]


let pick stack =
	List.hd !stack


let init_value c =
	c


		(** GESTION DE POS **)


(* Rajoute la clause d'indice i dans la liste correspondant au littéral x *)
let add_pos pos x i =
	if x > 0 then
		pos.(x) <- i::(fst pos.(x)), snd pos.(x)
	else
		pos.(-x) <- fst pos.(-x), i::(snd pos.(-x))


(* Supprime la clause d'indice i dans la liste correspondant au littéral x *)
let suppr_pos pos x i =
	if x > 0 then
		pos.(x) <- List.filter (fun y -> y <> i) (fst pos.(x)), snd pos.(x)
	else
		pos.(-x) <- fst pos.(-x), List.filter (fun y -> y <> i) (snd pos.(-x))


(* Actualise pos en prenant en compte la clause c *)
let activate c pos i =
	match c with
	| x::y::_ ->
		add_pos pos x i ;
		add_pos pos y i
	| [x] ->
		add_pos pos x i
	| _ -> ()




		(** DETECTION DE CLAUSES UNITAIRES **)


let units current solution uni =
	if solution.(0) = 0 then
		for i = 0 to current.length - 1 do
			if Watched.is_unit current.a.(i) solution then
				uni := (List.hd current.a.(i), i) :: !uni
		done




			(** UPDATE / PUSH **)


(* Parcourt la liste des clauses à examiner et change leurs littéraux surveillés *)
let rec update_aux to_change current pos solution absurd uni =
	match to_change with
	| [] -> !uni
	| _ when !absurd -> !uni
	| i::q ->
		(* Si un des deux littéraux de la clause n'est pas encore à vrai *)
		if not (is_w_true current.a.(i) solution) then
			begin
			
			(* Suppression dans pos des littéraux surveillés de la clause i *)
			begin
			match current.a.(i) with
			| [] -> ()
			| [x] -> suppr_pos pos x i
			| x1::x2::_ ->
				suppr_pos pos x1 i ;
				suppr_pos pos x2 i
			end ;
			
			(* On modifie les littéraux surveillés suite à l'affectation en cours *)
			current.a.(i) <- change_clause current.a.(i) solution ;
			
			(* Mise à jour des littéraux surveillés dans pos *)
			begin
			match current.a.(i) with
			| [] -> ()
			| [x] -> add_pos pos x i
			| x1::x2::_ ->
				add_pos pos x1 i ;
				add_pos pos x2 i
			end ;
			
			(* Détection d'une conséquence *)
			if is_unit current.a.(i) solution then
				uni := (List.hd current.a.(i), i) :: !uni ;
			
			(* Si la clause est à faux, il y a contradiction *)
			is_clause_false current.a.(i) solution ;
			if solution.(0) < 0 then
				begin
				solution.(0) <- -1 - i ;
				absurd := true
				end
			end ;
		
		update_aux q current pos solution absurd uni


(* Place l'affectation n = vrai au début de la pile, renvoie la liste des conséquences (clauses unitaires) apparues. *)
let update n stack current pos solution =
	let uni = ref [] in
	stack := n::!stack ;
	
	(* Liste des clauses à examiner : clauses dans lesquelles -n apparaît *)
	let to_change =
		if n > 0 then
			snd pos.(n)
		else
			fst pos.(-n)
	in
	
	update_aux to_change current pos solution (ref false) uni









		(** BACKTRACK / POP **)


(* Annule l'affectation en tête de liste et la renvoie. *)
let backtrack stack current pos levels =
	let n = List.hd !stack in
	stack := List.tl !stack ;
	n




		(** MANIPULATION DE LA PILE AVEC CLAUSE LEARNING **)


(* Remonte la pile et fait toutes les opérations correspondantes sur clause et pos *)
let rec separate stack clause solution =
	match stack with
	| [] -> ()
	| v::tail ->
		solution.(abs v) <- v ;
		clause := change_clause !clause solution ;
		separate tail clause solution


(* Traite la clause "clause" comme si elle avait été présente depuis le début de l'exécution dans current *)
let maj_cl stack clause pos levels pos_c =
	
	let stack_rev = List.rev !stack in
	let new_clause = ref clause in
	let solution = Array.make (Array.length levels) 0 in	(* Ersatz de solution, pour les fonctions dans Watched *)
	separate stack_rev new_clause solution ;
	
	begin	(* Mise à jour de pos *)
	match !new_clause with
	| [] -> ()
	| [x] -> add_pos pos x pos_c
	| x1::x2::_ ->
		add_pos pos x1 pos_c ;
		add_pos pos x2 pos_c
	end ;
	
	!new_clause




		(** CONSTRUCTION DU GRAPHE DU CONFLIT **)


(* Construit l'ensemble des arêtes du graphe, et place les couleurs bleu et blanc.
	start : noeud duquel on provient.
	seen : indique, pour chaque variable, si le noeud qui lui correspond a été traité ;
		permet d'éviter des doublons d'arêtes.
	v : nombre total de variables.                                                      *)
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
	
	(* On parcourt les littéraux mis à faux de la clause c.
	   Tient compte de l'ordonnancement des littéraux dans current. *)
	let rec princ c start =
		match c with
		
		| n::q when n*solution.(abs n) < 0 && levels.(abs n) = level && (start = 0 || orders.(abs n) <= orders.(abs start)) ->
			(* Tant qu'on est sur le niveau de décision courant *)
			(* Ajout de -n au graphe *)
			Dot.add_edge graph (-n, start) ;
			Dot.set_color graph (-n) Blue v ;
			
			(* Si -n n'est pas un pari, on peut continuer à explorer les littéraux qui ont causé la mise à faux de n *)
			if abs solution.(abs n) > 1 then
				edges ((abs solution.(abs n)) - 2) graph current solution levels orders seen (-n) level v ;
			princ q start
		
		| n::q when n*solution.(abs n) >= 0 || levels.(abs n) = level ->
			princ q start
		
		| _ ->	(* Tous les autres littéraux sont "blancs" *)
			if start = 0 || abs solution.(abs start) > 1 then
				explore_white c start ;
			seen.(abs start) <- true
	in
	
	if not seen.(abs start) then
		princ current.a.(pos_c) start




		(** FONCTIONS UTILISÉES DANS L'AFFICHAGE **)


let rec print_clause c solution =
	let rec aux c solution =
		match c with
		| x::c2 ->
			Printf.printf "%d " x ;
			aux c2 solution
		| [] -> print_string "\t0\n"
	in
	match c with
	| h::h2::c2 ->
		Printf.printf "\t[| %d (%d)\t|\t%d (%d) |]\t"
				h (max (-2) (min 2 (h*solution.(abs h))))
				h2 (max (-2) (min 2 (h2*solution.(abs h2)))) ;
		aux c2 solution
	| _ -> aux c solution


let is_clause_true c solution =
	is_w_true c solution

















let current_clauses current solution h =
	let res = DynArray.make 0 [] in
	if h = 2 || h = 3 then
		for i = 0 to current.length - 1 do
			if not (is_clause_true current.a.(i) solution) then
				DynArray.add res (List.filter (fun x -> x*solution.(abs x) > 0) current.a.(i)) []
		done
	;
	res
