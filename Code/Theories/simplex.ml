			(** SOLVEUR ARITHMETIQUE LINEAIRE **)
				(** Type : Theory **)

open Num

open General
open Tseitin
open Simplex_aux




		(** TYPES **)

(* Les types sont décris dans simplex_aux.ml *)


type atom = Simplex_aux.atom

type stack = Simplex_aux.stack

type struc = Simplex_aux.struc





		(** LECTURE DES FICHIERS **)


let compare_tmp a b = compare a b
module Atom =
	struct
		type t = int * (num*num) * int
		let compare = compare_tmp
	end
module KeyAtom = Map.Make (Atom)
open KeyAtom
let compare = compare_tmp


(* Prétraite l'ensemble des formules, calcule bl, nbl, con_add (liste des contraintes), et associe grâce à une Map une
   variable dans DPLL(T) à chaque atome de la nouvelle formule.
   Renvoie la liste de formules dans laquelle on a remplacé les atomes par leurs variables et un tableau aa représentant
   la fonction inverse de la Map construite.                                                                             *)
let analyze form_list =
	
	(* Preprocessing, dans simplex_aux.ml *)
	let new_form_list, con_add, bl, nbl, current_max_var = analyze_init form_list in
	let cm = create_cm con_add (current_max_var + 1) in	(* Construction de la matrice des contraintes *)
	
	(* Attribution des variables aux atomes *)
	let m = ref empty in
	let rec aux f =
		match f with
		| Lit ((x,y,z), v) when mem (x,y,z) !m ->
			Lit (find (x,y,z) !m, v)
		| Lit ((x,y,z), v) ->
			m := add (x,y,z) (cardinal !m + 1) !m ;
			Lit (cardinal !m, v)
		| And (f1, f2, v) ->
			And (aux f1, aux f2, v)
		| Or (f1, f2, v) ->
			Or (aux f1, aux f2, v)
		| Not (f, v) ->
			Not (aux f, v)
	in
	let f0 = List.map aux new_form_list in
	let aa = Array.make (cardinal !m + 1) (Leq (0, (num_of_int 0,num_of_int 0))) in
	while !m <> empty do
		let (x,r,k), i = choose !m in
		m := remove (x,r,k) !m ;
		match k with
		  | 1 | 3 -> aa.(i) <- Leq (x,r)
		  | _ -> aa.(i) <- Geq (x,r)	(* z = 2 ou 4 *)
	done ;
	
	f0, aa, cm, bl, nbl, current_max_var



(* Lit un fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)

(* Lit une chaîne de caractères *)
let lexstr s =
	Lexing.from_string s

(* Parse un fichier contenant des formules logiques quelconques *)
let parse file =
	Parser_simplex.formula Lexer_simplex.token (lexbuf file)

(* Parse une CNF dans une chaîne de caractères *)
let parse_cnf s =
	Parser_cnf.cnf Lexer_cnf.token (lexstr s)

(* Initialise le solveur *)
let create file aff_cnf =
	try
		
		let f = parse file in
		let f0, aa, cm, bl, nbl, current_max_var = analyze f in	(* Preprocessing *)
		let size = current_max_var + 1 in
		
		let solver = {
			aa = aa ;
			ba = Array.make size ((num_of_string "-1000000000000000000000000000000", num_of_int 0),
						(num_of_string "1000000000000000000000000000000", num_of_int 0)) ;
			fv = nbl ;
			inst = Array.make size (num_of_int 0, num_of_int 0) ;
			cm = cm ;
			size = size ;
			nbl = nbl ;
			bl = bl ;
			st = [] ;
			unsat = []
		} in
		
		let s = conv_tseitin f0 (Array.length aa) in
		if aff_cnf then
			begin
			print_string "CNF produite :\n" ;
			print_string s ;
			print_newline ()
			end ;
		let cnf = parse_cnf s in
		
		cnf, solver
		
	with _ -> failwith "Erreur de saisie"





		(** UPDATE **)


(* Implementation de update (nom de la méthode dans l'article) *)
let update_alg i v struc=
	match struc.bl with
	  | h::t -> struc.inst.(h) <- struc.inst.(h) +@ (v -@ (struc.cm.(h).(i) *@ struc.inst.(i)))
	  | [] -> ()
	;
	struc.inst.(i) <- v


(* Change la borne sup d'une variable *)
let assertupper i c struc lit =
	if c <@ snd struc.ba.(i) then
		begin
			if c <@ fst struc.ba.(i) then
				struc.st <- None :: struc.st
			else
				begin
				let save = Array.copy struc.inst in
				let prev = struc.ba.(i) in
				let l = fst struc.ba.(i) in
				struc.ba.(i) <- l, c ;	
				if List.exists (fun k-> k=i) struc.nbl && struc.inst.(i) >@ c then
					update_alg i c struc ;
				struc.st <- Some ((save, (i,prev)) , Upp(i,c), lit) :: struc.st
				end
		end
	else
		struc.st <- None :: struc.st


(* Change la borne inf d'une variable *)
let assertlower i c struc lit=
	if c >@ fst struc.ba.(i) then
		begin
			if c >@ snd struc.ba.(i) then
				struc.st <- None :: struc.st
			else
				begin
				let save = Array.copy struc.inst in
				let prev = struc.ba.(i) in
				let u = snd struc.ba.(i) in
				struc.ba.(i) <- c, u ;
				if List.exists (fun k-> k=i) struc.nbl && struc.inst.(i) <@ c then
					update_alg i c struc ;
				struc.st <- Some ((save, (i,prev)), Low (i,c), lit) :: struc.st
				end
		end
	else
		struc.st <- None :: struc.st


(* Suivant la valeur du litteral, applique la bonne modification à la structure *)

let modif_pos struc lit =	
	match struc.aa.(lit) with
	| Leq (var,rat) ->
		assertupper var rat struc lit
	| Geq (var,rat) ->
		assertlower var rat struc lit
	
let modif_neg struc lit =
	match struc.aa.(-lit) with
	| Leq (var, (rat,k)) when k =/ num_of_int 0 ->
		assertlower var (rat, num_of_int (-1)) struc lit
	| Geq (var, (rat,k)) when k =/ num_of_int 0 ->
		assertupper var (rat, num_of_int 1) struc lit
	| Leq (var, (rat,k)) ->
		assertlower var (rat, num_of_int 0) struc lit
	| Geq (var, (rat,k)) ->
		assertupper var (rat,num_of_int 0) struc lit

let modif struc lit = 
	if lit > 0 then
		modif_pos struc lit
	else
		modif_neg struc lit


(* Permet de verifier si une instance du solveur satisfait le système ; fonctions auxiliaires dans simplex_aux.ml *)
let check_unsat struc lit=
	let loop = ref true in
	let sat = ref false in
	while !loop do
		check_aux loop sat struc.bl struc lit
	done ;
	!sat
	
			 
(* Met a jour la structure *)
let update struc lit =
	(* On n'agit que si lit correspond à un atome *)
	if abs lit < Array.length struc.aa then
		begin
			modif struc lit ;
			if check_unsat struc lit then
				0
			else
				-max_int
		end
	else
		0




		(** BACKTRACK **)


(* Charge l'état du solveur à l'étape précédente *)
let restore_state bounds instanc struc =
	struc.inst <- instanc ;
	struc.ba.(fst bounds) <- snd bounds


(* Annule la dernière affectation, si elle correspond à un atome *)
let backtrack struc lit =
	if abs lit < Array.length struc.aa then
		begin
		let last_change = List.hd struc.st in
		struc.st <- List.tl struc.st ;
		match last_change with
			| None -> ()
			| Some((i,b), _, _) ->
				restore_state b i struc
		end




		(** INSATISFIABILITE **)


(* Donne une explication de l'insatisfiabilite*)
let unsat struc =
	struc.unsat




		(** AFFICHAGE DE LA SOLUTION **)


(* Trouve une valeur acceptable pour le delta *)
let compute_delta struc =
	let delta = ref (num_of_string "1/5") in
	let kmax = ref (num_of_int 0) in		(* = max { k | inst.(i) = _, k } *)
	for i = 1 to struc.size - 1 do
		let tmp = ((abs_num (fst (fst struc.ba.(i)))) +/ (abs_num (fst (snd struc.ba.(i))))) // (num_of_int 2) in
		if tmp <>/ num_of_int 0 then
			delta := min_num !delta tmp ;
		kmax := max_num !kmax (max_num (snd (fst struc.ba.(i))) (snd (snd struc.ba.(i))))
	done ;
	(!delta */ !kmax) // (num_of_int 2)


(* Affiche l'instanciation des variables se trouvant dans fv *)
let rec print_inst fv struc delta =
	match fv with
	| [] -> ()
	| h::t ->
		Printf.printf "x%d = %s\n" h (string_of_num ((fst struc.inst.(h)) +/ ((snd struc.inst.(h)) */ delta))) ;
		print_inst t struc delta


(* Affiche une instanciation *)
let print_solution res solver =
	match res with
	| Cnf.False ->
		print_string "s UNSATISFIABLE\n"
	| Cnf.True _ ->
		print_string "s SATISFIABLE\n" ;
		print_inst solver.fv solver (compute_delta solver)
