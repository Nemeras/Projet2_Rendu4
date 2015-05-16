			(** THEORIE DE L'EGALITE **)
			    (** Type : Theory **)

open General

open Tseitin
open List

		(** TYPES UTILISES **)


type atom =	(* Egalité ou diségalité de deux variables *)
	| Eq of int*int
	| Ineq of int*int
  
type change =
	| AddIneq of int*int*int	(* Littéral correspondant, x1, x2 où l'instanciation donne x1 != x2 *)
	| AddEq of int*int*int*int*int	(* Littéral, x1, x2, y1, y2 où *)

type uf = (int*int) array	(* Structure d'union find : (père, taille de l'arbre) *)
		  
type ineqs = (int*int) list	(* Liste des inégalités *)

type stack = change list	(* Pile des modifications au fil des instanciations dans DPLL *)
  
  
type struc = {
	aa : atom array ;		(* Associe à une variable l'atome correspondant *)
	eq : uf ;
	mutable ineq : ineqs ;
	mutable st : stack ;
	mutable unsat : Cnf.clause	(* Clause expliquant la contradiction levée par le solveur *)
}
	   



		(** LECTURE DES FICHIERS **)


module Atom =
	struct
		type t = int * int
		let compare = compare
	end

module KeyAtom = Map.Make (Atom)

(* Map prenant en entrée des couples de variables (égalités / diségalités) et leur associant une variable dans DPLL *)
open KeyAtom


(* Construit la Map associant à chaque atome sa variable dans DPLL.
   Renvoie la liste de formules dans laquelle on a remplacé les atomes par leurs variables et un tableau représentant
   la fonction inverse de la Map construite.                                                                          *)
let analyze form_list =
	let m = ref empty in
	let rec aux f =
		match f with
		| Lit ((x,y), v) when mem (min x y, max x y) !m ->
			Lit (find (min x y, max x y) !m, v)
		| Lit ((x,y), v) ->
			m := add (min x y, max x y) (cardinal !m + 1) !m ;
			Lit (cardinal !m, v)
		| And (f1, f2, v) ->
			And (aux f1, aux f2, v)
		| Or (f1, f2, v) ->
			Or (aux f1, aux f2, v)
		| Not (f, v) ->
			Not (aux f, v)
	in
	let f0 = List.map aux form_list in
	let aa = Array.make (cardinal !m + 1) (Eq (0,0)) in
	while !m <> empty do
		let (x,y), i = choose !m in
		m := remove (x,y) !m ;
		if x >= 0 then
			aa.(i) <- Eq (x,y)
		else
			aa.(i) <- Ineq (-x-1,-y-1)
	done ;
	f0, aa


(* Renvoie l'indice de variable (dans les atomes) maximal *)
let max_var aa =
	let maxv = ref 0 in 
	for i=0 to Array.length aa - 1 do
		match aa.(i) with
		| Eq (x,y) -> maxv := max !maxv (max x y) ;
		| Ineq (x,y) -> maxv := max !maxv (max x y) ;
	done ;
	!maxv


(* Lit un fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)


(* Lit une chaîne de caractères *)
let lexstr s =
	Lexing.from_string s


(* Parse un fichier contenant des formules logiques quelconques *)
let parse file =
	Parser_equ.formula Lexer_equ.token (lexbuf file)


(* Parse une CNF dans une chaîne de caractères *)
let parse_cnf s =
	Parser_cnf.cnf Lexer_cnf.token (lexstr s)


let create file aff_cnf =
	try
		let f = parse file in
		let f0, aa = analyze f in
		let solver = {
			aa = aa ;
			eq = Array.init (1 + max_var aa) (fun i -> (i,1)) ;
			ineq = [] ;
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
	with _ -> (failwith "Erreur de saisie")




		(** MANIPULATION DE L'UNION-FIND **)


(* Structure d'union-find classique, avec pondération par la taille des arbres.
   On n'a pas fait d'opérations de compression, pour préserver la structure et pouvoir faire des backtracks. *)

let rec find n aa =
	match fst (aa.(n)) with
	| x when x = n -> x
	| x -> find x aa


let union x y aa lit =
	match aa.(find x aa), aa.(find y aa) with
	| (a,_), (b,_) when a = b -> AddEq (lit,a,b,a,b)
	| (a,sza), (b,szb) when sza > szb ->
		aa.(b) <- (a, szb) ;
		aa.(a) <- (a, sza+szb) ;
		AddEq (lit,x,y,b,a)
	| (a,sza), (b,szb) ->
		aa.(a) <- (b, sza) ;
		aa.(b) <- (b, sza+szb) ;
		AddEq (lit,x,y,a,b)




		(** UPDATE **)


(* Choisit la modification à apporter à la structure selon la nature de l'instanciation *)
let modif struc lit =
	match struc.aa.(abs lit) with
	| Eq (x,y) when lit > 0 ->
		union x y struc.eq lit
	| Eq (x,y) ->
		struc.ineq <- (x,y)::struc.ineq ;
		AddIneq (lit, x, y)
	| Ineq (x,y) when lit > 0 ->
		struc.ineq <- (x, y)::struc.ineq ;
		AddIneq (lit, x, y)
	| Ineq (x,y) ->
		union x y struc.eq lit


(* S'il n'y a pas de contradiction, renvoie (0,0).
   Sinon, renvoie (x,y), où x != y est l'inégalité qui vient contredire une égalité *)
let check_unsat struc =
	let rec aux l aa =
		match l with
		| (x,y)::_ when find x aa = find y aa -> (x,y)
		| (x,y)::t -> aux t aa
		| [] -> (0,0)
	in
	aux struc.ineq struc.eq


(* Recherche le littéral responsable de l'affectation correspondant à x != y *)
let rec search_i st x y =
	match st with
	| AddEq _ :: t -> search_i t x y
	| AddIneq (lit, a, b) :: t when a = x && b = y -> lit
	| AddIneq _ ::t -> search_i t x y
	| [] -> failwith "Problème dans search_i"


(* Trouve un chemin dans le graphe représenté par les listes d'adjacence adj de start vers end_e *)
let path adj start end_e =
	let liste_visit = ref [start] in
	let current = ref (start,0) in
	let prev = ref [0,0] in
	while fst !current != end_e do 
		if adj.(fst !current) = [] then
			begin
			if !prev = [0,0] then
				failwith "Pas de chemin" ;
			current := hd !prev ;
			prev := tl !prev ;
			adj.(fst !current) <- tl adj.(fst !current)
			end
		else
			begin
			if List.exists (fun i -> i = fst (hd adj.(fst !current))) !liste_visit then
	  			begin
	   			adj.(fst !current) <- tl adj.(fst !current)
	 			end
			else
	 			begin
				prev := !current :: !prev ;
				current := hd adj.(fst !current) ;
				liste_visit := (fst !current) :: !liste_visit
				end
			end
	done ;
	tl (tl (rev (!current :: !prev)))


(* Construit le graphe des égalités données par DPLL, dans les listes d'adjacence du tableau adj *)
let rec build_adj stack adj =
	match stack with
	| AddIneq _ ::t -> build_adj t adj
	| AddEq (lit,x,y,a,b) ::t ->
		adj.(x) <- (y,lit)::adj.(x) ;
		adj.(y) <- (x,lit)::adj.(y) ;
		build_adj t adj
	| [] -> ()


(* Prend en compte l'instanciation de lit à vrai dans la structure *)
let update struc lit =
	(* On n'agit que si lit correspond à un atome *)
	if abs lit < Array.length struc.aa then
		begin
		struc.st <- (modif struc lit)::struc.st ;	(* Modification de la structure *)
		let check = check_unsat struc in
		if check = (0,0) then
			0
		else
			(* Construction de la clause expliquant la contradiction *)
			begin
			let x, y = check in
			let lit = search_i struc.st x y in
			(* Construction du graphe des égalités *)
			let adj = Array.make (Array.length struc.eq) [] in
			build_adj struc.st adj ;
			(* Chemin d'égalités de x vers y *)
			let list_edges = path adj x y in
			let clause = List.map (fun (a,b) -> -b) list_edges in
			(* Or, on a voulu supposer x != y, d'où l'explication de la contradiction *)
			struc.unsat <- -lit::clause ;
			-max_int
			end
		end
	else
		0



		(** BACKTRACK **)


(* Annule la dernière affectation, si elle correspond à un atome *)
let backtrack struc lit =
	if abs lit < Array.length struc.aa then
		begin
		let last_change = hd struc.st in
		struc.st <- tl struc.st ;
		match last_change with
		| AddEq (lit,x,y,rx,ry) when rx = ry -> ()
		| AddEq (lit,x,y,rx,ry) ->
			let a,sz = struc.eq.(rx) in
			struc.eq.(rx) <- rx, sz ;
			let b,szb = struc.eq.(ry) in 
			struc.eq.(ry) <- ry, szb-sz
		| AddIneq (_) ->
			struc.ineq <- tl struc.ineq
		end




		(** INSATISFIABILITE **)


let unsat struc =
	struc.unsat




		(** AFFICHAGE DE LA SOLUTION **)


(* Trouve une instanciation pour la i-ème variable *)
let color instanciation solver i =
	let rec color_aux i =
		match solver.eq.(i) with
		| j, _ when j = i || instanciation.(i) != i ->
			(* Si la variable est son propre représentant ou a déjà été instanciée, on garde i *)
			instanciation.(i)
		| j, _ ->
			(* Sinon, on cherche l'instanciation de son représentant *)
			let c = color_aux j in
			instanciation.(i) <- c ;
			solver.eq.(i) <- i, 0 ;
			c
	in
	let _ = color_aux i in
	()


(* Si l'ensemble de formules est satisfiables, renvoie une affectation des variables dans N correspondante *)
let find_instanciation solver =
	let l = Array.length solver.eq in
	let instanciation = Array.init l (fun i -> i) in
	for i = 0 to l - 1 do
		(* Il suffit de parcourir les variables et de faire en sorte qu'un classe d'équivalence soit instanciée de la même façon *)
		color instanciation solver i ;
		Printf.printf "x%d = %d\n" i instanciation.(i)
	done
	


let print_solution res solver =
	match res with
	| Cnf.False ->
		print_string "s UNSATISFIABLE\n"
	| Cnf.True _ ->
		print_string "s SATISFIABLE\n" ;
		find_instanciation solver
