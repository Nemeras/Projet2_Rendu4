			(** IMPLEMENTATION DE LA TRANSFORMATION DE TSEITIN **)


open Printf


(* Formule dont les atomes sont de type 'a
   Chaque noeud est accompagné d'un entier, représentant la variable qui y est associée *)
type 'a formula =
	| Lit of 'a * int
	| And of ('a formula) * ('a formula) * int
	| Or of ('a formula) * ('a formula) * int
	| Not of ('a formula) * int

type 'a form = 'a formula * int

type 'a formlist = ('a formula list) * int


(* Assigne à chaque formule une variable *)
let assign formula debut =	
	let numerotage = ref debut in
	let rec num form nvar =
		match form with
		| Lit (a,b) ->
			incr nvar ;
			let tmp = !nvar in
			Lit (a, tmp)
		| And (a,b,c) ->
			incr nvar ;
			let tmp = !nvar in
			And (num a nvar, num b nvar, tmp)
		| Or (a,b,c) ->
			incr nvar ;
			let tmp = !nvar in
			Or (num a nvar, num b nvar, tmp)
		| Not (a,b) ->
			incr nvar ;
			let tmp = !nvar in
			Not(num a nvar, tmp)
	in
 	let rec num_aux liste nvar =
		match liste with
		| tree::other ->
			(num tree nvar)::(num_aux other nvar)
		| [] -> []
	in
	num_aux formula numerotage


(* Renvoie la variable associée à la formule form *)
let var form =
	match form with
	| Lit (_, v) -> v
	| And (_, _, v) -> v
	| Or (_, _, v) -> v
	| Not (_, v) -> v


(* Transformation sur form *)
let rec print_form form buffer =
	match form with
	| Lit (f, v) ->
		bprintf buffer "-%d %d 0\n%d -%d 0\n" f v f v
	| And (f1, f2, v) ->
		bprintf buffer "-%d %d 0\n-%d %d 0\n%d -%d -%d 0\n" v (var f1) v (var f2) v (var f1) (var f2) ;
		print_form f1 buffer ;
		print_form f2 buffer
	| Or (f1, f2, v) ->
		bprintf buffer "-%d %d %d 0\n%d -%d 0\n%d -%d 0\n" v (var f1) (var f2) v (var f1) v (var f2) ;
		print_form f1 buffer ;
		print_form f2 buffer
	| Not (Lit (a, _), v) ->
		bprintf buffer "-%d -%d 0\n%d %d 0\n" v a v a
	| Not (f, v) ->
		bprintf buffer "-%d -%d 0\n%d %d 0\n" (var f) v (var f) v ;
		print_form f buffer


(* Applique la tarnsformation à chaque formule *)
let rec conv_list formula_list buffer=
	match formula_list with
	| tree::other ->
		bprintf buffer "%d 0\n" (var tree) ;
		print_form tree buffer ;
		conv_list other buffer
	| [] -> ()


(* Renvoie une chaîne de caractères représentant la CNF associée aux formules de formula_list *)
let conv_tseitin formula_list m =
	let buffer = Buffer.create 1 in
	bprintf buffer "p cnf 0 0\n" ;
	conv_list (assign formula_list m) buffer ;
	Buffer.contents buffer
