			(** HEURISTIQUE MOMS **)
			(** Type : Heuristic **)


(* Permet de choisir le prochain pari comme le littéral le plus présent dans les clauses de taille minimale *)


(* Ces commentaires complètent ceux faits dans types.ml *)


open General
open DynArray
open Cnf



let heuristic = 2


type struc = {
	taken : bool array ;	(* taken.(i) = true si la variable i est déjà instanciée *)
	mutable nbr : int	(* Nombre de variables restant à instancier *)
}


let init cnf pos =
	{
		taken = Array.make (cnf.v_real+1) false ;
		nbr = cnf.v_real
	}


let update h x =
	h.taken.(abs x) <- true ;
	h.nbr <- h.nbr - 1


let backtrack h x =
	h.taken.(abs x) <- false ;
	h.nbr <- h.nbr + 1


let learning h c =
	()



(* Choisit la prochaine variable libre *)
let next_free h =
	let i = ref 1 in
	while h.taken.(!i) do
		incr i
	done ;
	!i

(* Trouve les clauses de taille minimale de current et construit un tableau pos_min, où :
	fst pos_min.(i) est le nombre d'occurences du littéral i dans ces clauses de taille minimale,
	snd pos_min.(i) le nombre d'occurences du littéral -i dans ces clauses.
*)
let compute_min n current =
	let pos_min = Array.make (n+1) (0,0) in
	let rec aux c =
		match c with
		| [] -> ()
		| x::c2 when x > 0 ->
			pos_min.(x) <- 1 + fst pos_min.(x), snd pos_min.(x) ;
			aux c2
		| x::c2 ->
			pos_min.(-x) <- fst pos_min.(-x), 1 + snd pos_min.(-x) ;
			aux c2
	in
	let mini = ref max_int in
	for i = 0 to current.length - 1 do
		let l = List.length current.a.(i) in
		if l < !mini then
			begin
			mini := l ;
			Array.fill pos_min 0 n (0,0) ;
			end ;
		if l = !mini then
			aux current.a.(i) ;
	done ;
	pos_min

let next h pos current wl =
	let n = Array.length pos - 1 in
	let pos_min = compute_min n current in
	let lit_max = ref 0 in
	let maxi = ref 0 in
	(* Cherche le littéral le plus présent dans les clauses de taille minimale de current *)
	for i = 1 to n do
		if not h.taken.(i) && fst pos_min.(i) > !maxi then
			begin
			maxi := fst pos_min.(i) ;
			lit_max := i
			end ;
		if not h.taken.(i) && snd pos_min.(i) > !maxi then
			begin
			maxi := snd pos_min.(i) ;
			lit_max := -i
			end
	done ;
	if !lit_max <> 0 then
		!lit_max
	else
		(* Si on n'en a pas trouvé, alors toutes les clauses ont été satisfaites et on prend la première variable libre *)
		next_free h
	

let is_instanciation_full h =
	h.nbr = 0
