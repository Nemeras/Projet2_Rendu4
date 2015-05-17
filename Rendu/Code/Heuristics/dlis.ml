			(** HEURISTIQUE DLIS **)
			(** Type : Heuristic **)


(* Permet de choisir le prochain pari comme le littéral le plus présent dans l'ensemble de clauses, ie qui en satisfait le plus *)


(* Ces commentaires complètent ceux faits dans types.ml *)


open General
open DynArray
open Cnf



let heuristic = 3


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

(* Dans le cas des watched literals, pos ne contient d'informations que pour les littéraux surveillés ; on doit donc
   parcourir le tableau current des clauses actives pour déterminer quel littéral apparaît dans le plus de clauses.  *)
let next_wl pos current h =
	let n = Array.length pos - 1 in
	let pos_real = Array.make (n+1) (0,0) in
	let rec aux c =
		match c with
		| [] -> ()
		| x::c2 when x > 0 ->
			pos_real.(x) <- 1 + fst pos_real.(x), snd pos_real.(x) ;
			aux c2
		| x::c2 ->
			pos_real.(-x) <- fst pos_real.(-x), 1 + snd pos_real.(-x) ;
			aux c2
	in
	for i = 0 to current.length - 1 do
		aux current.a.(i)
	done ;
	let lit_max = ref 0 in
	let maxi = ref 0 in
	for i = 1 to n do
		if not h.taken.(i) && fst pos_real.(i) > !maxi then
			begin
			maxi := fst pos_real.(i) ;
			lit_max := i
			end ;
		if not h.taken.(i) && snd pos_real.(i) > !maxi then
			begin
			maxi := snd pos_real.(i) ;
			lit_max := -i
			end
	done ;
	!lit_max

(* Lorsque l'on ne manipule pas les littéraux surveillés, pos permet de savoir pour chaque littéral dans quelle clause active
   il apparaît : son score est donc la longueur de cette liste.                                                               *)
let next_basic pos h =
	let n = Array.length pos - 1 in
	let new_pos = Array.map (fun (l1,l2) -> (List.length l1, List.length l2)) pos in
	let lit_max = ref 0 in
	let maxi = ref 0 in
	for i = 1 to n do
		if not h.taken.(i) && fst new_pos.(i) > !maxi then
			begin
			maxi := fst new_pos.(i) ;
			lit_max := i
			end ;
		if not h.taken.(i) && snd new_pos.(i) > !maxi then
			begin
			maxi := snd new_pos.(i) ;
			lit_max := -i
			end
	done ;
	!lit_max

(* Renvoie le prochain pari à faire ; appelle une fonction différente selon si on manipule des littéraux surveillés ou non *)
let next h pos current wl =
	let lit_max =
		if wl then
			next_wl pos current h
		else
			next_basic pos h
	in
	if lit_max <> 0 then
		lit_max
	else
		(* Lorsque toutes les clauses ont été satisfaites, on choisit la première variable libre *)
		next_free h
	

let is_instanciation_full h =
	h.nbr = 0
