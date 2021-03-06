open General
open Cnf

open DynArray

type struc = {
	taken : bool array ;
	mutable nbr : int
}

let heuristic = 2

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


let next_free h =
	let i = ref 1 in
	while h.taken.(!i) do
		incr i
	done ;
	!i


let next h pos current wl =
	let n = Array.length pos - 1 in
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
	let lit_max = ref 0 in
	let maxi = ref 0 in
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
		next_free h
	

let is_instanciation_full h =
	h.nbr = 0
