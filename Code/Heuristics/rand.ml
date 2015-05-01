
open General
open Cnf



type struc = {
	taken : bool array ;
	mutable nbr : int
}


let heuristic = 1


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


let next h pos current wl =
	let n = ref (Random.int h.nbr) in
	let i = ref 1 in
	while !n <> 0 || h.taken.(!i) do
		if not h.taken.(!i) then
			decr n ;
		incr i
	done ;
	if Random.bool () then
		!i
	else
		- !i


let is_instanciation_full h =
	h.nbr = 0
