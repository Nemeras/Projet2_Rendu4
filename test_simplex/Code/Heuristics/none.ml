open General
open Cnf

type struc = {
	sol : (bool*int) array ;
	mutable k : int
}


let heuristic = 0


let init cnf pos =
	{
		sol = Array.make (cnf.v_real+2) (false, 0) ;
		k = 1
	}


let update h x =
	h.sol.(abs x) <- true, snd h.sol.(abs x) ;
	if abs x = h.k then
		begin
		while fst h.sol.(h.k) do
			h.k <- h.k + 1
		done ;
		h.sol.(h.k) <- false, abs x
		end

let backtrack h x =
	h.sol.(abs x) <- false, snd h.sol.(abs x) ;
	if abs x = snd h.sol.(h.k) then
		h.k <- abs x

let learning h c =
	()

let next h pos current wl =
	h.k

let is_instanciation_full h =
	h.k = Array.length h.sol - 1
