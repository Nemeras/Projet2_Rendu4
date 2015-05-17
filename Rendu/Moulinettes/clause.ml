open Random


let swap t i j =
	let tmp = t.(i) in
	t.(i) <- t.(j) ;
	t.(j) <- tmp


let random_permutation n =
	let s = Array.init (n+1) (fun i -> i) in
	for i = n downto 2 do
		let j = 1 + int (i-1) in
		swap s i j
	done ;
	s


let length n =
	if 0 = int (2*n) then
		1
	else if 1 = int n then
		2
	else
		begin
		let y = min (1.-.min_float) (float 1.) in
		3 + ((int_of_float ( -3.*. log (1. -. y))) mod (n-3))
		end
		

let random_clause n k =
	let c = ref [] in
	let s = random_permutation n in
	let l =
		if k = 0 then
			length n
		else
			k
	in
	for i = 1 to l do
		let sign = -1 + 2 * int 2 in
		c := (sign * s.(i)) :: !c
	done ;
	!c



let rec print_clause buffer c =
	match c with
	| [] -> Printf.bprintf buffer "0\n"
	| x::c2 ->
		Printf.bprintf buffer "%d " x ;
		print_clause buffer c2
