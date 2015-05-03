open Random


let rec add_lit c x =
	match c with
	| [] -> [x]
	| y::_ when x = y -> c
	| y::c2 -> y :: (add_lit c2 x)

let split cnf n nbr =
	let i = int !nbr in
	let sign = -1 + 2 * int 2 in
	let x = sign * (1 + int n) in
	cnf.(!nbr) <- add_lit cnf.(i) (-x) ;
	cnf.(i) <- add_lit cnf.(i) x ;
	incr nbr


let rec suppr_nth c n =
	if n = 0 then
		List.tl c
	else
		(List.hd c) :: (suppr_nth (List.tl c) (n-1))

let suppr_lit cnf nbr =
	let i = int !nbr in
	let l = List.length cnf.(i) in
	if l >= 2 then
		begin
		let j = int l in
		cnf.(i) <- suppr_nth cnf.(i) j
		end


let add_clause cnf n nbr =
	cnf.(!nbr) <- Clause.random_clause n 0 ;
	incr nbr


let suppr_last c k =
	let l = List.length c in
	let tmp = ref c in
	let res = ref [] in
	for i = 0 to k-1 do
		let j = int (l-i) in
		res := (List.nth !tmp j) :: !res ;
		tmp := suppr_nth !tmp j
	done ;
	!res
	

let delete_sup cnf k =
	for i = 0 to Array.length cnf - 1 do
		if List.length cnf.(i) >= k then
			cnf.(i) <- suppr_last cnf.(i) k
	done


let create_clauses buffer n m k =
	let cnf = Array.make m [] in
	let x = 1 + int n in
	cnf.(0) <- [x] ;
	cnf.(1) <- [-x] ;
	let nbr = ref 2 in
	while !nbr < m do
		let i = int 3 in
		if i = 0 then
			split cnf n nbr
		else if i = 1 then
			suppr_lit cnf nbr
		else
			add_clause cnf n nbr
	done ;
	if k > 0 then
		delete_sup cnf k ;
	for i = 0 to m-1 do
		Clause.print_clause buffer cnf.(i)
	done
	

let create n m k =
	self_init () ;
	let buffer = Buffer.create 1 in
	Printf.bprintf buffer "p cnf %d %d\n" n m ;
	create_clauses buffer n m k ;
	Buffer.contents buffer
