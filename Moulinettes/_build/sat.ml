

let rec change c solution = 
	match c with
	| [] -> []
	| [x] when (x > 0 && not solution.(x)) || (x < 0 && solution.(-x)) ->
		[-x]
	| x::c2 -> x :: (change c2 solution)


let create_clauses buffer solution n m k =
	for i = 1 to m do
		Clause.print_clause buffer (change (Clause.random_clause n k) solution)
	done


let create n m k =
	Random.self_init () ;
	let buffer = Buffer.create 1 in
	Printf.bprintf buffer "p cnf %d %d\n" n m ;
	let solution = Array.init (n+1) (fun _ -> Random.bool ()) in
	create_clauses buffer solution n m k ;
	Buffer.contents buffer

let _ =
	let f = open_out "test.cnf" in
	Printf.fprintf f "%s" (create 20 100 0) ;
	close_out f
