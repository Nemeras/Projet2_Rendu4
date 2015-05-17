


let test file options =
	let t1 = Unix.gettimeofday () in
	let _ = Unix.system ("./resol " ^ file ^ options ^ " > trash") in
	let t2 = Unix.gettimeofday () in
	t2-.t1





let create_file dir k n j =
	let name = Printf.sprintf "%s/sat%d.cnf" dir j in
	let f = open_out name in
	Printf.fprintf f "%s" (Sat.create n (5*n) k) ;
	close_out f ;
	let name = Printf.sprintf "%s/unsat%d.cnf" dir j in
	let f = open_out name in
	Printf.fprintf f "%s" (Unsat.create n (5*n) k) ;
	close_out f


let series file med j =
	med.(0).(j) <- test file " -vsids" ;
	med.(1).(j) <- test file " -vsids -wl"



let median med i =
	Array.sort compare med.(i) ;
	med.(i).(Array.length med.(i) / 2)


let tests k n nbr =
	let res = Array.make_matrix nbr 2 0. in
	let _ = Unix.system "rm -r Files" in
	Unix.mkdir "Files" 0o777 ;
	for i = 1 to nbr do
		let dir = Printf.sprintf "Files/%d" (20 + i*(n-20)/nbr) in
		Unix.mkdir dir 0o777 ;
		for j = 1 to 5 do
			create_file dir k (20 + i*(n-20)/nbr) j
		done ;
		Printf.printf "En cours : %dv...\t" (20 + i*(n-20)/nbr) ; flush stdout ;
		let med = Array.make_matrix 2 10 0. in
		for j = 1 to 5 do
			series (Printf.sprintf "%s/sat%d.cnf" dir j) med (2*(j-1)) ;
			series (Printf.sprintf "%s/unsat%d.cnf" dir j) med (2*(j-1)+1) ;
		done ;
		for j = 1 to 2 do
			res.(i-1).(j-1) <- median med (j-1)
		done ;
		print_string "Fait\n" ; flush stdout ;
		let _ = Unix.system (Printf.sprintf "rm -r %s" dir) in ()
	done ;
	let _ = Unix.system "rm -r Files" in
	let _ = Unix.system "rm trash" in
	res


let dat res n nbr =
	let f = open_out "comparaison.dat" in
	Printf.fprintf f "argument -CL -WL-CL\n" ;
	for i = 0 to Array.length res - 1 do
		Printf.fprintf f "%d " (20 + i*(n-20)/nbr) ;
		for j = 0 to Array.length res.(i) - 1 do
			Printf.fprintf f "%f " res.(i).(j)
		done ;
		Printf.fprintf f "\n"
	done ;
	close_out f


let generate () =
	Unix.system "gnuplot -persist Moulinettes/plot_versions.p"


let _ =
	
	(* Gestion des arguments et des options *)
	let k = ref 0 in
	let n = ref 0 in
	let nbr = ref 30 in
	
	let options = [
		("-nbr", Arg.Set_int nbr, "Permet de choisir le nombre de tests") ;
		("-k", Arg.Set_int k, "Se restreint à des clauses de longueur au plus k") ;
	] in
	
	Arg.parse options (fun s -> n := int_of_string s) "Ce programme génère une courbe gnuplot correspondant à des tests sur les différentes versions de DPLL" ;
	
	let res = tests !k !n !nbr in
	dat res !n !nbr ;
	generate ()
	
