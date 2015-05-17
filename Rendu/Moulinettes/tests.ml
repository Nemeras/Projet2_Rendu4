


let test file options =
	let t1 = Unix.gettimeofday () in
	let _ = Unix.system ("./resol " ^ file ^ options ^ " > trash") in
	let t2 = Unix.gettimeofday () in
	(*print_float (t2-.t1) ; print_newline () ;*)
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


let series file res i =
	let t1 = Unix.gettimeofday () in
	res.(i).(2) <- res.(i).(2) +. test file " -cl" ;
	res.(i).(3) <- res.(i).(3) +. test file " -cl -wl" ;
	res.(i).(4) <- res.(i).(4) +. test file " -cl -rand" ;
	res.(i).(5) <- res.(i).(5) +. test file " -cl -moms" ;
	res.(i).(6) <- res.(i).(6) +. test file " -cl -dlis" ;
	res.(i).(7) <- res.(i).(7) +. test file " -vsids" ;
	res.(i).(8) <- res.(i).(8) +. test file " -cl -wl -rand" ;
	res.(i).(9) <- res.(i).(9) +. test file " -cl -wl -moms" ;
	res.(i).(10) <- res.(i).(10) +. test file " -cl -wl -dlis " ;
	res.(i).(11) <- res.(i).(11) +. test file " -wl -vsids"


let tests k n nbr =
		let t1 = Unix.gettimeofday () in
	let res = Array.make_matrix nbr 12 0. in
	Unix.system "rm -r Files" ;
	Unix.mkdir "Files" 0o777 ;
	print_string "Création des fichiers...\n" ; flush stdout ;
	for i = 1 to nbr do
		let dir = Printf.sprintf "Files/%d" (20 + i*(n-20)/nbr) in
		Unix.mkdir dir 0o777 ;
		for j = 1 to 15 do
			create_file dir k (20 + i*(n-20)/nbr) j
		done
	done ;
	for i = 1 to nbr do
		let dir = Printf.sprintf "Files/%d" (20 + i*(n-20)/nbr) in
		Printf.printf "En cours : %d...\t" (20 + i*(n-20)/nbr) ; flush stdout ;
		for j = 1 to 15 do
			series (Printf.sprintf "%s/sat%d.cnf" dir j) res (i-1) ;
			series (Printf.sprintf "%s/unsat%d.cnf" dir j) res (i-1) ;
		done ;
		let t2 = Unix.gettimeofday () in
		for j = 1 to 12 do
			res.(i-1).(j-1) <- res.(i-1).(j-1)/.30.
		done ;
		print_string "Fait\n" ;
	done ;
	res;;


let dat res n nbr =
	let f = open_out "comparaison.dat" in
	Printf.fprintf f "argument CL CL-WL RAND MOMS DLIS VSIDS WL-RAND WL-MOMS WL-DLIS WL-VSIDS\n" ;
	for i = 0 to Array.length res - 1 do
		Printf.fprintf f "%d " (20 + i*(n-20)/nbr) ;
		for j = 2 to Array.length res.(i) - 1 do
			Printf.fprintf f "%f " res.(i).(j)
		done ;
		Printf.fprintf f "\n"
	done ;
	close_out f


let generate () =
	Unix.system "gnuplot -persist Moulinettes/script-plot.p" ;
	Unix.system "gnuplot -persist Moulinettes/script-plot2.p" ;
	Unix.system "gnuplot -persist Moulinettes/script-plot3.p"


let _ =
	
	(* Gestion des arguments et des options *)
	let t1= Unix.gettimeofday () in
	let k = ref 0 in
	let n = ref 0 in
	let nbr = ref 30 in
	
	let options = [
		("-nbr", Arg.Set_int nbr, "Permet de choisir le nombre de tests") ;
		("-k", Arg.Set_int k, "Se restreint à des clauses de longueur au plus k") ;
	] in
	
	Arg.parse options (fun s -> n := int_of_string s) "Ce programme génère une courbe gnuplot correspondant à des tests sur les différentes versions de DPLL " ;
	
	let res = tests !k !n !nbr in
	dat res !n !nbr ;
	generate ();
	let t2= Unix.gettimeofday () in
	print_float (t2-.t1) ; print_newline () ;
	
