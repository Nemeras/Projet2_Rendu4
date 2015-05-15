		(** THEORIE IMPLEMENTANT LA GESTION DES FICHIERS CNF **)

open General
open Cnf

(* Ce fichier permet d'intégrer facilement à l'aide de foncteurs la gestion des théories dans DPLL(T), sans avoir à traiter
   le SAT-solver originel comme un cas "spécial". En particulier, ce solveur ne fait rien : il sert d'ersatz de théorie à DPLL *)


type atom = int
type struc = int


(* Lit le fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)


(* Interprète le fichier *)
let parse file =
	Parser_cnf.cnf Lexer_cnf.token (lexbuf file)


(* Crée la CNF représentée dans le fichier *)
let create file aff_cnf =
	try
		let cnf = parse file in
		
		if aff_cnf then
			begin
			let f = open_in file in
			let l = in_channel_length f in
			let b = Buffer.create l in
			Buffer.add_channel b f l ;
			print_string "CNF produite :\n" ;
			print_string (Buffer.contents b) ;
			print_newline () ;
			end ;
		
		(* Affiche les warnings sur le nombre de clauses et de varaibles *)
		if cnf.v_real <> cnf.v then
			Printf.printf "Attention : L'indice maximal des variables est %d, alors que le nombre annoncé était %d\n" cnf.v_real cnf.v ;
		if cnf.c_real <> cnf.c then
			Printf.printf "Attention : Le fichier comporte %d clauses, alors que %d clauses étaient annoncées\n" cnf.c_real cnf.c ;
		
		cnf, cnf.v_real
	with _ -> (failwith "Erreur de saisie")


(* Le solveur ne fait rien dans cette théorie *)

let update solver x =
	0


let backtrack solver x =
	()


(* Cette fonction ne sera jamais utilisée dans DPLL, car le solveur ne relèvera jamais de contradiction *)
let unsat solver =
	[0]


let print_solution res solver =
	Cnf.print_solution res solver
