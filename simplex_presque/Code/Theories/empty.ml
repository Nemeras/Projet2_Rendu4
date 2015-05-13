		(** THEORIE RELATIVE A TSEITIN **)

open General

(* Cette théorie permet de l'analyse de formules logiques quelconques comme un cas particulier de DPLL(T), avec une théorie vide *)


type atom = int
type struc = int


(* Lit le fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)


(* Lit un chaîne de caractères *)
let lexstr s =
	Lexing.from_string s


(* Parse le fichier Tseitin *)
let parse file =
	Parser_tseitin.formula Lexer_tseitin.token (lexbuf file)


(* Parse la chaîne de caractères représentant une CNF produite par Tseitin *)
let parse_cnf s =
	Parser_cnf.cnf Lexer_cnf.token (lexstr s)


let create file aff_cnf =
	try
		let f, m = parse file in
		let s = Tseitin.conv_tseitin f m in
		if aff_cnf then
			begin
			print_string "CNF produite :\n" ;
			print_string s ;
			print_newline ()
			end ;
		let cnf = parse_cnf s in
		cnf, m
	with _ -> (failwith "Erreur de saisie")


(* Comme la théorie Base, le solveur ne fait rien ici *)

let update solver x =
	0


let backtrack solver x =
	()


(* Cette fonction ne sera jamais utilisée dans DPLL, car le solveur ne relèvera jamais de contradiction *)
let unsat solver =
	[0]


let print_solution res solver =
	Cnf.print_solution res solver
