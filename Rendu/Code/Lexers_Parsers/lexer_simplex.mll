{
open Parser_simplex;;
}


let int = ['0'-'9']+				(* Un entier positif *)
let abs = int '/' int				(* Un rationnel positif *)
let rat = ([ '-' ]? int) | ([ '-' ]? abs)	(* Un rationnel *)


rule token = parse
	| [' ' '\t' '\n']	{ token lexbuf }		(* Saut des blancs *)
	| "~"			{ NOT }
	| "("			{ LPAREN }
	| ")"			{ RPAREN }
	| "\\/"			{ OR }
	| "/\\"			{ AND }
	| "=>"			{ IMPLY }
	| (rat as s)		{ RAT (Num.num_of_string s) }	(* Coefficient *)
	| "x" (int as s)	{ VAR (int_of_string s) }	(* Variable *)
	| "+"			{ PLUS }
	| "<=" 			{ LEQ }
	| ">="			{ GEQ }
	| "<" 			{ LT }
	| ">"			{ GT }
	| "="			{ EQU }
	| "!="			{ DIS }
	| eof			{ EOF }				(* Fin de fichier *)
