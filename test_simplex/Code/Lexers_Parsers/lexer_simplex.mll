{
open Parser_simplex;;
open Num;;
}


let int = ['0'-'9']+	(* Un entier positif (variable) *)
let abs = int '/' int
let rat = ([ '-' ]? int) | ([ '-' ]? abs)

rule token = parse
	| [' ' '\t' '\n']	{ token lexbuf }		(* Saut des blancs *)
	| "~"			{ NOT }
	| "("			{ LPAREN }
	| ")"			{ RPAREN }
	| "\\/"			{ OR }
	| "/\\"			{ AND }
	| "=>"			{ IMPLY }
	| (rat as s)		{ RAT (Num.num_of_string s) }
	| "x" (int as s)	{ VAR (int_of_string s) }	(* Variable *)
	| "+"			{ PLUS }
	| "<=" 			{ LEQ }
	| ">="			{ GEQ }
	| "<" 			{ LT }
	| ">"			{ GT }
	| "="			{ EQU }
	| "!="			{ DIS }
	| eof			{ EOF }				(* Fin de fichier *)
