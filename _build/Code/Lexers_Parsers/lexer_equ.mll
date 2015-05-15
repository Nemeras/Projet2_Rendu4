{
open Parser_equ;;
}


let int = ['0'-'9']+	(* Un entier positif (variable) *)


rule token = parse
	| [' ' '\t' '\n']	{ token lexbuf }		(* Saut des blancs *)
	| "~"			{ NOT }
	| "("			{ LPAREN }
	| ")"			{ RPAREN }
	| "\\/"			{ OR }
	| "/\\"			{ AND }
	| "=>"			{ IMPLY }
	| "x" (int as s)	{ VAR (int_of_string s) }	(* Variable *)
	| "="			{ EQU }
	| "!="			{ DIS }
	| eof			{ EOF }				(* Fin de fichier *)
