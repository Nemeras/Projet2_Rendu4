{
open Parser_tseitin ;;
}


let int = ['0'-'9']+				(* Un entier positif (variable) *)
let comment = 'c' [^ '\n']* ('\n' | eof)	(* Un commentaire est délimité par c et un \n ou un eof *)


rule token = parse
	| [ ' ' '\t' '\n' ] | comment	{ token lexbuf }		(* Saut des blancs/commentaires*)
	| "~"				{ NOT }
	| "("				{ LPAREN }
	| ")"				{ RPAREN }
	| "\\/"				{ OR }
	| "/\\"				{ AND }
	| "=>"				{ IMPLY }
	| int as s			{ VAR (int_of_string s) }	(* Variable *)
	| eof				{ EOF }				(* Fin de fichier *)
