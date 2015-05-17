{
open Parser_cnf;;
}

let int = ['-']? ['0'-'9']+			(* Un entier positif ou négatif *)
let comment = 'c' [^ '\n']* ('\n' | eof)	(* Un commentaire est délimité par c et un \n ou un eof *)
let space = [' ' '\t']+				(* Un espace entre deux chaînes de caractères signifiantes *)

rule token = parse
	| [' ' '\t' '\n'] | comment			{ token lexbuf }				(* Saut des blancs/commentaires *)
	| "p cnf" space (int as v) space (int as c)	{ INIT (int_of_string v, int_of_string c) }	(* En-tête *)
	| '0'						{ EOC }						(* Fin de clause *)
	| int as s					{ LIT (int_of_string s) }			(* Littéral *)
	| eof						{ EOF }						(* Fin de fichier *)
