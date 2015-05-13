{
open Parser_cnf;;
}

let int = ['-']? ['0'-'9']+			(* Un entier positif ou n�gatif *)
let comment = 'c' [^ '\n']* ('\n' | eof)	(* Un commentaire est d�limit� par c et un \n ou un eof *)
let space = [' ' '\t']+				(* Un espace entre deux cha�nes de caract�res signifiantes *)

rule token = parse
	| [' ' '\t' '\n'] | comment			{ token lexbuf }				(* Saut des blancs/commentaires *)
	| "p cnf" space (int as v) space (int as c)	{ INIT (int_of_string v, int_of_string c) }	(* En-t�te *)
	| '0'						{ EOC }						(* Fin de clause *)
	| int as s					{ LIT (int_of_string s) }			(* Litt�ral *)
	| eof						{ EOF }						(* Fin de fichier *)
