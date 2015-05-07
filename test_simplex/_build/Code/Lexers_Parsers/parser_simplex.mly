%{
open General.Tseitin
%}

%token <int> VAR	/* Indice de variable */
%token <Num.num> RAT
%token LPAREN RPAREN
%token EQU DIS LEQ GEQ PLUS
%token IMPLY AND OR
%token NOT
%token EOF		/* Fin de fichier */

%right IMPLY
%left OR
%left AND
%nonassoc NOT

%start formula
%type <(((Num.num*int) list) *Num.num*int) General.Tseitin.formula list> formula	
%%

formula:
	| form formula		{ $1::$2 }
	| EOF			{ [] }
;


form:
	| atom			{ Lit ($1, 0) }
	| LPAREN form RPAREN	{ $2 }
	| NOT form		{ Not ($2,0) }
	| form OR form		{ Or ($1, $3, 0) }
	| form AND form		{ And ($1, $3, 0) }
	| form IMPLY form	{ Or (Not ($1,0), $3, 0) }
;
atom:
 	| sum EQU RAT		{ ($1,$3,1) }
	| sum LEQ RAT		{ ($1,$3,2) }
	| sum GEQ RAT 		{ ($1,$3,3) }
	| sum DIS RAT		{ ($1,$3,4) }
;
sum:
	|RAT VAR PLUS sum	{ ($1,$2)::$4 }
	|RAT VAR		{ [($1,$2)] }
