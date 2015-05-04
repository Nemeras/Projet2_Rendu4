%{
open General.Tseitin
%}


%token <int> VAR	/* Indice de variable */
%token LPAREN RPAREN
%token EQU DIS LEQ REQ PLUS
%token IMPLY AND OR
%token NOT
%token EOF		/* Fin de fichier */


%right IMPLY
%left OR
%left AND
%nonassoc NOT



%start formula
%type <(int*int) General.Tseitin.formula list> formula	/* On renvoie une formule dont les atomes sont des couples, avec des nombres
                                                   	* positifs si c'est une égalité
                                                        * négatifs sinon                                                       */
%type <(int*int) General.Tseitin.formula> form
			

%%
formula:
	| form formula		{ $1::$2 }
	| EOF			{ [] }
;


form:
	| atom			{ Lit (($1, $3), 0) }
	| LPAREN form RPAREN	{ $2 }
	| NOT form		{ Not ($2,0) }
	| form OR form		{ Or ($1, $3, 0) }
	| form AND form		{ And ($1, $3, 0) }
	| form IMPLY form	{ Or (Not ($1,0), $3, 0) }

atom:
 	| sum EQU INT		{ Eq($1,$3) }
	| sum LEQ INT		{ Leq($1,$3) }
	| sum GEQ INT 		{ Geq($1,$3) }
	| sum DIS INT		{ Ineq($1,$3) }

sum:
	|VAR PLUS sum		{ (1,$1)::$3 }		
	|INT VAR PLUS sum	{ ($1,$2)::$4 }
	|VAR 			{ [(1,$1)] } 
	|INT VAR		{ [($1,$2)] }
