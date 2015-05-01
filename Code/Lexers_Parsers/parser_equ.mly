%{
open General.Tseitin
%}


%token <int> VAR	/* Indice de variable */
%token LPAREN RPAREN
%token EQU DIS
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
	| VAR EQU VAR		{ Lit (($1, $3), 0) }
	| VAR DIS VAR		{ Not (Lit (($1, $3), 0), 0) }
	| LPAREN form RPAREN	{ $2 }
	| NOT form		{ Not ($2,0) }
	| form OR form		{ Or ($1, $3, 0) }
	| form AND form		{ And ($1, $3, 0) }
	| form IMPLY form	{ Or (Not ($1,0), $3, 0) }
