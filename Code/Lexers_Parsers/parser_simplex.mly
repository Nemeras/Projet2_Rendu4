%{
open General.Tseitin;;
open Num;;
%}

%token <int> VAR	/* Indice de variable */
%token <Num.num> RAT	/* Coefficient devant une variable */
%token LPAREN RPAREN
%token EQU DIS LEQ GEQ LT GT PLUS
%token IMPLY AND OR
%token NOT
%token EOF		/* Fin de fichier */

%right IMPLY
%left OR
%left AND
%nonassoc NOT



%start formula
%type <(((Num.num*int) list) * (Num.num*Num.num) * int) General.Tseitin.formula list> formula
/* L'élément de base de la formule est de la forme (somme de coef*xi, rationnel comme il est manipulé dans la théorie, k),
   où k désigne l'inégalité utilisée.                                                                                      */

	
%%


formula:
	| form formula		{ $1::$2 }
	| EOF			{ [] }
;


form:
	| atom_rel		{ Lit ($1, 0) }
	| atom_dis		{ $1 }
	| atom_equ        	{ $1 }
	| LPAREN form RPAREN	{ $2 }
	| NOT form		{ Not ($2,0) }
	| form OR form		{ Or ($1, $3, 0) }
	| form AND form		{ And ($1, $3, 0) }
	| form IMPLY form	{ Or (Not ($1,0), $3, 0) }
;


atom_rel:
	| sum LEQ RAT		{ ($1, ($3, Num.num_of_int 0), 1) }
	| sum GEQ RAT 		{ ($1, ($3, Num.num_of_int 0), 2) }
	| sum LT RAT		{ ($1, ($3, Num.num_of_int (-1)), 3) }
	| sum GT RAT 		{ ($1, ($3, Num.num_of_int 1), 4) }

;


/* On transforme une diségalité en deux inégalités strictes */
atom_dis:
	| sum DIS RAT
		{ Or (Lit (($1, ($3, Num.num_of_int (-1)), 3),0),
			Lit (($1, ($3, Num.num_of_int 1), 4),0),
			0)
		}
;


/* On transforme une égalité en deux égalités larges */
atom_equ:
 	| sum EQU RAT
		{ And (Lit (($1, ($3, Num.num_of_int 0), 1),0),
			Lit (($1, ($3, Num.num_of_int 0), 2),0),
			0)
		}
;


/* Somme de coefficients*xi */
sum:
	| RAT VAR PLUS sum	{ ($1, $2) :: $4 }
	| RAT VAR		{ [($1, $2)] }
