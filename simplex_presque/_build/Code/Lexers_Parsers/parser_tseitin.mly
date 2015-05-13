%{
open General.Tseitin
%}


%token <int> VAR	/* Indice de variable */
%token LPAREN RPAREN
%token IMPLY AND OR
%token NOT
%token EOF		/* Fin de fichier */


%right IMPLY
%left OR
%left AND
%nonassoc NOT


%start formula
%type <int General.Tseitin.formlist> formula	/* Renvoie la liste des formules et l'indice de variable maximal */
%type <int General.Tseitin.form> form		/* Renvoie la formule lue et l'indice de variable maximal */


%%
formula:
	| form formula		{ (fst $1)::(fst $2), max (snd $1) (snd $2) }
	| EOF			{ [], 0 }
;

form:
	| VAR			{ Lit ($1,0), $1 }
	| LPAREN form RPAREN	{ $2 }
	| NOT form		{ Not (fst $2,0), snd $2 }
	| form OR form		{ Or (fst $1, fst $3, 0), max (snd $1) (snd $3) }
	| form AND form		{ And (fst $1, fst $3, 0), max (snd $1) (snd $3) }
	| form IMPLY form	{ Or (Not (fst $1,0), fst $3, 0), max (snd $1) (snd $3) }

