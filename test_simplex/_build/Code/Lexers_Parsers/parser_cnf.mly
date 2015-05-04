%{
open General.Cnf
%}

%token <int*int> INIT	/* En-tête : indice max des variables, nombre de clauses */
%token <int> LIT	/* Indice de littéral */
%token EOC		/* Fin de clause */
%token EOF		/* Fin de fichier */

%start cnf
%type <General.Cnf.cnf> cnf	/* On construit la CNF et on compte son nombre de clauses
                                /* et l'indice max des variables en même temps qu'on lit le fichier */

%%

cnf:			/* Construction d'une CNF */
	| INIT		{ { clauses = [] ; v = fst $1 ; v_real = 0 ; c = snd $1 ; c_real = 0 } }	/* CNF vide */
	| cnf clause	{ $1.clauses <- (fst $2)::$1.clauses ;
			  $1.c_real <- $1.c_real + 1 ; $1.v_real <- max $1.v_real (snd $2) ;
			  $1 }										/* On ajoute une clause */
;

clause:			/* Construction d'une clause : on renvoie la clause et l'indice max des variables utilisées dans la clause */
	| LIT clause	{ $1::(fst $2), max (abs $1) (snd $2) }
	| EOC		{ [], 0 }	/* Clause vide */
