			(** FONCTIONS AUXILIAIRES POUR SIMPLEX **)


open Num

open General
open Tseitin




		(** TYPES **)

(* Un rationnel est représenté ici par le type num.num. On utilise des couples de rationnels pour l'extension aux inégalités strictes *)


(* Un atome est de la forme xi <= ou >= r où r est un rationnel *)
type atom =
	| Geq of int * (num*num)
	| Leq of int * (num*num)



(* Borne inf, borne sup d'une variable *)
type bound = (num*num) * (num*num)



(* Assigne à chaque variabke une instanciation temporaire *)
type instanciation = (num*num) array



(* Matrice des contraintes : matrice des (a_ij) dans l'article présentant la méthode *)
type constraint_matrix = (num array) array



	(* Informations retenues dans la pile *)

(* Hypothèse qui a été demandée *)
type change_aux =
	| Low of int * (num*num)	(* AssertLower(xi,r) *)
	| Upp of int * (num*num)	(* AssertUpper(xi,r) *)

(* Soit on n'a rien fait, soit on a modifié des informations suite à l'AssertLower/Upper *)
type change =
	| None
	| Some of (instanciation * (int * bound)) * change_aux * int
		(* Précédente instanciation, (i, bound) où bound est le précédent encadrement de la variable i,
		   hypothèse demandée, littéral correpsondant mis à vrai dans DPLL(T)                           *)

type stack = change list	(* Pile des modifications au fil des instanciations dans DPLL *)



type struc = {
	aa : atom array ;		(* Associe à une variable de DPLL(T) l'atome correspondant *)
	fv : int list ;			(* Variables dont on veut la valeur à la fin (final variables) *)
	mutable bl : int list ;		(* Variables basiques (B) (basic_list) *)
	mutable nbl : int list ;	(* Variables non basiques (N) (non_basic_list) *)
	mutable cm : constraint_matrix ;
	mutable inst : instanciation ;
	mutable ba : bound array ;
	mutable st : stack ;
	mutable unsat : Cnf.clause ;	(* Clause expliquant la contradiction levée par le solveur *)
	size : int			(* Array.length struc.cm *)
}




		(** OPERATEURS SPÉCIFIQUES **)


(* Definition d'operateurs necessaires pour la gestion des bornes strictes : cf page 10 de l'article *)
	
let (+@) a b =
	let rat, k = a in 
	let rat2, k2 = b in
	rat +/ rat2, k +/ k2

let (-@) a b =
	let rat, k = a in 
	let rat2, k2 = b in
	rat -/rat2, k -/ k2

let ( *@ ) a b =
	let rat, k = b in
	rat */ a, k */ a

let ( /@ ) a b =
	let rat, k = a in
	rat // b, k// b


let ( <@ ) a b =
	let rat, k = a in 
	let rat2, k2 = b in
	rat </ rat2 || (rat =/ rat2 && k </ k2)

let ( <=@ ) a b =
	let rat, k = a in 
	let rat2, k2 = b in
	rat </ rat2 || (rat =/ rat2 && k <=/ k2)

let ( >@ ) a b =
	let rat, k = a in 
	let rat2, k2 = b in
	rat >/ rat2 || (rat =/ rat2 && k >/ k2)

let ( >=@ ) a b =
	let rat, k = a in 
	let rat2, k2 = b in
	rat >/ rat2 || (rat =/ rat2 && k >=/ k2)

let ( =@ ) a b =
	let rat, k = a in 
	let rat2, k2 = b in
	rat =/ rat2 && k =/ k2




		(** PREPROCESSING **)


	(* maxv *)

(* Même chose que maxv_aux, mais sur la somme à gauche d'une égalité dans l'ensemble d'équations *)
let rec maxv_sum sum nbl =
	match sum with
	| [] -> 0
	| h::t ->
		nbl := (snd h) :: !nbl ;
		max (snd h) (maxv_sum t nbl)

(* Même chose que maxv, mais sur une unique formule *)
let rec maxv_aux f nbl =
	match f with
	| Not (f0, _) -> maxv_aux f0 nbl
	| And (f1, f2, _) -> max (maxv_aux f1 nbl) (maxv_aux f2 nbl)
	| Or (f1, f2, _) -> max (maxv_aux f1 nbl) (maxv_aux f2 nbl)
	| Lit ((sum,_,_), _) -> maxv_sum sum nbl


(* Construit nbl et renvoie la variable de plus grand indice dans l'ensemble de formules *)
let rec maxv form_list nbl =
	match form_list with
	| [] -> 0
	| f::t -> max (maxv_aux f nbl) (maxv t nbl)



	(* preprocess *)

(* Crée une nouvelle variable basique et transforme l'équation, ainsi que les contraintes con_add et bl en conséquence *)
let rec preprocess_equ atom bl con_add current_max_var =
	let sum, rat, k = atom in
	incr current_max_var ;
	bl := !current_max_var :: !bl ;
	con_add := (sum, !current_max_var) :: !con_add ;
	!current_max_var, rat, k

(* Applique preprocess_aux sur tous les atome de la formule *)
let rec preprocess_f f bl con_add current_max_var =
	match f with
	| Not (f0, v) ->
		Not (preprocess_f f0 bl con_add current_max_var, v)
	| And (f1, f2, v) -> 
		And (preprocess_f f1 bl con_add current_max_var, preprocess_f f2 bl con_add current_max_var, v)
	| Or (f1, f2, v) ->
		Or (preprocess_f f1 bl con_add current_max_var, preprocess_f f2 bl con_add current_max_var, v)
	| Lit (atom, v) ->
		Lit (preprocess_equ atom bl con_add current_max_var, v)

(* Applique preprocess_f sur toutes les formules de form_list *)
let rec preprocess form_list bl con_add current_max_var =
	match form_list with
	| [] -> []
	| h::t ->
		(preprocess_f h bl con_add current_max_var) :: preprocess t bl con_add current_max_var



(* Pretraite l'instance : consruit bl, nbl, con_add (liste des contraintes), et transforme les formules en conséquence
   (preprocessing dans l'article)                                                                                      *)
let analyze_init form_list =
	let bl = ref [] in
	let nbl = ref [] in
	let con_add = ref [] in
	let current_max_var = ref (maxv form_list nbl) in
	let new_form_list = preprocess form_list bl con_add current_max_var in
	nbl := Sort.sort_uniq compare !nbl ;
	bl := Sort.sort_uniq compare !bl ;
	new_form_list, !con_add, !bl, !nbl, !current_max_var



	(* Construction de la matrice de contraintes *)

(* Ajoute la contrainte correspondant à l'équation xi = sum(a_ij*xj) *)
let rec cm_equ equ i cm =
	match equ with
	| [] -> ()
	| (coef, var) :: t ->
		cm.(i).(var) <- cm.(i).(var) +/ coef ;
		cm_equ t i cm

(* Cree la matrice de contraintes a partir d'une liste de contraintes *)
let rec cm_aux con_add cm =
	match con_add with
	| (equ, i) :: t ->
		cm_equ equ i cm ;
		cm_aux t cm
	| [] -> ()

(* Crée la matrice de contraintes *)
let create_cm con_add size =
	let cm = Array.make_matrix size size (num_of_int 0) in
	cm_aux con_add cm ;
	cm




		(** PIVOT **)


(* Echange le rôle de deux variable dont l'une est basique et l'autre non basique : la basique r devient non basique et vice-versa pour s *)
let swap r s struc =
	struc.bl <- List.filter (fun i -> i != r) struc.bl ;
	struc.nbl <- List.filter (fun i -> i != s) struc.nbl ;
	struc.bl <- Sort.sort_uniq compare (s::struc.bl) ;
	struc.nbl <- Sort.sort_uniq compare (r::struc.nbl)


(* Implémente la méthode pivot *)
let pivot r s struc =
	
	(* Les rôles de r et s sont échangés *)
	swap r s struc ;
	
	(* Construction de l'équation xs = ... *)
	for i = 1 to struc.size - 1 do
		if i = r then
			struc.cm.(s).(r) <- (num_of_int 1) // struc.cm.(r).(s)
		else if i <> s then
			struc.cm.(s).(i) <-  (num_of_int (-1)) */ struc.cm.(r).(i) // struc.cm.(r).(s)
	done ;
	
	(* Injection de cette équation dans les autres contraintes (élimination de xr) *)
	for i = 1 to struc.size - 1 do
		if (i <> r) && (List.exists (fun k -> i = k) struc.bl) then
			begin
			for j = 1 to struc.size - 1 do
				struc.cm.(i).(j) <- struc.cm.(i).(j) +/ (struc.cm.(i).(s) */ struc.cm.(s).(j))
			done ;
			struc.cm.(i).(s) <- num_of_int 0
			end
	done ;
	
	for i = 1 to struc.size - 1 do
		struc.cm.(r).(i) <- num_of_int 0 ;
	done



	(* Implementation de pivot_and_update *)

let rec pivot_update_aux bl i j struc theta =
	match bl with
	| [] -> ()
	| h::t when h = i ->
		pivot_update_aux t i j struc theta
	| h::t ->
		struc.inst.(h) <- struc.inst.(h) +@ (struc.cm.(h).(j) *@ theta) ;
		pivot_update_aux t i j struc theta 

let pivot_and_update i j v struc =
	let theta = (v -@ struc.inst.(i)) /@ struc.cm.(i).(j) in
	struc.inst.(i) <- v ;
	struc.inst.(j) <- struc.inst.(j) +@ theta ;
	pivot_update_aux struc.bl i j struc theta ;
	pivot i j struc




		(** DETECTION / EXPLICATION DE LA CONTRADICTION **)


(* Permet de construire l'explication de l'insatisfiabilité *)
(* Dans l'article (page 8), Gamma se décompose en trois ensembles : le premier est calculé par search_np et search_stack_np,
   et le second par search_nm et search_stack_nm                                                                              *)

let rec search_stack_np x k struc stack refl=
	match stack with
	| [] -> ()
	| None :: t ->
		search_stack_np x k struc t refl
	| Some (_, Upp (i,c), lit) :: t when i = x && c =@ k ->
		refl := (-lit) :: !refl ;
		search_stack_np x k struc t refl
	| _ :: t ->
		search_stack_np x k struc t refl

let rec search_np struc lis i refl =
	match lis with
	| h::t when sign_num struc.cm.(i).(h) > 0 ->
		search_stack_np h (snd struc.ba.(h)) struc struc.st refl ;
		search_np struc t i refl
	| h::t -> search_np struc t i refl
	| [] -> ()


let rec search_stack_nm x k struc stack refl=
	match stack with
	| [] -> ()
	| None ::t ->
		search_stack_nm x k struc t refl
	| Some (_, Low (i,c), lit)::t when i = x && c =@ k ->
		refl := (-lit) :: !refl ;
		search_stack_nm x k struc t refl
	| _ :: t ->
		search_stack_nm x k struc t refl

let rec search_nm struc lis i refl =
	match lis with
	| h::t when sign_num struc.cm.(i).(h) < 0 ->
		search_stack_nm h (fst struc.ba.(h)) struc struc.st refl ;
		search_nm struc t i refl
	| h::t -> search_nm struc t i refl
	| [] -> ()



(* check_aux_inf/sup : cherche à réparer le décalage de l'instanciation de xi par rapport à sa borne inf/sup.
	- 1er cas du match : On n'a pas pu réparer l'erreur, on lève donc une contradiction, acoompagnée d'une clause l'expliquant.
	- 2e cas : On peut réparer l'erreur, et on le fait avec pivot_and_update.
	- 3e cas : On cherche toujours à réparer l'erreur.                                                                          *)

let rec check_aux_inf loop sat nbl struc i lit =
	match nbl with
	| [] ->
		sat := false ;
		loop := false ;
		let refl = ref [-lit] in
		search_np struc struc.nbl i refl ;
		search_nm struc struc.nbl i refl ;
		struc.unsat <- !refl
	| x::t when (struc.cm.(i).(x) >/ (num_of_int 0) && struc.inst.(x) <@ snd struc.ba.(x)) || (struc.cm.(i).(x) </ (num_of_int 0) && struc.inst.(x) >@ fst struc.ba.(x)) ->
		pivot_and_update i x (fst struc.ba.(i)) struc
	| x::t ->
		check_aux_inf loop sat t struc i lit

let rec check_aux_sup loop sat nbl struc i lit =
	match nbl with
	| [] ->
		sat := false ;
		loop := false ;
		let refl = ref [-lit] in
		search_np struc struc.nbl i refl ;
		search_nm struc struc.nbl i refl ;
		struc.unsat <- !refl
	| x::t when (struc.cm.(i).(x) </ (num_of_int 0) && struc.inst.(x) <@ snd struc.ba.(x)) || (struc.cm.(i).(x) >/ (num_of_int 0) && struc.inst.(x) >@ fst struc.ba.(x)) ->
		pivot_and_update i x (snd struc.ba.(i)) struc
	| x::t ->
		check_aux_sup loop sat t struc i lit


(* Cherche une variable basique qui est mal instanciee par rapport à ses bornes *)
let rec check_aux loop sat bl struc lit= 
	match bl with
	| [] ->	(* N'en a pas trouvé *)
		sat := true ; loop := false
	| x::t when struc.inst.(x) <@ fst struc.ba.(x) ->	(* En a trouvé une sous sa borne inf *)
		check_aux_inf loop sat struc.nbl struc x lit
	| x::t when struc.inst.(x) >@ snd struc.ba.(x) ->	(* En a trouvé une au-dessus de sa borne sup *)
		check_aux_sup loop sat struc.nbl struc x lit
	| x::t ->
		check_aux loop sat t struc lit
