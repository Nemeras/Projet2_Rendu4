			(** THEORIE DE L'EGALITE **)

open General

open Tseitin
open List
open Num

		(** TYPES UTILISES **)


type atom =	
	| Eq of int*num
	| Geq of int*num
	| Leq of int*num
	| Dis of int*num

type change =
	|Test

type rescheck=
	|Satisf
	|Unsatisf;;

type stack = change list	(* Pile des modifications au fil des instanciations dans DPLL *)
  
type instanciation = num array  

type bound = num*num

type constraint_matrix = (num array) array

type struc = {
	aa : atom array;		(* Associe à une variable l'atome correspondant *)
	mutable bl : int list;
	mutable nbl : int list;
	mutable cm : constraint_matrix;
	mutable inst : instanciation;
	mutable ba : bound array;
	mutable st : stack;
	mutable unsat : Cnf.clause	(* Clause expliquant la contradiction levée par le solveur *)
}
	   



		(** LECTURE DES FICHIERS **)
let fun_sort = (fun k l -> compare (snd k) (snd l)) ;;
let fun_sort2 = compare;;

let compare_n a b =
match a,b with
|(_,rat,_),(_,rat2,_) -> compare rat rat2;;

module Atom =
	struct
		type t = ((int*int) list)*num*int
		let compare = compare_n
	end

module KeyAtom = Map.Make (Atom)

(* Map prenant en entrée des couples de variables (égalités / diségalités) et leur associant une variable dans DPLL *)
open KeyAtom


(* Construit la Map associant à chaque atome sa variable dans DPLL
   Renvoie la liste de formules dans laquelle on a remplacé les atomes par leurs variables et un tableau
   représentant la fonction inverse de la Map construite                                                 *)


let rec max_vaux lis nbl=
match lis with
|[] -> 0;
|h::t -> nbl:=(snd h)::!nbl; max (snd h) (max_vaux t nbl);;


let rec max_v form_list nbl=
match form_list with
|Not(fl,_) -> max_v fl nbl
|And(fl1,fl2,_) -> max (max_v fl1 nbl) (max_v fl2 nbl)
|Or(fl1,fl2,_) -> max (max_v fl1 nbl) (max_v fl2 nbl)
|Lit(atom,_) -> let (lis,_,_) = atom in max_vaux lis nbl;;

let rec maxv form_list nbl =
match form_list with
|h::t -> max (max_v h nbl) (maxv t nbl);
|[] -> 0;;

let rec analyze_aux ato bl numerotage con_add=
let (lis,rat,k) =ato in
numerotage:=!numerotage+1;
bl:=(!numerotage)::(!bl);
con_add:=((lis,!numerotage)::!con_add);
([1,!numerotage],rat,k);;

let rec analyze_rec2 forml bl nbl numerotage con_add=
match forml with
|Not(fl,v) -> Not(analyze_rec2 fl bl nbl numerotage con_add,v)
|And(fl1,fl2,v) -> And(analyze_rec2  fl1 bl nbl numerotage con_add, analyze_rec2  fl2 bl nbl numerotage con_add,v)
|Or(fl1,fl2,v) -> Or(analyze_rec2  fl1 bl nbl numerotage con_add,analyze_rec2 fl2 bl nbl numerotage con_add,v)
|Lit(ato,v) -> Lit(analyze_aux ato bl numerotage con_add ,v);;

let rec analyze_rec forml bl nbl numerotage con_add=
match forml with
|[] -> [];
|[h] -> [analyze_rec2 h bl nbl numerotage con_add]
|h::t -> (analyze_rec2 h bl nbl numerotage con_add )::(analyze_rec t bl nbl numerotage con_add);;

let analyze_beginning form_list =
let bas_list = ref [] in
let nbas_list = ref [] in
let maxva = maxv form_list nbas_list in
let numerotage = ref maxva in
let con_add = ref [[num_of_int 0,0],-1] in
let new_form = analyze_rec form_list nbas_list bas_list numerotage con_add in
let new_bas_list = Sort.sort_uniq fun_sort2 (List.map int_of_num !bas_list) in
nbas_list := Sort.sort_uniq fun_sort2 !nbas_list;
new_form,!con_add,new_bas_list,!nbas_list;;

let analyze form_l =
  let form_list,con_add,bas_list,nbas_list = analyze_beginning form_l in
	let m = ref empty in
	let rec aux f =
		match f with
		| Lit ((x,y,z), v) when mem (x,y,z) !m ->
			Lit (find (x,y,z) !m, v)
		| Lit ((x,y,z), v) ->
			m := add (x,y,z) (cardinal !m + 1) !m ;
			Lit (cardinal !m, v)
		| And (f1, f2, v) ->
			And (aux f1, aux f2, v)
		| Or (f1, f2, v) ->
			Or (aux f1, aux f2, v)
		| Not (f, v) ->
			Not (aux f, v)
	in
	let f0 = List.map aux form_list in
	let aa = Array.make (cardinal !m + 1) (Eq(0, num_of_int 0)) in
	while !m <> empty do
		let (x,y,z), i = choose !m in
		m := remove (x,y,z) !m ;
		match x,y,z with
		|[_,x],y,z when z=1 -> aa.(i) <- Eq(x,y);
		|[_,x],y,z when z=2 -> aa.(i) <- Leq(x,y);
		|[_,x],y,z when z=3 -> aa.(i) <- Geq(x,y);
		|[_,x],y,z when z=4 -> aa.(i) <- Dis(x,y);
	done ;
	f0,aa,con_add,bas_list,nbas_list



(* Lit un fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)


(* Lit une chaîne de caractères *)
let lexstr s =
	Lexing.from_string s


(* Parse un fichier contenant des formules logiques quelconques *)
let parse file =
	Parser_simplex.formula Lexer_simplex.token (lexbuf file)


(* Parse une CNF dans une chaîne de caractères *)
let parse_cnf s =
	Parser_cnf.cnf Lexer_cnf.token (lexstr s)

let create_con_m con_add bas_list nbas_list=
let cm = Array.make_matrix (List.length nbas_list) (List.length bas_list) (num_of_int 0) in
cm;;

let rec maxlist liste =
match liste with
|[]->0;
|h::t -> max h (maxlist t);;

let create file aff_cnf =
	try
		let f = parse file in
		let f0, aa,con_add,bas_list,nbas_list = analyze f in
		let cm = create_con_m con_add bas_list nbas_list in
		let solver = {
			aa = aa;
			ba = Array.make (List.length bas_list) (num_of_string "-1/0",num_of_string "1/0");
			inst = Array.make (max (maxlist nbas_list) (maxlist bas_list)) (num_of_int 0);
			cm = cm;
			nbl = nbas_list;
			bl = bas_list;
			st = [] ;
			unsat = []
		} in
		let s = conv_tseitin f0 (Array.length aa) in
		if aff_cnf then
			begin
			print_string "CNF produite :\n" ;
			print_string s ;
			print_newline ()
			end ;
		let cnf = parse_cnf s in
		cnf, solver
	with _ -> (failwith "Erreur de saisie");;









		(** UPDATE / BACKTRACK **)


(* Choisit la modification à apporter à la structure selon la nature de l'instanciation *)
let modif struc lit = ();;





let rec parcours liste x =
match liste with
|h::t when h=x -> parcours t x
|h::t -> h::(parcours t x)
|[] -> [];;
 

let remove_l r s struc =
struc.bl <- parcours struc.bl r;
struc.nbl <- parcours struc.nbl s;
struc.bl <- Sort.sort_uniq fun_sort2 (s::struc.bl);
struc.nbl <- Sort.sort_uniq fun_sort2 (r::struc.nbl);;

let pivot r s struc= 
remove_l r s struc;;

let rec update_alg_aux i v struc=
match struc.bl with
|h::t -> struc.inst.(h) <- struc.inst.(h) +/ (v -/ struc.inst.(i)) */ struc.cm.(h).(i);
|[] -> ();;

let update_alg i v struc=
update_alg_aux i v struc;
struc.inst.(i)<-v;;


let rec pivot_update_aux liste i j struc theta=
match liste with
|[] -> ();
|h::t when h = i-> pivot_update_aux t i j struc theta;
|h::t -> struc.inst.(h) <- struc.inst.(h) +/ theta */ struc.cm.(h).(j);pivot_update_aux t i j struc theta;; 

let pivotandupdate i j v struc=
  let theta =(v -/ struc.inst.(i)) // (struc.cm.(i).(j)) in
  struc.inst.(i) <- v;
  struc.inst.(j) <- struc.inst.(j) +/ theta;
  pivot_update_aux (struc.bl) i j struc theta;
  pivot i j struc;;
  

let rec check_aux_aux1 loop sat liste struc i =
match liste with
|[]-> sat:=false;loop:=false;
|x::t when ((struc.cm.(i).(x) >/ (num_of_int 0)) && (struc.inst.(x) </ snd (struc.ba.(x)))) || ((struc.cm.(i).(x) </ (num_of_int 0))  && (struc.inst.(x) > fst (struc.ba.(x)))) -> pivotandupdate i x (fst (struc.ba.(i))) struc;
|x::t -> check_aux_aux1 loop sat t struc i;;

let rec check_aux_aux2 loop sat liste struc i =
match liste with
|[]-> sat:=false;loop:=false;
|x::t when ((struc.cm.(i).(x) </ (num_of_int 0)) && (struc.inst.(x) </ snd (struc.ba.(x)))) || ((struc.cm.(i).(x) >/ (num_of_int 0))  && (struc.inst.(x) > fst (struc.ba.(x)))) -> pivotandupdate i x (fst (struc.ba.(i))) struc;
|x::t -> check_aux_aux1 loop sat t struc i;;

let rec check_aux loop sat liste struc= 
match liste with
|[] -> sat:=true;
|x::t when struc.inst.(x) < fst struc.ba.(x) -> check_aux_aux1 loop sat struc.nbl struc x;
|x::t when struc.inst.(x) > snd struc.ba.(x) -> check_aux_aux2 loop sat struc.nbl struc x;
|x::t -> check_aux loop sat t struc;;



let check_unsat struc =
let loop = ref true in
let sat = ref false in
while !loop do
	begin
	check_aux loop sat struc.bl struc;
	end
done;
!sat;;
	

let update struc lit =
(*	(* On n'agit que si lit correspond à un atome *)
	if abs lit < Array.length struc.aa then
		
	else
		0*)();;



		(** BACKTRACK **)


(* Annule la dernière affectation, si elle correspond à un atome *)
let backtrack struc lit =();;





		(** INSATISFIABILITE **)


let unsat struc =
	struc.unsat




		(** AFFICHAGE DE LA SOLUTION **)


(* Trouve une instanciation pour la i-ème variable *)


(* Si l'ensemble de formules est satisfiables, renvoie une affectation des variables dans N correspondante *)

