			(** THEORIE DE L'EGALITE **)

open General

open Tseitin
open List
open Num

		(** TYPES UTILISES **)


type atom =	
	| Lt of int*(num*num)
	| Geq of int*(num*num)
	| Leq of int*(num*num)
	| Gt of int*(num*num);;
  
type atochange =(num*num)*(num*num);;


type bound = (num*num)*(num*num)

type instanciation = (num*num) array  

type change_aux =
  |Low of int*(num*num)
  |LowS of int*(num*num)
  |Upp of int*(num*num)
  |UppS of int*(num*num)

type change =
  |Nothing of change_aux
  |Something of (instanciation*(bound array))*change_aux*int

type disequality = int*num;;

type rescheck=
	|Satisf
	|Unsatisf;;

type stack = change list	(* Pile des modifications au fil des instanciations dans DPLL *)

(*Definition d'operateurs necessaires pour la gestion des bornes strictes*)
  
let (+@) a b =
  let (rat,k)=a in 
  let (rat2, k2)=b in
  rat +/ rat2,k +/ k2;;

let (-@) a b =
  let (rat,k)=a in
  let (rat2,k2)=b in
  rat -/rat2, k -/ k2;;

let ( *@ ) a b =
  let (rat,k)=b in
  rat */ a,k */ a;;

let ( /@ ) a b  =
  let (rat,k)=a in
  rat // b, k// b;;

let ( <@ ) a b =
  let (rat,k) = a in
  let (rat2,k2) = b in
  (rat </ rat2)||((rat =/ rat2)&&(k </ k2));;

let ( <=@ ) a b =
  let (rat,k) = a in
  let (rat2,k2) = b in
  (rat </ rat2)||((rat =/ rat2)&&(k <=/ k2));;


let ( >@ ) a b =
  let (rat,k) = a in
  let (rat2,k2) = b in
  (rat >/ rat2)||((rat =/ rat2)&&(k >/ k2));;

let ( >=@ ) a b =
  let (rat,k) = a in
  let (rat2,k2) = b in
  (rat >/ rat2)||((rat =/ rat2)&&(k >=/ k2));;

let ( =@ ) a b =
  let (rat,k) = a in
  let (rat2,k2) = b in
  (rat =/ rat2)&&(k =/ k2);;




type constraint_matrix = (num array) array

type struc = {
	aa : atom array;		(* Associe à une variable l'atome correspondant *)
	fv : int list;
	mutable bl : int list;
	mutable nbl : int list;
	mutable cm : constraint_matrix;
	mutable inst : instanciation;
	mutable ba : bound array;
	mutable st : stack;
	taille : int;
	mutable unsat : Cnf.clause	(* Clause expliquant la contradiction levée par le solveur *)
}
	   



		(** LECTURE DES FICHIERS **)
let fun_sort = (fun k l -> compare (snd k) (snd l)) ;;
let fun_sort2 = compare;;



let compare_n a b = compare a b;;

let compare_n2 a b =
match a,b with
|(n1,_),(n2,_) -> compare n1 n2;;

module Atom =
	struct
		type t = ((int*int) list)*(num*num)*int
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
|Lit(ato,_) -> let (lis,_,_) = ato in max_vaux lis nbl;;

let rec maxv form_list nbl =
match form_list with
|h::t -> max (max_v h nbl) (maxv t nbl);
|[] -> 0;;
(*Lit recursivement l'arbre, renvoie la valeur de la plus "grande" variable et stocke toutes les variables non basiques dans la liste de variables non basiques.*)

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

(*Pretraite l'arbre : voir Readme*)
let analyze_beginning form_list =
let bas_list = ref [] in
let nbas_list = ref [] in
let maxva = maxv form_list nbas_list in
let numerotage = ref maxva in
let con_add = ref [] in
let new_form = analyze_rec form_list bas_list nbas_list numerotage con_add in
let new_nbas_list = Sort.sort_uniq fun_sort2 (!nbas_list) in
bas_list := Sort.sort_uniq fun_sort2 !bas_list;
new_form,!con_add,!bas_list,new_nbas_list;;

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
	let aa = Array.make (cardinal !m + 1) (Leq(0, (num_of_int 0,num_of_int 0))) in
	while !m <> empty do
		let (x,y,z), i = choose !m in
		m := remove (x,y,z) !m ;
		match x,y,z with
		|[_,x],y,z when z=1 -> aa.(i) <- Leq(x,y);
		|[_,x],y,z when z=2 -> aa.(i) <- Geq(x,y);
		|[_,x],y,z when z=3 -> aa.(i) <- Leq(x,y);
		|[_,x],y,z when z=4 -> aa.(i) <- Geq(x,y);
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

(*Donne le maximum d'une liste d'int*)
let rec maxlist liste =
match liste with
|[]->0;
|h::t -> max h (maxlist t);;

let rec con_m_aux_aux lis num cm =
match lis with
|[] -> ();
|h::t ->let (rat,var)=h in cm.(num).(var) <- cm.(num).(var) +/ rat;;

let rec con_m_aux con_add cm =
match con_add with
|h::t -> let (lis,num)=h in con_m_aux_aux lis num cm;con_m_aux t cm;
|[]-> flush stdout;cm;;

(*Cree la matrice de contraintes a partir d'une liste de contraintes*)
let create_con_m con_add taille=
let cm = Array.make_matrix taille taille (num_of_int 0) in
con_m_aux con_add cm;;

(*Initialise le solveur*)
let create file aff_cnf =
	try
		let f = parse file in
		let f0, aa,con_add,bas_list,nbas_list = analyze f in
		let taille = (1+ (max (maxlist bas_list) (maxlist nbas_list))) in
		let cm = create_con_m con_add taille in
		let solver = {
			aa = aa;
			ba = Array.make taille ((num_of_string "-9999999999999",num_of_int 0),(num_of_string "9999999999999",num_of_int 0));
			fv = nbas_list;
			inst = Array.make taille (num_of_int 0,num_of_int 0);
			cm = cm;
			taille = taille;
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


(*parcours une liste en supprimant toute apparence de la variable x dans cette liste*)
let rec parcours liste x =
match liste with
|h::t when h=x -> parcours t x
|h::t -> h::(parcours t x)
|[] -> [];;

(*sauvegarde l'etat du solveur*) 
let save_state struc =
Array.copy struc.inst,Array.copy struc.ba

(*Echange deux variable dont l'une est basique et l'autre non basique : la basique devient non basique et vice-versa.*)
let remove_l r s struc =
struc.bl <- parcours struc.bl r;
struc.nbl <- parcours struc.nbl s;
struc.bl <- Sort.sort_uniq fun_sort2 (s::struc.bl);
struc.nbl <- Sort.sort_uniq fun_sort2 (r::struc.nbl);;

(*Implementation de pivot *)
let pivot r s struc= 
remove_l r s struc;
let i = ref 0 in
let j = ref 0 in
while !i < (struc.taille-1) do
i:=!i+1;
if (!i <> s)&&(List.exists (fun k -> !i = k) struc.bl) then
  begin
    j:=0;
    while !j < (struc.taille-1) do
      j:=!j+1;
      
      if !j=r then
	struc.cm.(!i).(r)<- struc.cm.(!i).(r) +/ ((struc.cm.(!i).(s))//(struc.cm.(r).(s)))
      else
	begin
	  if !j <> s then
	    struc.cm.(!i).(!j) <- struc.cm.(!i).(!j) -/ ((struc.cm.(!i).(s) */ struc.cm.(r).(!i))//(struc.cm.(r).(s)));
	end
    done;
    struc.cm.(!i).(s) <- num_of_int 0;
  end
done;
i:=0;
while !i < (struc.taille-1) do
i:=!i+1;
if !i <> s then
  begin
    if !i=r then
      struc.cm.(s).(r) <- (num_of_int 1)//(struc.cm.(r).(s))
    else
    struc.cm.(s).(!i) <- (struc.cm.(r).(!i))//(struc.cm.(r).(s));
  end
done;;


let rec update_alg_aux i v struc=
match struc.bl with
|h::t -> struc.inst.(h) <- struc.inst.(h) +@ (v -@ (struc.cm.(h).(i) *@ struc.inst.(i)));
|[] -> ();;

(*Implementation de update*)
let update_alg i v struc=
update_alg_aux i v struc;
struc.inst.(i)<-v;;


let rec pivot_update_aux liste i j struc theta=
match liste with
|[] -> ();
|h::t when h = i-> pivot_update_aux t i j struc theta;
|h::t -> struc.inst.(h) <- struc.inst.(h) +@ (struc.cm.(h).(j) *@ theta); pivot_update_aux t i j struc theta;; 

(*Implementation de pivotandupdate*)
let pivotandupdate i j v struc=
  let theta =((v -@ struc.inst.(i)) /@ (struc.cm.(i).(j))) in
  struc.inst.(i) <- v;
  struc.inst.(j) <- struc.inst.(j) +@ theta;
  pivot_update_aux (struc.bl) i j struc theta;
  pivot i j struc;;

(*Change la borne sup d'une variable*)
let assertupper i c struc lit=
if c <@ (snd struc.ba.(i)) then
  begin
    if (c <@ (fst struc.ba.(i))) then
      begin
	struc.st <- Nothing(Upp(i,c))::struc.st
      end
    else
      begin
	let save = save_state struc in
	let (l,_)=struc.ba.(i) in
	struc.ba.(i) <-(l,c);	
	if List.exists (fun k-> k=i) struc.nbl && struc.inst.(i) >@ c then update_alg i c struc;
	struc.st<-Something(save,Upp(i,c),lit)::struc.st;
      end
  end
else struc.st <-Nothing(Upp(i,c))::struc.st;;

(*Change la borne inf d'une variable*)
let assertlower i c struc lit=
if c >@ (fst struc.ba.(i)) then
  begin
    if (c >@ (snd struc.ba.(i))) then
      begin
        struc.st <-Nothing(Low(i,c))::struc.st;
      end
    else
      begin
        let save = save_state struc in
	let (_,u)=struc.ba.(i) in
	struc.ba.(i) <-(c,u);
	if List.exists (fun k-> k=i) struc.nbl && struc.inst.(i) <@ c then update_alg i c struc;
	struc.st <- Something(save,Low(i,c),lit)::struc.st
      end
  end
else struc.st <-Nothing(Low(i,c))::struc.st;;


let modif_pos struc lit =	
  match struc.aa.(lit) with	
  |Leq(var,rat) -> assertupper var rat struc lit;
  |Geq(var,rat) -> assertlower var rat struc lit;
  |_-> failwith "probleme dans modif : lt ou gt encore present.";;
  
let modif_neg struc lit =
  match struc.aa.(lit) with
  |Leq(var,(rat,k)) when k =/ num_of_int 0 -> assertlower var (rat,num_of_int (-1)) struc (-lit);
  |Geq(var,(rat,k)) when k =/ num_of_int 0 -> assertupper var (rat,num_of_int 1) struc (-lit);
  |Leq(var,(rat,k)) -> assertlower var (rat,num_of_int 0) struc (-lit);
  |Geq(var,(rat,k)) -> assertupper var (rat,num_of_int 0) struc (-lit);
  |_-> failwith "probleme dans modif : lt ou gt encore present.";;

(*Suivant la valeur du litteral, applique la bonne modification a la structure.*)
let modif struc lit = 
if abs lit = lit then modif_pos struc lit else modif_neg struc (abs lit);;


let rec search_stack1 x k struc stack refl=
match stack with
|Nothing(_)::t->search_stack1 x k struc t refl;
|Something(_,Upp(i,c),lit)::t when (i=x)&&(c =@ k)-> refl:=(-lit)::!refl;search_stack1 x k struc t refl;
|Something(_,_,_)::t -> search_stack1 x k struc t refl;
|[] -> ();;

let rec search_stack2 x k struc stack refl=
match stack with
|Nothing(_)::t->search_stack1 x k struc t refl;
|Something(_,Low(i,c),lit)::t when (i=x)&&(c =@ k)-> refl:=(-lit)::!refl;search_stack2 x k struc t refl;
|Something(_,_,_)::t -> search_stack1 x k struc t refl;
|[] -> ();;


(*Permet de construire l'explication de l'insatisfiabilité*)
let rec search_np1 struc lis i refl=
match lis with
|h::t when (struc.cm.(i).(h) >/ num_of_int 0) -> search_stack1 h (snd struc.ba.(h)) struc struc.st refl; search_np1 struc t i refl
|h::t -> search_np1 struc t i refl
|[] -> ();;

let rec search_nm1 struc lis i refl=
match lis with
|h::t when (struc.cm.(i).(h) </ num_of_int 0) -> search_stack1 h (fst struc.ba.(h)) struc struc.st refl; search_nm1 struc t i refl
|h::t -> search_np1 struc t i refl
|[] -> ();;


let rec check_aux_aux1 loop sat liste struc i lit=
match liste with
|[]-> sat:=false;loop:=false;let refl = ref [-lit] in search_np1 struc struc.nbl i refl; search_nm1 struc struc.nbl i refl;struc.unsat<- !refl;
|x::t when List.exists (fun k -> k=x) struc.bl -> check_aux_aux1 loop sat t struc i lit;
|x::t when ((struc.cm.(i).(x) >/ (num_of_int 0)) && (struc.inst.(x) <@ (snd (struc.ba.(x)))))|| ((struc.cm.(i).(x) </ (num_of_int 0))  && (struc.inst.(x) >@ (fst  (struc.ba.(x))))) -> 
  pivotandupdate i x ( fst (struc.ba.(i))) struc;
|x::t ->check_aux_aux1 loop sat t struc i lit;;


let rec check_aux_aux2 loop sat liste struc i lit=
match liste with
|[]-> sat:=false;loop:=false;let refl = ref [-lit] in search_np1 struc struc.nbl i refl; search_nm1 struc struc.nbl i refl; struc.unsat <- !refl;
|x::t when List.exists (fun k -> k=x) struc.bl -> check_aux_aux2 loop sat t struc i lit;
|x::t when ((struc.cm.(i).(x) </ (num_of_int 0)) && (struc.inst.(x) <@ snd (struc.ba.(x)))) || ((struc.cm.(i).(x) >/ (num_of_int 0)) && (struc.inst.(x) >@ fst (struc.ba.(x)))) ->
  pivotandupdate i x (fst (struc.ba.(i))) struc;
|x::t -> check_aux_aux2 loop sat t struc i lit;;


let rec check_aux loop sat liste struc lit= 
match liste with
|[] -> sat:=true;loop:=false;
|x::t when (List.exists (fun k -> k=x) struc.nbl) -> check_aux loop sat t struc lit;
|x::t when struc.inst.(x) <@ (fst struc.ba.(x)) -> check_aux_aux1 loop sat struc.nbl struc x lit;
|x::t when struc.inst.(x) >@ (snd struc.ba.(x)) -> check_aux_aux2 loop sat struc.nbl struc x lit; 
|x::t -> check_aux loop sat t struc lit;;

(*charge un etat memoire*)
let restore_state bounds instanc struc =
struc.inst <- instanc;
struc.ba <- bounds;;


(* Permet de verifier si une instance du solveur est satisfiable*)
let check_unsat struc lit=
let loop = ref true in
let sat = ref false in
while !loop do
	check_aux loop sat struc.bl struc lit;
done;

!sat;;
	
       
(*met a jour la structure*)
let update struc lit =
  (* On n'agit que si lit correspond à un atome *)
  if (abs lit) < (Array.length struc.aa) then
    begin
      modif struc lit;
      if check_unsat struc lit then 0
      else
	-max_int;
    end
  else
    0;;
  


		(** BACKTRACK **)


(* Annule la dernière affectation, si elle correspond à un atome *)
let backtrack struc lit =
if abs lit < Array.length (struc.aa) then
begin
  let last_change = hd struc.st in
  struc.st <- tl struc.st;
  match last_change with
    |Nothing(_) -> ();
    |Something((i,b),_,_) -> restore_state b i struc;
end;;

(*trouve une valeur acceptable pour le delta*)
let compute_delta struc =
  let delta = ref (num_of_string "2/10") in
  let kmax = ref (num_of_int 0) in
    let temp = ref (num_of_int 0) in
for i=1 to (-1+Array.length struc.ba) do
  temp:= ((abs_num (fst (fst struc.ba.(i)))) +/ (abs_num (fst (snd struc.ba.(i))))) // (num_of_int 2);
  if !temp <>/ (num_of_int 0) then
    delta := min_num (!delta) (!temp);
      kmax := max_num (!kmax) (max_num (snd (fst struc.ba.(i))) (snd (snd struc.ba.(i))));
done;
(!delta */ !kmax)//(num_of_int 2);;

		(** INSATISFIABILITE **)

(*Donne une explication de l'insatisfiabilite*)
let unsat struc =
	struc.unsat




		(** AFFICHAGE DE LA SOLUTION **)

let rec print_l_inst lis struc delta=
match lis with
|[] -> flush stdout;
|h::t -> Printf.printf "x%d = %s\n" h (string_of_num ((fst struc.inst.(h)) +/ ((snd struc.inst.(h)) */ delta)));flush stdout;print_l_inst t struc delta;;

(*Affiche une instanciation*)
let print_solution res solver =
	match res with
	| Cnf.False ->
		print_string "s UNSATISFIABLE\n"
	| Cnf.True _ ->
		print_string "s SATISFIABLE\n" ;
		print_l_inst solver.fv solver (compute_delta solver);;
