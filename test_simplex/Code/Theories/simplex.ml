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

type instanciation = num array  

type change_aux =
  |Low of int*num
  |LowS of int*num
  |Upp of int*num
  |UppS of int*num

type change =
  |Nothing of change_aux
  |Something of (instanciation*(bound array))*change_aux*int

type disequality = int*num;;

type rescheck=
	|Satisf
	|Unsatisf;;

type stack = change list	(* Pile des modifications au fil des instanciations dans DPLL *)
  



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

let compare_n a b =
match a,b with
|(_,(rat,_),_),(_,(rat2,_),_) -> compare_num rat rat2;;

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
|h::t -> nbl:=(snd h)::!nbl;print_string "j'ai trouve une putain de variable\n"; max (snd h) (max_vaux t nbl);;


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
(*let con_add = ref [[num_of_int 0,0],-1] in*)
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

let rec maxlist liste =
match liste with
|[]->0;
|h::t -> max h (maxlist t);;

let rec con_m_aux_aux lis num cm =
match lis with
|[] -> ();
|h::t ->let (rat,var)=h in Printf.printf "%d %d\n" num var;cm.(num).(var) <- cm.(num).(var) +/ rat;;

let rec con_m_aux con_add cm =
match con_add with
|h::t ->print_string "ajout d'une contrainte dans la matrice\n";flush stdout;let (lis,num)=h in con_m_aux_aux lis num cm;con_m_aux t cm;
|[]-> print_string "matrice cree"; flush stdout;cm;;

let create_con_m con_add taille=
print_string "matrice incoming\n";flush stdout;
let cm = Array.make_matrix taille taille (num_of_int 0) in
con_m_aux con_add cm;;

let create file aff_cnf =
	(*try*)
		let f = parse file in
		let f0, aa,con_add,bas_list,nbas_list = analyze f in
		let taille = (1+ (max (maxlist bas_list) (maxlist nbas_list))) in
		let cm = create_con_m con_add taille in
		print_int taille;
		print_string " <- c'etait la taille.\n";
		let solver = {
			aa = aa;
			ba = Array.make taille ((num_of_string "-9999999999999",num_of_int 0),(num_of_string "9999999999999",num_of_int 0));
			fv = nbas_list;
			inst = Array.make taille (num_of_int 0);
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
(*with _ -> (failwith "Erreur de saisie");;*)









		(** UPDATE / BACKTRACK **)


(* Choisit la modification à apporter à la structure selon la nature de l'instanciation *)
let rec parcours liste x =
match liste with
|h::t when h=x -> parcours t x
|h::t -> h::(parcours t x)
|[] -> [];;
 
let save_state struc =
Array.copy struc.inst,Array.copy struc.ba

let remove_l r s struc =
struc.bl <- parcours struc.bl r;
struc.nbl <- parcours struc.nbl s;
struc.bl <- Sort.sort_uniq fun_sort2 (s::struc.bl);
struc.nbl <- Sort.sort_uniq fun_sort2 (r::struc.nbl);;

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

let assertupper i c struc lit=
print_string "premiere condition incoming\n";
print_string "je vais chercher ";
print_int i;
print_newline();
print_int (Array.length struc.ba);
print_newline();
print_string (string_of_num (fst (snd struc.ba.(i))));
print_newline();
if c </ fst (snd struc.ba.(i)) then
  begin
	print_string "deuxieme condition incoming\n";
	flush stdout;
	let va = (snd (fst struc.ba.(i))) in
	print_string "troisieme condition incoming\n";
	flush stdout;
    if ((c </ fst (fst struc.ba.(i)))||((c =/ fst (fst struc.ba.(i)))&&(va >/ num_of_int 0))) then
      begin
	print_string "Nothing\n";
	flush stdout;
	struc.st <- Nothing(Upp(i,c))::struc.st
      end
    else
      begin
	print_string "begin else\n";
	flush stdout;
	let save = save_state struc in
	print_string "save faite.\n";
	flush stdout;
	let (l,_)=struc.ba.(i) in
	struc.ba.(i) <-(l,(c,num_of_int 0));
	print_string "bound mise à jour.\n";
	flush stdout;	
	let truc = (List.exists (fun k -> k=i) struc.nbl) in
	print_string "truc";
	print_newline();
	flush stdout;
	if List.exists (fun k -> k=i) struc.nbl && struc.inst.(i) >/ c then
	begin
	print_string "yolo je vais update_alg\n";
	flush stdout;
	end;
	if List.exists (fun k-> k=i) struc.nbl && struc.inst.(i) >/ c then update_alg i c struc;
	print_string "update_alg faite si necessaire.\n";
	flush stdout;
	struc.st<-Something(save,Upp(i,c),lit)::struc.st;
	print_string "maj du stack faite\n";
	flush stdout;
      end
  end
else struc.st <-Nothing(Upp(i,c))::struc.st;;

let assertlower i c struc lit=
if c >/ fst (fst struc.ba.(i)) then
  begin
	print_string "premiere condition incoming\n";
    if ((c >/ fst (snd struc.ba.(i)))||((c =/ fst (snd struc.ba.(i)))&&((snd (snd struc.ba.(i))) </ num_of_int 0))) then
      begin
        struc.st <-Nothing(Low(i,c))::struc.st;
      end
    else
      begin
        let save = save_state struc in
	let (_,u)=struc.ba.(i) in
	struc.ba.(i) <-((c,num_of_int 0),u);
	if List.exists (fun k-> k=i) struc.nbl && struc.inst.(i) </ c then update_alg i c struc;
	struc.st <- Something(save,Low(i,c),lit)::struc.st
      end
  end
else struc.st <-Nothing(Low(i,c))::struc.st;;

let assertupperstrict i c struc lit=
if c </ fst (snd struc.ba.(i)) then
  begin
    if (c <=/ fst (fst struc.ba.(i))) then
      begin
	struc.st<-Nothing(UppS(i,c))::struc.st
      end
    else
      begin
	let save = save_state struc in
	let (l,_)=struc.ba.(i) in
	struc.ba.(i) <-(l,(c,num_of_string "-1"));
	Printf.printf "Valeur de variable : %d" i;
	if List.exists (fun k-> k=i) struc.nbl then print_string "la variable existe\n";
	if List.exists (fun k-> k=i) struc.nbl && struc.inst.(i) >=/ c then
	  begin
	    print_string "Je dois mettre a jour une valeur\n";
	    flush stdout;
	    let nouvelle_instanciation = max_num ((c +/ fst (fst (struc.ba.(i))))//(num_of_int 2)) (c -/ (num_of_int 1)) in
	    Printf.printf "\n up : nv in : %s\n" (string_of_num nouvelle_instanciation);
	    update_alg i nouvelle_instanciation struc;
	  end;
	struc.st <- Something(save,Upp(i,c),lit)::struc.st
      end
  end
else struc.st<-Nothing(UppS(i,c))::struc.st;;

let assertlowerstrict i c struc lit=
print_string "assertlows incoming\n";
if c >/ fst (fst struc.ba.(i)) then
  begin
    Printf.printf "test: %s %s" (string_of_num c) (string_of_num (fst (snd struc.ba.(i))));
    if c >=/ fst (snd struc.ba.(i)) then
      begin
	print_string "pas de modification a faire\n";
	struc.st <-Nothing(LowS(i,c))::struc.st
      end
    else
      begin
        let save = save_state struc in
	let (_,u)=struc.ba.(i) in
	struc.ba.(i) <-((c,num_of_int 1),u);
	Printf.printf "Valeur de variable : %d" i;
	let nouvelle_instanciation = min_num ((c +/ fst (snd (struc.ba.(i))))//(num_of_int 2)) (c +/ (num_of_int 1)) in
	Printf.printf "\n low : nv in %s\n" (string_of_num nouvelle_instanciation);
	if List.exists (fun k -> k=i) struc.nbl then print_string "test sdkjfdkjfdksjfksjfs \n \n";
	if List.exists (fun k-> k=i) struc.nbl && struc.inst.(i) <=/ c then 
	  begin
	    print_string "Je dois metre a jour une valeur\n";
	    flush stdout;
	    update_alg i nouvelle_instanciation struc;
	    Printf.printf "nouvelle valeur : %s" (string_of_num struc.inst.(i))
	  end;
	struc.st<- Something(save,Low(i,c),lit)::struc.st
      end
  end
else struc.st <- Nothing(LowS(i,c))::struc.st;;

let modif_pos struc lit =
	print_string "debut modif struc \n";
let bidule = struc.aa.(lit) in
	print_string "lit recupere\n";		
  match bidule with	
  |Leq(var,(rat,k)) when k =/ num_of_int 0 -> print_string "assertupper incoming\n"; assertupper var rat struc lit;
  |Geq(var,(rat,k)) when k =/ num_of_int 0-> print_string "assertlow incoming\n"; assertlower var rat struc lit;
  |Leq(var,(rat,_)) -> print_string "assertupperstrict incoming\n"; assertupperstrict var rat struc lit;
  |Geq(var,(rat,_)) -> print_string "assertlowerstrict incoming\n"; assertlowerstrict var rat struc lit;
  |_-> failwith "probleme dans modif : lt ou gt encore present.";;
  
let modif_neg struc lit =
  match struc.aa.(lit) with
  |Leq(var,(rat,k)) when k =/ num_of_int 0 -> print_string "assups incoming \n"; assertupperstrict var rat struc (-lit);
  |Geq(var,(rat,k)) when k =/ num_of_int 0 -> print_string "asslows incoming \n"; assertlowerstrict var rat struc (-lit);
  |Leq(var,(rat,_)) -> assertupper var rat struc (-lit);
  |Geq(var,(rat,_)) -> assertlower var rat struc (-lit);
  |_-> failwith "probleme dans modif : lt ou gt encore present.";;


let modif struc lit = 
print_int lit;
print_string "\n";
if abs lit = lit then modif_pos struc lit else modif_neg struc lit;;


let rec search_stack1 x k struc stack refl=
match stack with
|Nothing(_)::t->search_stack1 x k struc t refl;
|Something(_,Upp(i,c),lit)::t when (i=x)&&(c=k)-> refl:=(-lit)::!refl;search_stack1 x k struc t refl;
|Something(_,_,_)::t -> search_stack1 x k struc t refl;
|[] -> ();;

let rec search_stack2 x k struc stack refl=
match stack with
|Nothing(_)::t->search_stack1 x k struc t refl;
|Something(_,Low(i,c),lit)::t when (i=x)&&(c=k)-> refl:=(-lit)::!refl;search_stack2 x k struc t refl;
|Something(_,_,_)::t -> search_stack1 x k struc t refl;
|[] -> ();;



let rec search_np1 struc lis i refl=
match lis with
|h::t when (struc.cm.(i).(h) >/ num_of_int 0) -> search_stack1 h (fst (snd struc.ba.(h))) struc struc.st refl; search_np1 struc t i refl
|h::t -> search_np1 struc t i refl
|[] -> ();;

let rec search_nm1 struc lis i refl=
match lis with
|h::t when (struc.cm.(i).(h) </ num_of_int 0) -> search_stack1 h (fst (fst struc.ba.(h))) struc struc.st refl; search_nm1 struc t i refl
|h::t -> search_np1 struc t i refl
|[] -> ();;

let valeur_correct1 i struc =
min (fst ( fst (struc.ba.(i))) +/ (num_of_int 1)) (((fst ( fst (struc.ba.(i))))+/(fst ( snd ( struc.ba.(i)))//(num_of_int 2))));;

let valeur_correct2 i struc =
max (fst ( snd (struc.ba.(i))) -/ (num_of_int 1)) (((fst ( fst (struc.ba.(i))))+/(fst ( snd ( struc.ba.(i)))//(num_of_int 2))));;


let rec check_aux_aux1 loop sat liste struc i =
print_string "yolo\n";
flush stdout;
Printf.printf "variables : %d %d et tailles : %d %d" i 4 (Array.length struc.cm) 4;
match liste with
|[]->print_string "check _aux_1 dit : insatisfiable\n";flush stdout; sat:=false;loop:=false;let refl = ref [0] in search_np1 struc struc.nbl i refl; search_nm1 struc struc.nbl i refl;struc.unsat<- !refl;
|x::t when List.exists (fun k -> k=x) struc.bl -> check_aux_aux1 loop sat t struc i;
|x::t when ((struc.cm.(i).(x) >/ (num_of_int 0)) && (struc.inst.(x) </ fst (snd (struc.ba.(x))))) || ((struc.cm.(i).(x) </ (num_of_int 0))  && (struc.inst.(x) >/ fst (fst  (struc.ba.(x))))) -> print_string "j'ai trouve de quoi pivotandupdate !\n";flush stdout; pivotandupdate i x (fst ( fst (struc.ba.(i)))) struc;
|x::t ->check_aux_aux1 loop sat t struc i;;

let rec check_aux_aux1strict loop sat liste struc i =
print_string "yolo\n";
flush stdout;
Printf.printf "variables : %d %d et tailles : %d %d" i 4 (Array.length struc.cm) 4;
match liste with
|[]->print_string "check _aux_1 dit : insatisfiable\n";flush stdout; sat:=false;loop:=false;let refl = ref [0] in search_np1 struc struc.nbl i refl; search_nm1 struc struc.nbl i refl;struc.unsat<- !refl;
|x::t when List.exists (fun k -> k=x) struc.bl -> check_aux_aux1strict loop sat t struc i;
|x::t when ((struc.cm.(i).(x) >/ (num_of_int 0)) && (struc.inst.(x) </ fst (snd (struc.ba.(x))))) || ((struc.cm.(i).(x) </ (num_of_int 0))  && (struc.inst.(x) >/ fst (fst  (struc.ba.(x))))) -> print_string "j'ai trouve de quoi pivotandupdate !\n";flush stdout; pivotandupdate i x (valeur_correct1 i struc) struc;
|x::t ->print_string "pas de degre de liberte sur celui la, j'avance.";flush stdout; check_aux_aux1strict loop sat t struc i;;

let rec check_aux_aux2 loop sat liste struc i =
match liste with
|[]-> sat:=false;loop:=false;let refl = ref [0] in search_np1 struc struc.nbl i refl; search_nm1 struc struc.nbl i refl; struc.unsat <- !refl;
|x::t when List.exists (fun k -> k=x) struc.bl -> check_aux_aux2 loop sat t struc i;
|x::t when ((struc.cm.(i).(x) </ (num_of_int 0)) && (struc.inst.(x) </ fst (snd (struc.ba.(x))))) || ((struc.cm.(i).(x) >/ (num_of_int 0))  && (struc.inst.(x) >/ fst (fst (struc.ba.(x))))) -> pivotandupdate i x (fst (fst (struc.ba.(i)))) struc;
|x::t -> check_aux_aux2 loop sat t struc i;;

let rec check_aux_aux2strict loop sat liste struc i =
print_newline();
print_string "yolo : peut etre bug :";
print_int i;
print_newline();
print_int (Array.length struc.cm);
print_newline();
Printf.printf "deuxieme valeur pouvant buguer : %d %d -> %s %s %d\n" (i) (hd liste) (string_of_num struc.inst.(hd liste)) (string_of_num (fst (fst struc.ba.(hd liste)))) (Array.length struc.cm.(i));
print_newline();
match liste with
|[]-> sat:=false;loop:=false;let refl = ref [0] in print_string "recherche d'explication\n";search_np1 struc struc.nbl i refl; search_nm1 struc struc.nbl i refl; struc.unsat <- !refl;
|x::t when List.exists (fun k -> k=x) struc.bl -> check_aux_aux2strict loop sat t struc i;
|x::t when ((struc.cm.(i).(x) </ (num_of_int 0)) && (struc.inst.(x) </ fst (snd (struc.ba.(x))))) || ((struc.cm.(i).(x) >/ (num_of_int 0))  && (struc.inst.(x) >/ fst (fst (struc.ba.(x))))) -> print_string "pivot and update incoming\n"; flush stdout;pivotandupdate i x (valeur_correct2 i struc) struc;
|x::t -> print_string "pas celui la\n"; check_aux_aux2strict loop sat t struc i;;

let rec check_aux loop sat liste struc= 
match liste with
|[] -> print_string "check unsat dit : satisfiable !\n"; flush stdout;sat:=true;loop:=false;
|x::t when (List.exists (fun k -> k=x) struc.nbl) -> check_aux loop sat t struc;
|x::t when ((struc.inst.(x) <=/ fst (fst struc.ba.(x)))&&(snd (fst struc.ba.(x)) <>/ num_of_int 0)) ->print_string "check_strict1\n"; check_aux_aux1strict loop sat struc.nbl struc x;
|x::y when ((struc.inst.(x) >=/ fst (snd struc.ba.(x)))&&(snd (snd struc.ba.(x)) <>/ num_of_int 0)) ->print_string "check_strict2\n"; check_aux_aux2strict loop sat struc.nbl struc x;
|x::t when struc.inst.(x) </ fst (fst struc.ba.(x)) -> print_string "j'ai trouve un x inferieur a sa borne inf\n";flush stdout; check_aux_aux1 loop sat struc.nbl struc x;
|x::t when struc.inst.(x) >/ fst (snd struc.ba.(x)) -> print_string "j'ai trouve un x superieur a sa borne sup\n";flush stdout; check_aux_aux2 loop sat struc.nbl struc x; 
|x::t -> check_aux loop sat t struc;;


let restore_state bounds instanc struc =
struc.inst <- instanc;
struc.ba <- bounds;;

let check_unsat struc =
let loop = ref true in
let sat = ref false in
while !loop do
	begin
	print_string "loop\n";
	flush stdout;
	check_aux loop sat struc.bl struc;
	end
done;
print_string "reussi ! \n";
flush stdout;
!sat;;
	
       

let update struc lit =
  print_string "update\n";
  (* On n'agit que si lit correspond à un atome *)
  if (abs lit) < (Array.length struc.aa) then
    begin
      print_string "modif struc\n";
      modif struc lit;
      print_string "modif struc fini, check_unsat\n";
      flush stdout;
      if check_unsat struc then 0
      else
	-max_int;
    end
  else
    0;;
  


		(** BACKTRACK **)


(* Annule la dernière affectation, si elle correspond à un atome *)
let backtrack struc lit =
print_string "backtrack\n";
if abs lit < Array.length (struc.aa) then
begin
  let last_change = hd struc.st in
  struc.st <- tl struc.st;
  match last_change with
    |Nothing(_) -> ();
    |Something((i,b),_,_) -> restore_state b i struc;
end;;





		(** INSATISFIABILITE **)


let unsat struc =
	struc.unsat




		(** AFFICHAGE DE LA SOLUTION **)

let rec print_l_inst lis struc=
match lis with
|[] -> Printf.printf "\n solutions donnees \n"; flush stdout;
|h::t -> Printf.printf "x%d = %s\n" h (string_of_num (struc.inst.(h)));flush stdout;print_l_inst t struc;;

let print_solution res solver =
	print_string "print_solution";
	flush stdout;
	match res with
	| Cnf.False ->
		print_string "s UNSATISFIABLE\n"
	| Cnf.True _ ->
		print_string "s SATISFIABLE\n" ;
		print_l_inst solver.fv solver;
		print_l_inst solver.nbl solver;
		print_l_inst solver.bl solver;;
