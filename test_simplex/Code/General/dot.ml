			(** CRÉATION DE FICHIERS DOT **)


open Printf



(* Graphes : liste d'arêtes et tableau donnant les couleurs de chaque sommet *)
(* Les sommets peuvent être :
	0 : Conflit
	0 < n <= nb_variables : littéral positif n
	nb_variables < n < 2*nb_variables : littéral négatif -n+nb_variables
*)

type edges = (int*int) list

type color =
	| None	(* Sommet qui n'apparaîtra pas dans le graphe *)
	| Red
	| Blue
	| White
	| Purple
	| Yellow

type graph = {
	mutable e : edges ;
	mutable c : color array
}


(* Convertit un littéral en un index de tableau (de graph.c) *)
let to_index lit nb_variables =
	if lit >= 0 then
		lit
	else
		nb_variables + (abs lit)

(* Convertit un index de tableau (de graph.c) en un littéral *)
let to_lit i nb_variables =
	if i <= nb_variables then
		i
	else
		-i + nb_variables


let create_graph nb_variables =
	{ e = [] ; c = Array.make (1+2*nb_variables) None }


(* Ajoute une arête au graphe *)
let add_edge graph arete =
	graph.e <- arete::(graph.e)


(* Place le noeud lit à la couleur color *)
let set_color graph lit color nb_variables =
	let i = to_index lit nb_variables in
	graph.c.(i) <- color


(* Ecrit dans buffer les lignes correspondant aux arêtes du graphe *)
let rec compile_edges edges buffer =
	match edges with
	| [] -> ()
	| (s1,s2)::tail ->
		fprintf buffer "%d -> %d;\n" s1 s2 ;
		compile_edges tail buffer


(* Ecrit dans buffer les lignes correspondant aux couleur des sommets du graphe *)
let compile_color tc buffer nb_variables=
	fprintf buffer " 0 [label=\"Conflict\",style=filled,color=crimson]; \n";
	for i = 1 to Array.length tc - 1 do
		let label = to_lit i nb_variables in
		match (tc.(i)) with
		| None -> ()
		| Blue -> fprintf buffer "%d [label=\"%d\",style=filled,color=\"cornflowerblue\"];\n" label label
		| White -> fprintf buffer "%d [label=\"%d\",style=filled,color=\"ghostwhite\"];\n" label label
		| Purple -> fprintf buffer "%d [label=\"%d\",style=filled,color=\"darkorchid1\"];\n" label label
		| Yellow -> fprintf buffer "%d [label=\"%d\",style=filled,color=\"gold\"];\n" label label
		| Red -> failwith "[Dot] Mauvaise assignation de la couleur Rouge\n"
	done


(* Crée le fichier .dot représentant le graphe *)
let compile graph nb_variables =
	let buffer = open_out "graph.dot" in
	fprintf buffer "digraph G {\n size =\"4,4\";\n" ;
	compile_edges graph.e buffer ;
	compile_color graph.c buffer nb_variables ;
	fprintf buffer "}" ;
	close_out buffer
