			(** TABLEAUX DYNAMIQUES **)



type 'a dynarray = {
	mutable a : 'a array ;	(* Tableau courant *)
	mutable length : int	(* Longueur actuelle du tableau dynamique *)
}


(* Crée un tableau dynamique de taille l rempli avec la constante l *)
let make l cst =
	{ a = Array.make l cst ; length= l }


(* Renvoie dyn.a.(i) si i < dyn.length, et cst hors de cette limite *)
let func_add dyn cst i=
	match i with
	| i when i < (Array.length dyn.a) -> dyn.a.(i)
	| i -> cst


(* Insère x en fin du tableau dynamique
 * cst sera inséré dans les éléments de dyn.a inutilisés
 *)
let add dyn x cst=
	(* Si dyn.a est saturé, on double la taille de dyn.a *)
	if dyn.length = (Array.length dyn.a) then 
		begin
		let dyn2 = Array.init (1+2*dyn.length) (fun i -> func_add dyn cst i) in
		dyn.a <- dyn2;
		end
	;
	dyn.a.(dyn.length) <- x;
	dyn.length <- 1 + dyn.length (* La taille apparente du tableau est augmentée de 1 *)
