type op =
	|Equal 
	|Nequal
	|Sinferieur 
	|Ssup

and cond =
	|And of (cond*cond)
	|Or of (cond*cond)
	|Relation of (idstring*op*idstring)
	|In of (idstring*requete)
	|Nin of (idstring*requete)

and idstring =
 |Id of (string*string)
 |Name of string 

and orderl = 
  |Idd of (string*string)
  |Ida of (string*string)

and requete = 
 |Order of (requete*orderl list)
 |Group of (requete*idstring*column list)
 |Where of (column list*filename list*cond)
 |Union of (requete*requete)
 |Minus of (requete*requete)

and column = 
  |MinRename of (idstring*string)
  |MinCol of (idstring)
  |MaxRename of (idstring*string)
  |MaxCol of (idstring)
  |SumRename of (idstring*string)
  |SumCol of (idstring)
  |Rename of (idstring*string)
  |Col of (idstring)
  
and filename = 
  |Req of (requete*string)
  |File of (string*string)




(** On représente les tables par des listes de listes, avec dans une table, chaque colonne représenté par une liste avec en tête le nom de la colonne **)
(**
let rec appartient s l = match l with 
 |[] -> false
 |t::q -> if t==s then true else appartient s q
 
let rec find s l = match l with 
 |[] -> failwith "not in l"
 |(t::q)::q2 -> if s=t then (q) else find s q2
 |_->failwith "erreur dans la table"
 
let rec retrouve n l = match n,l with 
 |0,t::q -> t
 |_,[] -> failwith "outranged"
 |_,t::q -> retrouve (n-1) q
 
let rec find_table s l = match l with 
  |[] -> failwith "not in l_tot"
  |((t::q1)::q2)::q -> if s=t then ((t::q1)::q2) else find_table s q
  |_-> failwith "erreur dans la table"
 
 (** Nous avons le droit de supposer que l'union se fait sur des colonnes identiques sur l1 et l2 **)
let rec union (i1:req) (i2:req) = match i1,i2 with |Requete(l1),Requete(l2) -> 
 let rec union_colonne l1 l2 = match l2 with
  |[] -> l1
  |t::q -> if not (appartient t l1) then t::(union_colonne l1 q) else union_colonne l1 q in 
  match l1,l2 with 
  |_,[] -> l1
  |(t1::q1)::q11,(t::q)::q2 when t1==t -> (t1::union_colonne q1 q)::union (Requete(q11)) (Requete q2)
  |(t1::q1)::q11,(t::q)::q2 -> union (Requete(q11@[t1::q1])) (Requete l2) (** cette ligne serait incorrecte sans la suposition initiale**)
  |_->failwith "pb"
  
 (** Pour un minus, nous avons forcément des tailles de tables en entrée égale, avec les mêmes colonnes **)
 
let rec minus (i1:req) (i2:req) = match i1,i2 with |Requete(l1),Requete(l2) ->
 let rec minus_colonne l1 l2 = match l1 with
  |[] -> []
  |t::q -> if appartient t l2 then (minus_colonne q l2) else t::(minus_colonne q l2) in 
  match l1 with 
  |[] -> []
  |(t::q)::q1 -> let q2=find t l2 in (t::minus_colonne q q2)::(minus (Requete(q1)) (Requete(l2)))
  |_-> failwith "pb2"
 
 (** Comme vu dans le parser, on entre dans table_tot une liste de table (une table = une liste de liste, avec en tête l'id de la table) **)
 
 let rec test_cond table_tot nm_ligne cond = match cond with 
  |And(c1,c2) -> (test_cond table_tot nm_ligne c1) && (test_cond table_tot nm_ligne c2)
  |Or(c1,c2) -> (test_cond table_tot nm_ligne c1) || (test_cond table_tot nm_ligne c2)
  |Rel(Id(i1,i2),Equal,Id(i3,i4)) -> let l1= find_table i1 table_tot and l2 = find_table i3 table_tot in let q1 = find i2 l1 and q2 = find i4 l2 in retrouve nm_ligne q1 = retrouve nm_ligne q2
  |Rel(Id(i1,i2),Sinferieur,Id(i3,i4)) -> let l1= find_table i1 table_tot and l2 = find_table i3 table_tot in let q1 = find i2 l1 and q2 = find i4 l2 in retrouve nm_ligne q1 < retrouve nm_ligne q2
  |In(Id(i1,i2),l) -> let l1 = find_table i1 table_tot in let q1 = find i2 l1 in appartient (retrouve nm_ligne q1) l 
  |Nin(Id(i1,i2),l) -> let l1 = find_table i1 table_tot in let q1 = find i2 l1 in not (appartient (retrouve nm_ligne q1) l )
 
 let rec where_it q1 table cond i = match q1 with 
  |[] -> []
  |t::q -> if test_cond [table] i cond then t::where_it q table cond (i+1) else where_it q table cond (i+1)
 
 let rec where col table cond = match col with 
  |[] -> []
  |t::q -> let q1 = find t table in (where_it q1 table cond 0)::where q table cond **)
  
