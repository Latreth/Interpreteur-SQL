open Expr
open Read_csv


exception Wrong_type


let invert_liste l =
  let rec aux_4 l lvide = match l with
    |[] -> lvide
    |x::xs -> aux_4 xs (x::lvide)
  in aux_4 l []

let trouve col_id liste =
  let rec aux col_id liste n =
    match liste with
    |[] -> failwith "erreur 1"
    |l::ls ->if col_id = l then n else aux col_id ls (n+1)
  in aux col_id liste 0

let rec ordre a b n asc = match n with
	|0 -> begin
		match (a,b) with
		| (Name(b1)::q1,Name(c)::q2) -> begin let t1=Name(b1) in let t2=Name(c) in try  if ( int_of_string(b1) < int_of_string(c) ) then asc else ( if ( int_of_string(b1)  = int_of_string(c) ) then false else not asc )  with _ -> if ( t1 < t2 ) then asc else ( if ( t1 = t2 ) then false else not asc ) end
		| _ -> failwith "erreur -2"
		end
	|_ -> match (a,b) with
		| (t1::q1,t2::q2) -> ordre q1 q2 (n-1) asc
		| _ -> failwith "erreur 0"

let rec insertion col tab att asc = match tab with
	|[] -> [col]
	|t::q -> if ordre col t att asc then col::tab else t::(insertion col q att asc)

let tri tab col = 
	let rec insere tableau att asc trie = match tableau with
		|[] -> trie
		|t::q -> insere q att asc (insertion t trie att asc)
	in
	match tab with
		|[] -> failwith "erreur -1"
		|t::q -> begin match col with
			|Idd(a,b)-> t::(insere q (trouve (Id(a,b)) t) false [])
			|Ida(a,b)-> t::(insere q (trouve (Id(a,b)) t) true [])
		end
		
let rec fuse l1 l2 col_l = match (l1,l2,col_l) with
	| ([],[],[]) -> []
	| (Name(t1)::q1,Name(t2)::q2,MinRename(_,_)::q) -> if (int_of_string t1) < (int_of_string t2) then Name(t1) :: (fuse q1 q2 q) else Name(t2) :: (fuse q1 q2 q)
	| (Name(t1)::q1,Name(t2)::q2,MinCol(_)::q) -> if (int_of_string t1) < (int_of_string t2) then Name(t1) :: (fuse q1 q2 q) else Name(t2) :: (fuse q1 q2 q)
	| (Name(t1)::q1,Name(t2)::q2,MaxRename(_,_)::q) -> if (int_of_string t1) > (int_of_string t2) then Name(t1) :: (fuse q1 q2 q) else Name(t2) :: (fuse q1 q2 q)
	| (Name(t1)::q1,Name(t2)::q2,MaxCol(_)::q) -> if (int_of_string t1) > (int_of_string t2) then Name(t1) :: (fuse q1 q2 q) else Name(t2) :: (fuse q1 q2 q)
	| (Name(t1)::q1,Name(t2)::q2,SumRename(_,_)::q) -> Name(string_of_int((int_of_string t1)+(int_of_string t2))) :: (fuse q1 q2 q) 
	| (Name(t1)::q1,Name(t2)::q2,SumCol(_)::q) -> Name(string_of_int((int_of_string t1)+(int_of_string t2))):: (fuse q1 q2 q)
	| (Name(t1)::q1,Name(t2)::q2,Rename(_,_)::q) -> Name(t1) :: (fuse q1 q2 q)
	| (Name(t1)::q1,Name(t2)::q2,Col(_)::q) -> Name(t1) :: (fuse q1 q2 q)
	| (_,_,_)-> failwith "erreur -4"
	
let rec regroup l1 l2 col_l = match (l1,l2,col_l) with
	| ([],[],[]) -> failwith "erreur -2"
	| (t1::q1,t2::q2,Rename(_,_)::q) -> if (t1=t2) then true else false
	| (t1::q1,t2::q2,Col(_)::q) -> if (t1=t2) then true else false
	| (t1::q1,t2::q2,t::q) -> regroup q1 q2 q
	|_ -> failwith "pb"

let group table col col_liste =
	let rec merge tab col_l = match tab with
		|[] -> []
		|[a] -> [a]
		|a::b::q -> if (regroup a b col_l) then merge ((fuse a b col_l)::q) col_l else a::(merge (b::q) col_l)
	in
	match col with
		|Id(a,b)-> merge (tri table (Ida(a,b))) (invert_liste col_liste)
		| _ -> failwith "erreur -3"


let extra  liste n =
  let rec aux liste n m =
    match liste with
    |[] -> failwith "erreur 2"
    |l::ls -> if n=m then l else let m=m+1 in aux ls n m
  in aux liste n 0

let rename m a =
  let rec aux l a =
    match l with
    |[] -> []
    |x::xs ->
      begin
        match x with
        |Name(nom) -> (Id(a,nom))::(aux xs a)
        |Id(nom,col) -> (Id(a,col))::(aux xs a)
      end
  in
  match m with
  |[] -> []
  |deb::fin -> (aux deb a)::fin
   
let concat a b =
  let rev_a=invert_liste a in
  let rec aux i j=
    match i with
    |[] -> j
    |x::xs -> aux xs (x::j)
  in aux rev_a b

let cartesian taba tabb =
  let rec aux_cart_b x tabb res = match tabb with
    |[] -> res
    |y::ys -> aux_cart_b x ys ((concat  x y)::res)
  in
  let rec aux_cart_a taba tabb res = match taba with
    |[] -> res
    |x::xs -> aux_cart_a xs tabb (aux_cart_b x tabb res)
  in match taba with
     |[] -> tabb
     |deba::fina ->
       begin
         match tabb with
         |[] -> taba
         |debb::finb -> (concat deba debb)::(invert_liste (aux_cart_a fina finb []))
       end
                              
let cond_ok a b c =
  match b with
  |Equal -> if a=c then true else false
  |Nequal -> if a != c then true else false
  |Sinferieur -> begin match a,c with 
                  |Name(a1),Name(c1) -> if  int_of_string(a1) <  int_of_string(c1) then true else false
                  |_-> failwith "pas bon !!"end
  |Ssup -> match a,c with 
                  |Name(a1),Name(c1) -> if  int_of_string(a1) >  int_of_string(c1) then true else false
                  |_-> failwith "pas bon !!"
   (** begin
      match a with
      |Val(na) ->
        begin
          match c with
          |Val(nc) -> if na<nc then true else false
          |_ -> failwith "erreur 3"
        end
      |_ -> failwith "erreur 4"
    end **)

(**let rec parcours_double l1 b l2 tab = match l1 with
  |[] -> []
  |t1::q1 -> parcours_simple1 t1 b l2 tab @ parcours_double q1 b l2 tab
  
  and parcours_simple1 t1 b l2 tab = match l2 with 
    |[] -> []
    |t2::q2 -> parcours_simple t1 b t2 tab @ parcours_simple1 t1 b q2 tab  **)

let rec parcours_simple a b c tab = 
  match a with
  |Id(taba,cola) ->
    begin
      match c with
      |Id(tabc,colc) ->
        begin
          match tab with
          |tab_name::tab_noname -> 
            let inda= trouve a tab_name in
            let indc= trouve c tab_name in
            let rec aux tab_aux inda b indc res =
              match tab_aux with
              |[] -> res
              | x::xs ->
                 begin
                   match cond_ok (extra x inda) b (extra x indc) with
                   |false -> aux xs inda b indc res
                   |true -> aux xs inda b indc (x::res)
                 end
            in tab_name::(aux tab_noname inda b indc [])
          |_ -> failwith "erreur 5"
        end
      |Name(n) -> begin
          match tab with
          |tab_name::tab_noname -> 
            let inda= trouve a tab_name in
            let rec aux tab c b inda res =
              match tab with
              |[] -> res
              | x::xs ->
                 begin
                   match cond_ok (extra x inda) b c with
                   |false -> aux xs c b inda res
                   |true -> aux xs c b inda (x::res)
                 end
            in tab_name::(aux tab_noname c b inda [])
          |_ -> failwith "erreur 6"
        end
    end
  |_ -> 
    begin
      match c with 
      |Id(tabc,colc) ->
        begin
          match tab with
          |tab_name::tab_noname -> 
            let indc= trouve c tab_name in
            let rec aux tab a b indc res =
              match tab with
              |[] -> res
              | x::xs ->
                 begin
                   match cond_ok a b (extra x indc) with
                   |false -> aux xs a b indc res
                   |true -> aux xs a b indc (x::res)
                 end
            in tab_name::(aux tab_noname a b indc [])
          |_ -> failwith "erreur 6"
        end
      |_ -> failwith "condition ne portant pas sur les colonne de la table : inutile"
    end
   
let rec is_in x list =
  match list with
  |[] -> false
  |l::ls -> if l=x then true else is_in x ls
   
let intersec resa resb =
  let rec aux resa resb res = match resa with
    |[] -> res
    |x::xs -> if is_in x resb then aux xs resb (x::res) else aux xs resb res
  in (**aux resa resb []**)
    match resa with 
      |l::ls -> begin 
          match resb with 
            |lb::lbs -> concat [l] (aux ls lbs [])
            |_-> failwith "erreur 7.1"
           end 
       |_ -> failwith "erreru 7.2"

let cartesian_l tuple_l =
  let rec aux tuple_l res =
    match tuple_l with
    |[] -> res
    |x::xs -> aux xs (cartesian x res)
  in aux (invert_liste tuple_l) []

      
let rec find tuple_ll cond =
  let table_c = cartesian_l (invert_liste tuple_ll) in 
  match cond with
  |And(a,b) -> intersec (find tuple_ll a) (find tuple_ll b)
  |Or(a,b) -> concat (find tuple_ll a) (find tuple_ll b)
  |Relation(a,b,c) -> parcours_simple a b c table_c
  |_ -> failwith "erreur, le In et le Nin ne doivent pas être présent"
     
let minu taba tabb =
  match taba with
  |[] -> failwith "erreur 9"
  |la::las ->
    begin
      match tabb with
      |[] -> failwith "erreur 10"
      |lb::lbs -> 
        let rec aux taba tabb res =
          match taba with
          |[] -> res
          |x::xs -> if is_in x tabb then aux xs tabb res else aux xs tabb (x::res)
        in
        let res_fin=aux las lbs []
        in if la=lb then (la::res_fin) else raise Wrong_type
    end

let unio taba tabb =
  match tabb with
  |[] -> taba
  |lb::lbs ->
    let interm= minu taba tabb in
    begin
      match interm with
      |[] -> tabb
      |la::las -> if la=lb then (la::(concat las lbs)) else raise Wrong_type
    end

let traite_as col_l =
  let rec aux col_l col_l_id new_col =
    match col_l with
    |[] -> (invert_liste col_l_id,invert_liste new_col)
    |x::xs ->
      begin
        match x with
        |Col(c) -> aux xs (c::col_l_id) (c::new_col)
        |Rename(Id(aa,ab),b) -> aux xs (Id(aa,ab)::col_l_id) (Id(aa,b)::new_col)
        |MinCol(Id(a,b)) -> aux xs (Id(a,b)::col_l_id) (Id(a,"min("^b^")")::new_col)
        |MinRename(Id(aa,ab),b) -> aux xs (Id(aa,ab)::col_l_id) (Id(aa,b)::new_col)
        |MaxCol(Id(a,b)) -> aux xs (Id(a,b)::col_l_id) (Id(a,"max("^b^")")::new_col)
        |MaxRename(Id(aa,ab),b) -> aux xs (Id(aa,ab)::col_l_id) (Id(aa,b)::new_col)
        |SumCol(Id(a,b)) -> aux xs (Id(a,b)::col_l_id) (Id(a,"sum("^b^")")::new_col)
        |SumRename(Id(aa,ab),b) -> aux xs (Id(aa,ab)::col_l_id) (Id(aa,b)::new_col)
        |_ -> failwith "error traite_as "
      end
  in aux col_l [] []
            
let change_name tab name =
  match tab with
  |[] -> []
  |x::xs -> name::xs
    
    
let rec orde tab col_l = match col_l with
	|[] -> tab
	|t::q-> tri (orde tab q) t    
	
    
let print_id1 id = match id with
  |Id(i1,i2) ->  begin print_string(i1); print_string ("."); print_string (i2) end
  |_ -> failwith " error print_id"
      
let rec indice x nom_l res=
    match nom_l with
    |[] -> -1
    |l::ls -> if l=x then res else indice x ls (res+1)
   
and extrac_col tab col_l =
  let (col_l_id,new_col)= traite_as col_l in
  let rec aux1 ligne n w =
    match ligne with
    |[] -> failwith "erreur 13"
    |x::xs -> if w=n then x else aux1 xs n (w+1)
  in
  let rec aux2 tab n res =
    match tab with
    |[] -> res
    |x::xs -> aux2 xs n ((aux1 x n 0)::res)
  in
  let rec aux3 nom_l col_l res =
    match col_l with
    |[] -> res
    |x::xs -> let ind = indice x nom_l 0 in
              if ind > (-1) then
                aux3 nom_l xs ((indice x nom_l 0)::res)
              else
                aux3 nom_l xs res
  in
  let rec aux4 tab col_n res =
    match col_n with
    |[] -> res
    |x::xs -> aux4 tab xs ((invert_liste(aux2 tab x []))::res)
  in
  match tab with 
  |[] -> ([],[])
  |tab_name::tab_r -> ((transpose (invert_liste(aux4 tab (aux3 tab_name col_l_id []) []))),new_col)

and print_col col = match col with 
  |[] -> ()
  |t::q -> match t with 
    |Col(id) -> begin print_id1 id; print_col q end
    |Rename(id,name) -> begin print_id1 id; print_string name; print_col q end
    |_ -> failwith "agregation pas encore implementé"

                    

let rec retourneop op = match op with 
  |Equal -> Nequal
  |Nequal -> Equal
  |Sinferieur -> Ssup
  |Ssup -> Sinferieur

let rec retourne b = match b with 
  |And(a1,b1) -> Or(retourne(a1),retourne(b1))
  |Or(a1,b1) -> And(retourne(a1),retourne(b1))
  |Relation(a1,op,b1) -> Relation(a1,(retourneop op),b1)
  |_ -> failwith ("pas de not in ou de in")

let rec final_cartesian tab_l =
  match tab_l with
  |[] -> []
  |(x,y)::xs -> cartesian y (final_cartesian xs)
  

let aboule x tab_l =
  let rec aux x tab_l reste =
    match tab_l with
    |[] -> failwith "la tab demandé n'existe pas !"
    |l::ls -> match l with
              |(lis,tab) -> if is_in x lis then
                              (lis,tab,concat ls reste)
                            else
                              aux x ls (l::reste)
  in aux x tab_l []

let cartesian_lex la taba lb tabb =
  match (la,lb) with
  |(x::xs,y::ys) -> if x<y then
                      cartesian taba tabb
                    else
                      cartesian tabb taba
  |_ -> failwith " nom des table est vide dans tab_l "

let opti_extrac tab_l l =
  match l with
  |[] -> failwith "error débile"
  |x::xs -> match xs with
            |[] -> aboule x tab_l
            |y::ys -> let (li,tab,reste) = aboule y tab_l in
                      if is_in x li then
                        (li,tab,reste)
                      else 
                      let (lj,tabj,restej) = aboule x reste in
                      (concat li lj,cartesian_lex li tab lj tabj,restej)
                   

let opti_extrac a b c tab_l la=
  let (l,tab,reste) = opti_extrac tab_l la in
  let new_tab = parcours_simple a b c tab in
  (l,new_tab)::reste
  
        
let opti_parcour a b c tab_l =
  match (a,c) with
  |(Id(la,na),Id(lc,nc)) -> if la=lc then
                              opti_extrac a b c tab_l [la]
                            else
                              opti_extrac a b c tab_l [la;lc]
  |(Id(la,na),Name(c2))-> opti_extrac a b c tab_l [la]
  |(Name(c2),Id(la,na)) -> opti_extrac a b c tab_l [la]
  |_ -> failwith "des nom de col ne sont pas des Id !!!"
      
let rec opti_find tab_l cond =
  match cond with
  |And(a,b) -> let tab_aux = (opti_find tab_l a) in
               (opti_find tab_aux b)
  |Or(a,b) -> let tab_aux = (opti_find tab_l a) in
              (opti_find tab_aux (retourne(b)))
  |Relation(a,b,c) -> opti_parcour a b c tab_l
  |_ -> failwith "erreur, le In et le Nin ne doivent pas être présent"

let rec is_in_col x col_l =
  match col_l with
  |[] -> false
  |y::ys -> match y with
            |MinRename(a,b) ->if a=x then true
                            else is_in_col x ys 
            |MinCol(a) -> if a=x then true
                       else is_in_col x ys
            |MaxRename(a,b) -> if a=x then true
                            else is_in_col x ys
            |MaxCol(a) -> if a=x then true
                       else is_in_col x ys
            |SumRename(a,b) ->if a=x then true
                            else is_in_col x ys
            |SumCol(a) -> if a=x then true
                       else is_in_col x ys
            |Rename(a,b) -> if a=x then true
                            else is_in_col x ys
            |Col(a) -> if a=x then true
                       else is_in_col x ys
                     
      
let rec opti_col col_l cond =
  match cond with
  |And(a,b) -> concat (opti_col col_l a) (opti_col [] b)
  |Or(a,b) -> concat (opti_col col_l a) (opti_col [] b)
  |Relation(a,b,c) -> if is_in_col c col_l then 
                        if is_in_col a col_l then
                          col_l
                        else
                          Col(a)::col_l
                      else
                        if is_in_col a col_l then
                          Col(c)::col_l
                        else
                          Col(c)::(Col(a)::col_l)
                        
  |_ -> failwith "In or Not_in non traitée dans l'interpreteur"

let rec opti_interpr file_l cond util tab_l =
  match file_l with
  |[] -> opti_find tab_l cond
  |x::xs ->
    begin
      match x with
      |Req(i,j) -> let (a,b) = extrac_col (rename (optimize i) j) util in
                   opti_interpr xs cond util (([j],a)::tab_l)
      |File(i,j) ->let (a,b) =extrac_col (rename (invert_liste(read_file i)) j) util in
                   opti_interpr xs cond util (([j],a)::tab_l)
    end
    
and optimize req =
  match req with
  |Minus(reqa,reqb) -> minu (optimize reqa) (optimize reqb)
  |Union(reqa,reqb) -> unio (optimize reqa) (optimize reqb) 
  |Where(col_l,file_l,cond) -> let util = opti_col col_l cond in
                               let (a,b) = extrac_col (final_cartesian (opti_interpr file_l cond util [])) col_l in
                               change_name a (invert_liste(b))
  |Order(req,col_l) -> orde (optimize req) col_l
  |Group(req,id,col_l) -> group (optimize req) id col_l
