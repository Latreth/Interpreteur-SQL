open Expr
open Read_csv
open Optimize

let lexbuf c = Lexing.from_string c

let parse c = Parser.main Lexer.token (lexbuf c)
exception Wrong

(**
(** affichage de parsage**)
let rec afficher requet = match requet with 
  |Where(col,tab,cond) -> begin print_string ("Where("); print_column_list col; print_string(", "); print_filename_list tab; print_string(", "); print_cond cond; print_string(")")end 
  |Union(tab1,tab2) ->begin print_string("Union("); afficher tab1; print_string(", "); afficher tab2; print_string(")") end
  |Minus(tab1,tab2) ->begin print_string("Minus("); afficher tab1; print_string(", "); afficher tab2; print_string(")") end

and print_cond cond = match cond with 
  |And(cond1,cond2) -> begin print_string ("And("); print_cond cond1; print_cond cond2; print_string(")") end
  |Or(cond1,cond2) -> begin print_string("Or("); print_cond cond1; print_cond cond2; print_string(")") end
  |Relation(id1,Equal,id2) -> begin print_string("Rel(");print_id (id1); print_string(", Equal, "); print_id(id2); print_string(")") end
  |Relation(id1,Sinferieur,id2) -> begin print_string("Rel(");print_id (id1); print_string(", Sinferieur, "); print_id(id2); print_string(")") end
  |In(id1,s) -> begin print_string ("In ");print_id id1;  print_string(", "); afficher s; print_string (")") end
  |Nin(id1,s) -> begin print_id id1; print_string (" NOT IN "); print_string("("); afficher s; print_string (")") end
  
and print_id id = match id with
  |Id(i1,i2) ->  begin print_string(i1); print_string ("."); print_string (i2) end

  
and print_column col = match col with
  |Rename(Id(i1,i2),s) -> begin print_string(i1); print_string ("."); print_string (i2); print_string (" AS "); print_string s end
  |Col(Id(i1,i2)) -> begin print_string(i1); print_string ("."); print_string (i2) end
  |_ -> raise Wrong
  
and print_column_list l = match l with 
  |[] -> ()
  |t::q -> if q!= [] then begin print_column t; print_string (", "); print_column_list q end else begin print_column t; print_column_list q end
  
and print_filename tab = match tab with
  |Req(requet,s) -> begin print_string ("("); afficher requet; print_string (")"); print_string s end
  |File(file,s) -> begin print_string "File( ";print_string file; print_string(","); print_string s; print_string(")") end

and print_filename_list l = match l with
  |[] -> ()
  |t::q -> if q != [] then begin print_filename t; print_string (", "); print_filename_list q end else begin print_filename t;  print_filename_list q end **)


    let rec normalize_req (req : requete) : requete =
        let rec analyse_cond c =
            match c with
            | And(c1, c2) -> let tables1, cond1, condaux1 = analyse_cond c1 in
                             let tables2, cond2, condaux2 = analyse_cond c2 in
                             let condaux = match condaux1, condaux2 with
                                | None, None -> None
                                | None, Some x -> Some x
                                | Some x, None -> Some x
                                | Some x, Some y -> Some (And(x, y))
                             in
                             tables1 @ tables2, And(cond1, cond2), condaux
            | Or(c1, c2) -> let tables1, cond1, condaux1 = analyse_cond c1 in
                            let tables2, cond2, condaux2 = analyse_cond c2 in
                            let condaux = match condaux1, condaux2 with
                                | None, None -> None
                                | None, Some x -> Some x
                                | Some x, None -> Some x
                                | Some x, Some y -> Some (And(x, y))
                            in
                            tables1 @ tables2, Or(cond1, cond2), condaux
            | Relation(_,_,_) -> [], c, None
            | In(id, table) -> let table = normalize_req table in
                                begin
                                    match table with
                                    | Where(c,ltable,cond) ->
                                            if (List.length c) = 1 then
                                                let c = match List.hd c with | Col(x) -> x | Rename(x, _) -> x | _ -> failwith "idk yet"in
                                                ltable, Relation(c, Equal, id), Some cond
                                            else
                                                failwith "Error in the query"
                                    | _ -> failwith "I don't how to normalize this query"
                                end
            | Nin (_, _) -> failwith "Il ne devrait à cette étape plus y avoir de NOT IN. Si c'est le cas c'est que la requête ne peut pas être traîtée."
        in
        match req with
	| Order(r, col_l) -> Order(normalize_req r, col_l)
	| Group(r, col, col_l) -> Group(normalize_req r, col, col_l)
        | Union(r1, r2) -> Union(normalize_req r1, normalize_req r2)
        | Minus(r1, r2) -> Minus(normalize_req r1, normalize_req r2)
        | Where(lc,ltable,cond) ->
                let newtable, newcond, condaux = analyse_cond cond in
                let t =
                    let analyse_col x =
                        match x with
                        | Req(r, name) -> Req(normalize_req r, name)
                        | _ -> x
                    in
                    List.map analyse_col (ltable @ newtable)
                in
                let c = match condaux with
                    | None -> newcond
                    | Some x -> And(newcond, x)
                in
                Where (lc,t,c)


    let rec check_DNF cond =
        let rec clause = function
            | Or(c1, c2) -> (clause c1) && (clause c2)
            | And(c1, c2) -> false
            | Relation(_,_,_) -> true
            | In(_, _) -> true
            | Nin(_,_) -> true
        in
        match cond with
        | And(c1, c2) -> (check_DNF c1) && (check_DNF c1)
        | _ -> clause cond


    let rec delete_notin req =
        let rec sup_notin cond =
            match cond with
            | And(c1, c2) -> let cond1, cond2, b1 = sup_notin c1 in
                             let cond3, cond4, b2 = sup_notin c2 in
                             And(cond1, cond3), And(cond2, cond4), b1 || b2
            | Or(c1, c2) -> let cond1, cond2, b1 = sup_notin c1 in
                            let cond3, cond4, b2 = sup_notin c2 in
                            Or(cond1, cond3), Or(cond2, cond4), b1 || b2
            | Relation(_,_,_) -> cond, cond, false
            | In(x,y) -> let y' = delete_notin y in
                         In(x, y') , In(x, y'), false
            | Nin(x,y) -> let y' = delete_notin y in
                            Relation(x, Equal, x), In(x, y'), true
        in
        match req with
	| Order(r, col_l) -> Order(delete_notin r, col_l)
	| Group(r, col, col_l) -> Group(delete_notin r, col, col_l)
        | Union(r1, r2) -> Union(delete_notin r1, delete_notin r2)
        | Minus(r1, r2) -> Minus(delete_notin r1, delete_notin r2)
        | Where(lc,ltable,cond) -> begin
                let cond1, cond2, b = sup_notin cond in
                let t =
                    let analyse_col x =
                        match x with
                        | Req(r, name) -> Req(delete_notin r, name)
                        | _ -> x
                    in
                    List.map analyse_col ltable
                in

                match b with
                | true -> if check_DNF cond then
                              Minus(Where(lc,t,cond1),
                                    Where(lc,t,cond2))
                          else failwith "Je ne peux rien faire"
                | false -> req end

let _ =
    let argc = Array.length Sys.argv in
    for i = 1 to argc - 1 do
        let file = open_in Sys.argv.(i) in
        let file_name = String.sub Sys.argv.(i) 0 (String.index Sys.argv.(i) '.') in
        let csvfile = file_name^".csv" in
        let table = [Name(file_name)] :: Read_csv.revmat(Read_csv.transpose(Read_csv.read_file csvfile)) in
        let _ = Read_csv.print_tuple_col(table) in
        print_string ("File " ^ file_name ^ ".csv loaded\n");
        Pervasives.close_in file
    done;

    while true do
        Printf.printf "> ";
        flush_all ();
        let x = input_line stdin in
        let requeteparsee = normalize_req(delete_notin (parse x)) in
        (**afficher requeteparsee**)
        (**Interpreteur.print_table  (Interpreteur.compute requeteparsee)**)
        (**Read_csv.print_tuple_col (Algebra_engine.algebra_engine requeteparsee) **)
        Read_csv.print_tuple_col (Optimize.optimize requeteparsee)
    done;
