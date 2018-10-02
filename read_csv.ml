open String
open Str
open Expr
  
exception Null
(** lecture d'un fichier csv en entrée *)
let decoupe_ligne : string -> string list = fun s -> 
  Str.split (Str.regexp ",") s
     
let read_file nom = 
  let file = open_in nom in
  let rec to_idstring liste =
    match liste with
    |[] -> []
    |x::xs -> (Name(x))::(to_idstring xs)
  in
  
  let result=[to_idstring (decoupe_ligne(input_line file))]; in
  let rec auxi file result =
    try
      let result = (to_idstring (decoupe_ligne(input_line file)))::result in
      auxi file result;
    with End_of_file ->
         begin
           close_in file;
           result
         end
  in
  auxi file result


let rec print_tuple t = match t with
  |a::b ->
    begin
      match a with
      |Name(aa) ->
        begin print_string aa ;print_string "\t"; print_tuple b end
      |Id(i1,i2) -> begin print_string i1; print_string ("."); print_string i2; print_string(" "); print_tuple b end
    end
  | _ -> print_string "\n"

let rec print_tuple_col t = match t with
  |a::b -> begin print_tuple a; print_tuple_col b end
  |_ -> ()




let rev l =
 let rec aux l acc = match l with 
  |[] -> acc
  |t::q -> aux q (t::acc) in aux l []

let rec revmat l = match l with
 |[] -> []
 |t::q -> rev t :: revmat q

let rec transpose list =
  match list with
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
     (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss);; 

(*let _ = print_tuple_col(revmat(transpose (read_file "employes.csv"))) ;;*)

