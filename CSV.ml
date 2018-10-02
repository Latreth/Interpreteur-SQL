exception Fichier_mal_forme

let list_of_ligne ligne =
  (Str.split (Str.regexp ";") ligne)

let lire_csv f =
  let entree = (open_in f)
  and resultat = ref []
  in begin
  try
    let ligne = ref  (list_of_ligne (input_line entree))
    in
    let nb_champs = (List.length !ligne)
    in
    while true do
      if (List.length !ligne) != nb_champs then
	raise Fichier_mal_forme
      else
	resultat := List.append !resultat [ !ligne ];
      ligne := (list_of_ligne (input_line entree))
    done
  with
    | End_of_file -> close_in entree
  end;
  !resultat


let liste_string_vers_liste_entier ll =
  let l = ref ll
  and r = ref []
  in
  while !l <> [] do
    try
      r := List.append !r [ (int_of_string (List.hd !l)) ];
      l := (List.tl !l)
    with
      | _ -> raise Not_found
  done;
  !r

let lire_csv_entiers f =
  let entree = (open_in f)
  and resultat = ref []
  in begin
  try
    let ligne = ref (liste_string_vers_liste_entier (list_of_ligne (input_line entree)))
    in
    let nb_champs = (List.length !ligne)
    in
    while true do
      if (List.length !ligne) != nb_champs then
	raise Fichier_mal_forme
      else
	resultat := List.append !resultat [ !ligne ];
      ligne := (liste_string_vers_liste_entier (list_of_ligne (input_line entree)))
    done
  with
    | End_of_file -> close_in entree
  end;
  !resultat;;
  
lire_csv test.csv
