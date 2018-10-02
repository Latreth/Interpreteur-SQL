open Algebra_engine

   
let tab= [[ Id("a","nom");Id("a","prenom");Id("a","sex");Id("a","classe")];
          [Name("Perotin");Name("Lucas");Name("M");Name("M1IF")];
          [Name("Rodriguez");Name("Emmanuel");Name("M");Name("M1IF")]]


let tabb= [[ Id("b","nom");Id("b","prenom");Id("b","sex");Id("b","classe")];
          [Name("Rodriguez");Name("Lucie");Name("F");Name("CAP coiffure")];
          [Name("Rodriguez");Name("Titi");Name("M");Name("boulot")]]
       
let col_l = [Col(Id("a","prenom"));Col(Id("b","classe"))] 
let tab_name=[ Id("a","nom");Id("a","prenom");Id("a","sex");Id("a","classe")]
;;
  trouve (Id("a","prenom")) tab_name ;;
  trouve (Id("a","sex")) tab_name ;;

    extra tab_name 1;;
      extra tab_name 2;;

let nom = [Name("nom");Name("prenom");Name("sex");Name("classe")];;
  rename [nom] "a";;

  let a=[1;2;3;4]
  let b=[5;6;7];;
    invert_liste a;;
      concat a b;;

      let c=[[1;2];[1;1]];;
        cartesian tab tab;;

          cond_ok (Val(1)) Sinferieur (Val(3));;
            cond_ok (Name("bob")) Equal (Name("Emmanuel"));;

              parcours_simple (Id("a","prenom")) Equal (Id("a","prenom")) tab ;;
                parcours_simple (Id("a","prenom")) Equal (Name("Emmanuel")) tab;;
                  
                  parcours_simple (Id("a","nom")) Equal (Id("b","nom")) (cartesian_l [tab; tabb]) ;;

                      is_in (Id("a","prenom")) tab_name;;
                        intersec tab [tab_name];;

                          cartesian_l [tab;tabb];;

                            find [tab;tabb] (Rel(Id("a","nom"),Equal,Id("b","nom")));;
let tuple_ll = [(tab);(tabb)]
let cond =Rel(Id("a","nom"),Equal,Id("b","nom"))
let file_l=[File ("test.csv","a");File ("testb.csv","b")];;
  extrac_col tab col_l ;;
    find tuple_ll cond ;;
      read_file "testb.csv";;
        rename (read_file "testb.csv") "b";;
          rename (invert_liste (read_file "test.csv")) "a";;

            let tmp =simple_interpr file_l cond [];;
              col_to_id col_l [];;
                extrac_col tmp col_l;;
interpr (Where(col_l,file_l,cond ));;
                
  Read_csv.print_tuple_col(algebra_engine (Where(col_l,file_l,cond )));;  
