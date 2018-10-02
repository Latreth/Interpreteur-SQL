{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']   { token lexbuf }    (* on saute les blancs, les tabulations et les retours chariots *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | "IN"            { IN }
  | "NOT"           { NOT }
  | "\""            { QOT } 
  | "="             { EQU }
  | '<'             { SINF }
  | "."             { DOT }
  | ","             { COMMA }
  | "SELECT"        { SELECT }
  | "ORDER"         { ORDER }
  | "BY"            { BY }
  | "MIN"           { MIN }
  | "MAX"           { MAX }
  | "SUM"           { SUM }
  | "GROUP"         { GROUP }
  | "DESC"          { DESC }
  | "ASC"           { ASC }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "OR"            { OR }
  | "AND"           { AND }
  | "FROM"          { FROM }
  | "WHERE"         { WHERE }
  | "MINUS"         { MINUS }
  | "UNION"         { UNION }
  | "AS"            { AS }
  | ['A' - 'Z' 'a'-'z'] ['A'-'Z' '0'-'9' 'a'-'z' '_']* as s     { VAL(s) }
  | ['0'-'9']* as s { NUM(s) }
  | ['A' - 'Z' 'a' -'z' '\\' - '_' '0' - '9']* ".csv" as s   {FILE(s)}
  | eof             { EOF } 
