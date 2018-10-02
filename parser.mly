%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Literal of int | And of expr*expr | Or of expr*expr *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <string> VAL FILE NUM NOM
%token IN NOT DOT COMMA SELECT FROM WHERE MINUS UNION AS OR AND QOT ORDER BY DESC ASC MIN MAX SUM GROUP
%token EQU NEQ SUP INF SSUP SINF
%token LPAREN RPAREN
%token EOF


%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */

%type <Expr.requete> main     /* on _doit_ donner le type associé au point d'entrée */

%%
/* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
   s EOF              { $1 }  
;


s:    
  | s ORDER BY stts             { Order($1,$4) }
  | SELECT atts FROM rels WHERE cond          { Where($2,$4,$6) }
  | SELECT atts FROM rels WHERE cond GROUP BY gtt        { Group(Where($2,$4,$6),$9,$2) }
  | LPAREN s RPAREN MINUS LPAREN s RPAREN     { Minus($2,$6) }
  | LPAREN s RPAREN UNION LPAREN s RPAREN     { Union($2,$6) }


gtt :
  |id DOT id { Id($1,$3) }

stts:
  | stt COMMA stts                  { $1::$3 }
  | stt                             { [$1] }
  
stt:
  | id DOT id DESC                  { Idd($1,$3) }
  | id DOT id ASC                   { Ida($1,$3) }
  | id DOT id                       { Ida($1,$3) }

atts:        
  | attd COMMA atts                 { $1::$3 }
  | attd                            { [$1] }


attd: 
  | MIN LPAREN att RPAREN      {MinCol($3)}
  | MIN LPAREN att RPAREN AS id   {MinRename($3,$6)}
  | MAX LPAREN att RPAREN      {MaxCol($3)}
  | MAX LPAREN att RPAREN AS id   {MaxRename($3,$6)}
  | SUM LPAREN att RPAREN      {SumCol($3)}    
  | SUM LPAREN att RPAREN AS id   {SumRename($3,$6)} 
  | att        	               { Col($1) }
  | att AS id                { Rename($1,$3) }
  
att:
  | id DOT id  { Id($1,$3) }
  | NUM        { Name($1) }
  | QOT VAL QOT        { Name($2) }

id:
  | VAL       { $1 }
  
  
rels:       
  | rels COMMA rel			       { $1@$3 }
  | rel			       { $1 }


rel:       
  | filename id                        { [File($1,$2)] }
  | LPAREN s RPAREN id                   { [Req($2,$4)] }

cond:       
  | and_cond OR cond		       { Or($1,$3) }
  | and_cond	       { $1 }
  
  
and_cond:
  | at_cond AND and_cond  {And($1,$3)}
  | at_cond { $1 }
  
at_cond:
  | att EQU att  {Relation($1,Equal,$3)}
  | att SINF att  {Relation($1,Sinferieur,$3)}
  | att IN LPAREN s RPAREN  {In($1,$4)}
  | att NOT IN LPAREN s RPAREN  {Nin($1,$5)}
  
filename:
  | QOT FILE QOT  { $2 }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
