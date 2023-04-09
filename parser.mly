%token <string> ID
%token LPAREN "("
%token RPAREN ")"
%token COMMA ","
%token GAP
%token EQUAL "="
%token COLON ":"
%token SLASH "/"
%token SEMI ";"
%token ARROW "->"
%token BAR "|"
%token QUOTE "'"
%token LBRACKET "["
%token RBRACKET "]"
%token LBRACE "{"
%token RBRACE "}"
%token EOF
%start <def list> prog

%right "/"
%right ";"
%right "(" // TODO: check if app has higher priority in e; e() 

(* highest precedence at the bottom *)


%{
open Syntax
%}

%%

let prog :=
  | gap; ~=terminated(def, gap)*; EOF; <>

let gap := GAP?; <>

let csep(x) :=
  gap; ~=separated_list(terminated(",", gap), terminated(x,gap)); <>

  (* gap x . gap  — reduce list or shift? *)

let lisp(x) :=
  | "["; l=csep(x); "]";
    { fun nil cons -> List.fold_right (fun a l -> cons a l) l nil }
  | "["; gap; l=x; gap; "|"; gap; r=x; gap; "]";
    { fun _nil cons -> cons l r }

let intercepts :=
  | gap; is=separated_nonempty_list(GAP, ID); gap; { [is] }
  | gap; { [] }
  | gap; is=separated_nonempty_list(GAP, ID); gap; ","; iss=intercepts; { is :: iss }
  | gap; ","; iss=intercepts; { [] :: iss }


let def :=
  | ~=ID; gap; "->"; gap; ~=exp; <DVal>
  | ~=ID; "("; ~=intercepts; ")"; gap; ":"; <DIntc>
  | id=ID; "("; pats=csep(pat); ")"; gap; "->"; gap; e=exp; { DClause (id, (pats, e)) }

let exp :=
  | "'"; ~=ID; <EAtom>
  | ~=ID; <EVar>
  | fn=exp; "("; args=csep(exp); ")"; <EApp>
  | l=exp; "/"; gap; r=exp; <EFst>
  | l=exp; ";"; gap; r=exp; <ESnd>
  | l=lisp(exp); { l (EAtom "") (fun car cdr -> ECons(car,cdr)) }

let pat :=
  | "{"; gap; ~=ID; gap; "}"; <PThunk>
  | "{"; gap; "'"; ~=ID; "("; ~=csep(vpat); ")"; gap; "->"; gap; ~=ID; gap; "}"; <PCmd>
  | ~=vpat; <PVpat>

let vpat :=
  | ~=ID; <VPVar>
  | "'"; ~=ID; <VPAtom>
  | "="; gap; ~=ID; <VPAtom>
  | l=lisp(vpat); {l (VPAtom "") (fun car cdr -> VPCons(car,cdr)) }
