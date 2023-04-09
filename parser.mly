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

%{
open Syntax
%}

%%

let prog :=
  | gap; ~=terminated(def, gap)*; EOF; <>

let gap := GAP?

let csep(x) :=
  gap; ~=separated_list(gap; ","; gap, x); gap; <>

let ncsep(x) :=
  gap; ~=separated_nonempty_list(gap; ","; gap, x); gap; <>

let lisp(x) :=
  | "["; l=csep(x); "]";
    { fun nil cons -> List.fold_right (fun a l -> cons a l) l nil }
  | "["; gap; l=x; gap; "|"; gap; r=x; gap; "]";
    { fun _nil cons -> cons l r }

let intercepts :=
  ~=ID; "("; ~=csep(separated_list(GAP, ID)); ")"; ":"; <>

let clause :=
  ~=ID; "("; ~=csep(pat); ")"; gap; "->"; gap; ~=exp; <>

let def :=
  | ~=ID; gap; "->"; gap; ~=exp; <DVal>
  | intercepts=intercepts?; gap; clauses=ncsep(clause); { DOp { intercepts; clauses } }


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
