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
//%token RPARENGAPCOLON "):"
//%token RPARENARROW ")->"
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

let lisp(x) :=
  | "["; l=csep(x); "]";
    { fun nil cons -> List.fold_right (fun a l -> cons a l) l nil }
  | "["; gap; l=x; gap; "|"; gap; r=x; gap; "]";
    { fun _nil cons -> cons l r }

let params := separated_nonempty_list(",", preceded(gap, pat_or_atoms))

let handle_decl := "("; ~=params; ")"; gap; ":"; <>
let clause := "("; ~=params; ")"; gap; "->"; ~=exp; <>

let def :=
  | ~=ID; gap; "->"; gap; ~=exp; <DVal>
  | ~=ID; ~=handle_decl; <DIntc>
  | ~=ID; ~=clause; <DClause>

let exp :=
  | "'"; ~=ID; <EAtom>
  | ~=ID; <EVar>
  | fn=exp; "("; args=csep(exp); ")"; <EApp>
  | l=exp; "/"; gap; r=exp; <EFst>
  | l=exp; ";"; gap; r=exp; <ESnd>
  | l=lisp(exp); { l (EAtom "") (fun car cdr -> ECons(car,cdr)) }

let pat_(vpat) :=
  | "{"; gap; ~=ID; gap; "}"; <PThunk>
  | "{"; gap; "'"; ~=ID; "("; ~=csep(vpat); ")"; gap; "->"; gap; ~=ID; gap; "}"; <PCmd>
  | ~=vpat; <PVpat>

let pat := pat_(vpat)
let pat0 := pat_(vpat0)

let vpat0 :=
  | "'"; ~=ID; <VPAtom>
  | "="; gap; ~=ID; <VPAtom>
  | l=lisp(vpat); {l (VPAtom "") (fun car cdr -> VPCons(car,cdr)) }

let pat_or_atoms :=
  | ~=pat0; gap; <UPat>
  | ~=list(terminated(ID, gap)); <UAtoms>

let vpat :=
  | ~=vpat0; <>
  | ~=ID; <VPVar>
