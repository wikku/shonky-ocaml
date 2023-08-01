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
%right "("

(* highest precedence at the bottom *)

%{
open Syntax

type pat_or_atoms =
  | UPat of pat
  | UAtoms of string list

let validate_handled poas =
  List.map
    (function UAtoms a -> a | _ -> failwith "pattern in handle decl")
    poas

let validate_params poas =
  List.map
    (function
     | UAtoms [a] -> PVpat (VPVar a)
     | UPat p -> p
     | UAtoms _ -> failwith "pattern in handle decl")
    poas

let validate_dop (sio,scl) =
  let name = List.hd (List.map fst (Option.to_list sio) @ List.map fst scl) in
  DOp(name, Option.fold ~some:snd ~none:[] sio,
      List.map
        (fun (n,c) ->
         if name <> n then failwith "different names in definition" else c)
        scl)
%}

%%


let prog :=
  | gap; ~=def*; EOF; <>

let gap := GAP?; <>

let iloption(x) ==
  | (* nothing *) { [] }
  | ~=x; <>

let csep(x) :=
  gap; ~=separated_list(terminated(",", gap), terminated(x,gap)); <>

let lisp(x) :=
  | "["; l=csep(x); "]";
    { fun nil cons -> List.fold_right (fun a l -> cons a l) l nil }
  | "["; gap; l=x; gap; "|"; gap; r=x; gap; "]";
    { fun _nil cons -> cons l r }

let poas := separated_nonempty_list(",", preceded(gap, pat_or_atoms))
let handled == ~=poas; < validate_handled >
let params == ~=poas; < validate_params >

let handle_decl := "("; ~=handled; ")"; gap; ":"; <>
let clause := "("; ~=params; ")"; gap; "->"; gap; ~=exp; <>
let named_handle_decl := ~=ID; ~=handle_decl; <>
let named_clause := ~=ID; ~=clause; <>

let def :=
  | ~=ID; gap; "->"; gap; ~=exp; gap; <DVal>
  | ~=ioption(terminated(named_handle_decl, gap));
    ~=separated_nonempty_list(terminated(",", gap), terminated(named_clause, gap)); <validate_dop>

let exp :=
  | "'"; ~=ID; <EAtom>
  | ~=ID; <EVar>
  | fn=exp; "("; args=csep(exp); ")"; <EApp>
  | l=exp; "/"; gap; r=exp; <EFst>
  | l=exp; ";"; gap; r=exp; <ESnd>
  | l=lisp(exp); { l (EAtom "") (fun car cdr -> ECons(car,cdr)) }
  | "{"; gap; ~=exp; gap; "}"; { EThunk([],[[],exp]) }
  | "{"; gap;
    ~=iloption(terminated(handle_decl, gap));
    ~=separated_nonempty_list(terminated(",", gap), terminated(clause, gap));
    "}"; <EThunk>

let pat_(outer_vpat) :=
  | "{"; gap; ~=ID; gap; "}"; <PThunk>
  | "{"; gap; "'"; ~=ID; "("; ~=csep(vpat); ")"; gap; "->"; gap; ~=ID; gap; "}"; <PCmd>
  | ~=outer_vpat; <PVpat>

let pat0 := pat_(vpat0)

let vpat0 :=
  | "'"; ~=ID; <VPAtom>
  | "="; gap; ~=ID; <VPEq>
  | l=lisp(vpat); {l (VPAtom "") (fun car cdr -> VPCons(car,cdr)) }

let pat_or_atoms :=
  | ~=pat0; gap; <UPat>
  | ~=list(terminated(ID, gap)); <UAtoms>

let vpat :=
  | ~=vpat0; <>
  | ~=ID; <VPVar>
