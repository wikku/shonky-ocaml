{
open Parser
}

rule token = parse
  | ['\t'-'\r']+ { GAP }
  | ['A'-'Z' 'a'-'z' '0'-'9']+ as id { ID id }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '=' { EQUAL }
  | ':' { COLON }
  | '/' { SLASH }
  | ';' { SEMI }
  | "->" { ARROW }
  | '|' { BAR }
  | "'" { QUOTE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | eof { EOF }
