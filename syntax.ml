type exp = (*  e  *)
  | EVar of string (*  a  *)
  | EAtom of string (*  'a  *)
  | ECons of exp * exp (*  [ e | e ]  *) (*  [ e , .. ]  *)
  | EApp of exp * exp list (*  e( e , .. )  *)
  | ESnd of exp * exp (*  e; e  *)
  | EFst of exp * exp (*  e/ e  *)
  | ELocal of def list * exp (*  {| d* |} e  *)
  | EText of (char, exp) Either.t list
and intercepts = (*  h  *)
  string list list (*  ( a * , .. )  *)
and clause = (*  c  *)
  pat list * exp (*  ( p , .. ) -> e  *)
and def = (*  d  *)
  | DVal of string * exp (*  a -> e  *)
  | DOp of
    { intercepts: (string * intercepts) option
    ; clauses: (string * pat list * exp) list
    } (*  ah? ac , ..  *)
and pat = (*  p  *)
  | PVpat of vpat (*  q  *)
  | PThunk of string (*  { a }  *)
  | PCmd of string * vpat list * string (*  { 'a( q , .. ) -> a }  *)
and vpat = (*  q  *)
  | VPVar of string (*  a  *)
  | VPAtom of string (*  'a  *)
  | VPCons of vpat * vpat (*  [ q | q ]  *) (*  [ q , .. ]  *)
  | VPEq of string (*  = a  *)
  | VPText of (char, vpat) Either.t list

