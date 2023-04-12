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
  pat_or_atoms list (*  ( a * , .. )  *)
and clause = (*  c  *)
  pat_or_atoms list * exp (*  ( p , .. ) -> e  *)
and def = (*  d  *)
  | DVal of string * exp (*  a -> e  *)
  | DIntc of string * intercepts
  | DClause of string * clause
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
and pat_or_atoms =
  | UPat of pat
  | UAtoms of string list
