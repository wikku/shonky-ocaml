type exp = (*  e  *)
  | EVar of string (*  a  *)
  | EAtom of string (*  'a  *)
  | ECons of exp * exp (*  [ e | e ]  *) (*  [ e , .. ]  *)
  | EApp of exp * exp list (*  e( e , .. )  *)
  | ESnd of exp * exp (*  e; e  *)
  | EFst of exp * exp (*  e/ e  *)
  | EThunk of intercepts option * clause list
  | ELocal of def list * exp (*  {| d* |} e  *)
  | EText of (char, exp) Either.t list
and intercepts = (*  h  *)
  string list list (*  ( a * , .. )  *)
and clause = (*  c  *)
  pat list * exp (*  ( p , .. ) -> e  *)
and def = (*  d  *)
  | DVal of string * exp (*  a -> e  *)
  | DOp of string * intercepts option * clause list
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

module P=Presyntax

let rec exp : P.exp -> exp = function
  | EVar(s) -> EVar s
  | EAtom(s) -> EAtom s
  | ECons(e1,e2) -> ECons(exp e1, exp e2)
  | EApp(f,args) -> EApp(exp f, List.map exp args)
  | ESnd(e1,e2) -> ECons(exp e1, exp e2)
  | EFst(e1,e2) -> EFst(exp e1, exp e2)
  | EThunk(io,cs) -> EThunk(Option.map intercepts io, List.map clause cs)
  | ELocal(ds,e) -> ELocal(List.map def ds, exp e)
  | EText(cs) -> EText(List.map (Either.map ~left:Fun.id ~right:exp) cs)
and intercepts : P.intercepts -> intercepts =
  List.map
    (function P.UPat _ -> failwith "list of atoms expected" | UAtoms ss -> ss)
and clause : P.clause -> clause =
  fun (ps,e) ->
    List.map
      (function
        | P.UPat p -> pat p
        | UAtoms [a] -> PVpat (VPVar a)
        | UAtoms _ -> failwith "one var expected")
      ps,
    exp e
and def : P.def -> def = function
  | DVal(s,e) -> DVal(s, exp e)
  | DOp(sio, scs) ->
    let name = List.hd (List.map fst (Option.to_list sio) @ List.map fst scs) in
    List.iter (fun (s,_) -> assert (name = s)) scs;
    DOp(name,
        Option.map (fun i -> intercepts (snd i)) sio,
        List.map (fun c -> clause (snd c)) scs)
and pat : P.pat -> pat = function
  | PVpat(vp) -> PVpat(vpat vp)
  | PThunk(s) -> PThunk(s)
  | PCmd(s1,vps,s2) -> PCmd(s1, List.map vpat vps, s2)
and vpat : P.vpat -> vpat = function
  | VPVar(s) -> VPVar(s)
  | VPAtom(s) -> VPAtom(s)
  | VPCons(v1,v2) -> VPCons(vpat v1, vpat v2)
  | VPEq(s) -> VPEq(s)
  | VPText(cs) -> VPText(List.map (Either.map ~left:Fun.id ~right:vpat) cs)
