
open Ast
open Helper

exception TypeError
exception UnificationError

let rec occurs (v : var) (t : typ) : bool =
  match t with
  | TVar v' -> v = v'
  | TPair (t1, t2) | TArrow (t1, t2) -> occurs v t1 || occurs v t2
  | _ -> false

let rec subst_typ (s : subst) (t : typ) : typ =
  match t with
  | TVar v -> (try VarMap.find v s with Not_found -> t)
  | TArrow (t1, t2) -> TArrow (subst_typ s t1, subst_typ s t2)
  | TPair (t1, t2) -> TPair (subst_typ s t1, subst_typ s t2)
  | _ -> t

let subst_constr (c : constr) (v : var) (t : typ) : constr =
  Constr.map (fun (t1, t2) -> (subst_typ (VarMap.singleton v t) t1, subst_typ (VarMap.singleton v t) t2)) c

  let rec unify (c0 : constr) : subst =
    if Constr.is_empty c0 then VarMap.empty
    else
      let (t1, t2) = Constr.choose c0 in
      let rest = Constr.remove (t1, t2) c0 in
      if t1 = t2 then unify rest
      else match (t1, t2) with
        | (TVar v, t) | (t, TVar v) ->
            if occurs v t then raise UnificationError
            else
              let s = VarMap.singleton v t in
              let updated_rest = Constr.map (fun (a, b) -> (subst_typ s a, subst_typ s b)) rest in
              VarMap.union (fun _ _ y -> Some y) s (unify updated_rest)
        | (TArrow (t1a, t1b), TArrow (t2a, t2b))
        | (TPair (t1a, t1b), TPair (t2a, t2b)) ->
            unify (Constr.add (t1a, t2a) (Constr.add (t1b, t2b) rest))
        | _ -> raise UnificationError

        let rec check (g : context) (e0 : exp) : typ * constr =
          match e0 with
          | Var x ->
              (try (VarMap.find x g, Constr.empty)
               with Not_found -> raise TypeError)
          | Int _ -> (TInt, Constr.empty)
          | True | False -> (TBool, Constr.empty)
          | Lam (x, e) ->
              let tv = next_tvar () in
              let (t_body, c_body) = check (VarMap.add x tv g) e in
              (TArrow (tv, t_body), c_body)
          | App (e1, e2) ->
              let (t1, c1) = check g e1 in
              let (t2, c2) = check g e2 in
              let tv = next_tvar () in
              (tv, Constr.union c1 (Constr.union c2 (Constr.singleton (t1, TArrow (t2, tv)))))
          | Let (x, e1, e2) ->
              let (t1, c1) = check g e1 in
              let (t2, c2) = check (VarMap.add x t1 g) e2 in
              (t2, Constr.union c1 c2)
          | Plus (e1, e2) | Times (e1, e2) | Minus (e1, e2) ->
              let (t1, c1) = check g e1 in
              let (t2, c2) = check g e2 in
              (TInt, Constr.union c1 (Constr.union c2 (Constr.of_list [(t1, TInt); (t2, TInt)])))
          | Pair (e1, e2) ->
              let (t1, c1) = check g e1 in
              let (t2, c2) = check g e2 in
              (TPair(t1,t2), Constr.union c1 c2)
          | Fst e ->
              let(t,c)=check g e in 
              let tv1=next_tvar() in 
              let tv2=next_tvar() in 
             tv1 ,Constr.add(t ,TPair(tv1,tv2))c 
          | Snd e ->
              let(t,c)=check g e in 
              let tv1=next_tvar() in 
              let tv2=next_tvar() in 
             tv2 ,Constr.add(t ,TPair(tv1,tv2))c 
          | Eq(e1,e2) ->
              let(t1,c1)=check g e1 in 
              let(t2,c2)=check g e2 in 
             (TBool ,Constr.union c1(Constr.union c2(Constr.of_list [(t1,TInt);(t2,TInt)])))
          | If(e1,e2,e3) ->
              let(t1,c1)=check g e1 in 
              let(t2,c2)=check g e2 in 
              let(t3,c3)=check g e3 in 
             (t2 ,Constr.union c1(Constr.union c2(Constr.union c3(Constr.of_list [(t1,TBool);(t2,t3)]))))