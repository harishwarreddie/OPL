type aexp =
  | Int of int
  | Add of aexp * aexp
  | Mul of aexp * aexp

exception NoRuleApplies

let rec sstep_aexp = function
  | Add (Int n1, Int n2) -> Int (n1 + n2)
  | Add (a1, a2) when not (match a1 with Int _ -> true | _ -> false) ->
      Add (sstep_aexp a1, a2)
  | Add (Int n1, a2) -> Add (Int n1, sstep_aexp a2)
  | Mul (Int n1, Int n2) -> Int (n1 * n2)
  | Mul (a1, a2) when not (match a1 with Int _ -> true | _ -> false) ->
      Mul (sstep_aexp a1, a2)
  | Mul (Int n1, a2) -> Mul (Int n1, sstep_aexp a2)
  | _ -> raise NoRuleApplies

type bexp =
  | True
  | False
  | Eq of aexp * aexp
  | Neq of aexp * aexp
  | Leq of aexp * aexp
  | Gt of aexp * aexp
  | Neg of bexp
  | And of bexp * bexp

let rec sstep_bexp = function
  | Eq (Int n1, Int n2) -> if n1 = n2 then True else False
  | Eq (a1, a2) when not (match a1 with Int _ -> true | _ -> false) ->
      Eq (sstep_aexp a1, a2)
  | Eq (Int n1, a2) -> Eq (Int n1, sstep_aexp a2)
  | Neq (Int n1, Int n2) -> if n1 != n2 then True else False
  | Neq (a1, a2) when not (match a1 with Int _ -> true | _ -> false) ->
      Neq (sstep_aexp a1, a2)
  | Neq (Int n1, a2) -> Neq (Int n1, sstep_aexp a2)
  | Leq (Int n1, Int n2) -> if n1 <= n2 then True else False
  | Leq (a1, a2) when not (match a1 with Int _ -> true | _ -> false) ->
      Leq (sstep_aexp a1, a2)
  | Leq (Int n1, a2) -> Leq (Int n1, sstep_aexp a2)
  | Gt (Int n1, Int n2) -> if n1 > n2 then True else False
  | Gt (a1, a2) when not (match a1 with Int _ -> true | _ -> false) ->
      Gt (sstep_aexp a1, a2)
  | Gt (Int n1, a2) -> Gt (Int n1, sstep_aexp a2)
  | Neg True -> False
  | Neg False -> True
  | Neg b -> Neg (sstep_bexp b)
  | And (True, True) -> True
  | And (True, False) -> False
  | And (False, _) -> False
  | And (True, b2) -> And (True, sstep_bexp b2)
  | And (b1, b2) -> And (sstep_bexp b1, b2)
  | _ -> raise NoRuleApplies