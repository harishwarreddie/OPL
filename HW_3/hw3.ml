(* Type representing arithmetic expressions. *)
type aexp =
  | Int of int                 (* Constructor for n *)
  | Add of aexp * aexp          (* Constructor for a1 + a2 *)
  | Mul of aexp * aexp          (* Constructor for a1 * a2 *)

(* Exception for cases where no rule applies. *)
exception NoRuleApplies

(* Large-step semantics for arithmetic expressions. *)
let rec lstep_aexp = function
  | Int n -> Int n                                (* Base case: an integer is already a value. *)
  | Add (Int a1, Int a2) -> Int (a1 + a2)         (* If both operands are integers, return their sum. *)
  | Add (a1, a2) ->
      let v1 = lstep_aexp a1 in                   (* Evaluate left operand to a value. *)
      let v2 = lstep_aexp a2 in                   (* Evaluate right operand to a value. *)
      lstep_aexp (Add (v1, v2))                   (* Recursively apply addition. *)
  | Mul (Int a1, Int a2) -> Int (a1 * a2)         (* If both operands are integers, return their product. *)
  | Mul (a1, a2) ->
      let v1 = lstep_aexp a1 in                   (* Evaluate left operand to a value. *)
      let v2 = lstep_aexp a2 in                   (* Evaluate right operand to a value. *)
      lstep_aexp (Mul (v1, v2))                   (* Recursively apply multiplication. *)

(* Type representing boolean expressions. *)
type bexp =
  | True                                    (* Constructor for true. *)
  | False                                   (* Constructor for false. *)
  | Eq of aexp * aexp                       (* Constructor for a1 = a2. *)
  | Neq of aexp * aexp                      (* Constructor for a1 != a2. *)
  | Leq of aexp * aexp                      (* Constructor for a1 <= a2. *)
  | Gt of aexp * aexp                       (* Constructor for a1 > a2. *)
  | Neg of bexp                             (* Constructor for negation: !b. *)
  | And of bexp * bexp                      (* Constructor for conjunction: b1 && b2. *)

(* Large-step semantics for boolean expressions. *)
let rec lstep_bexp = function
  | True -> True
  | False -> False
  | Eq (a1, a2) ->
      let v1 = lstep_aexp a1 in               (* Evaluate left operand to a value. *)
      let v2 = lstep_aexp a2 in               (* Evaluate right operand to a value. *)
      (match (v1, v2) with
       | Int n1, Int n2 -> if n1 = n2 then True else False
       | _ -> raise NoRuleApplies)
  | Neq (a1, a2) ->
      let v1 = lstep_aexp a1 in
      let v2 = lstep_aexp a2 in
      (match (v1, v2) with
       | Int n1, Int n2 -> if n1 <> n2 then True else False
       | _ -> raise NoRuleApplies)
  | Leq (a1, a2) ->
      let v1 = lstep_aexp a1 in
      let v2 = lstep_aexp a2 in
      (match (v1, v2) with
       | Int n1, Int n2 -> if n1 <= n2 then True else False
       | _ -> raise NoRuleApplies)
  | Gt (a1, a2) ->
      let v1 = lstep_aexp a1 in
      let v2 = lstep_aexp a2 in
      (match (v1, v2) with
       | Int n1, Int n2 -> if n1 > n2 then True else False
       | _ -> raise NoRuleApplies)
  | Neg b ->
      let v = lstep_bexp b in
      (match v with
       | True -> False
       | False -> True
       | _ -> raise NoRuleApplies)
  | And (b1, b2) ->
      let v1 = lstep_bexp b1 in
      let v2 = lstep_bexp b2 in
      (match (v1, v2) with
       | True, True -> True
       | _, _ -> False)
