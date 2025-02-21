(* Type definitions *)
type aexp =
  | Int of int
  | Add of aexp * aexp
  | Mul of aexp * aexp

type bexp =
  | True
  | False
  | Eq of aexp * aexp
  | Neq of aexp * aexp
  | Lt of aexp * aexp
  | Leq of aexp * aexp
  | Gt of aexp * aexp
  | Neg of bexp
  | And of bexp * bexp

exception NoRuleApplies

(* Small-step semantics for arithmetic expressions *)
let rec sstep_aexp = function
  | Int _ -> raise NoRuleApplies
  | Add (Int n1, Int n2) -> Int (n1 + n2)
  | Add (Int n1, a2) ->
      let a2' = sstep_aexp a2 in
      Add (Int n1, a2')
  | Add (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Add (a1', a2)
  | Mul (Int n1, Int n2) -> Int (n1 * n2)
  | Mul (Int n1, a2) ->
      let a2' = sstep_aexp a2 in
      Mul (Int n1, a2')
  | Mul (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Mul (a1', a2)

(* Small-step semantics for boolean expressions *)
let rec sstep_bexp = function
  | True | False -> raise NoRuleApplies
  | Eq (Int n1, Int n2) -> if n1 = n2 then True else False
  | Eq (Int n1, a2) ->
      let a2' = sstep_aexp a2 in
      Eq (Int n1, a2')
  | Eq (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Eq (a1', a2)
  | Neq (Int n1, Int n2) -> if n1 <> n2 then True else False
  | Neq (Int n1, a2) ->
      let a2' = sstep_aexp a2 in
      Neq (Int n1, a2')
  | Neq (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Neq (a1', a2)
  | Lt (Int n1, Int n2) -> if n1 < n2 then True else False
  | Lt (Int n1, a2) ->
      let a2' = sstep_aexp a2 in
      Lt (Int n1, a2')
  | Lt (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Lt (a1', a2)
  | Leq (Int n1, Int n2) -> if n1 <= n2 then True else False
  | Leq (Int n1, a2) ->
      let a2' = sstep_aexp a2 in
      Leq (Int n1, a2')
  | Leq (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Leq (a1', a2)
  | Gt (Int n1, Int n2) -> if n1 > n2 then True else False
  | Gt (Int n1, a2) ->
      let a2' = sstep_aexp a2 in
      Gt (Int n1, a2')
  | Gt (a1, a2) ->
      let a1' = sstep_aexp a1 in
      Gt (a1', a2)
  | Neg True -> False
  | Neg False -> True
  | Neg b ->
      let b' = sstep_bexp b in
      Neg b'
  | And (True, b) -> b
  | And (False, _) -> False
  | And (b1, b2) ->
      let b1' = sstep_bexp b1 in
      And (b1', b2)

(* Large-step semantics for arithmetic expressions *)
let rec lstep_aexp = function
  | Int n -> Int n
  | Add (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> Int (n1 + n2)
       | _ -> failwith "Invalid arithmetic expression")
  | Mul (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> Int (n1 * n2)
       | _ -> failwith "Invalid arithmetic expression")

(* Large-step semantics for boolean expressions *)
let rec lstep_bexp = function
  | True -> True
  | False -> False
  | Eq (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> if n1 = n2 then True else False
       | _ -> failwith "Invalid arithmetic expression")
  | Neq (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> if n1 <> n2 then True else False
       | _ -> failwith "Invalid arithmetic expression")
  | Lt (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> if n1 < n2 then True else False
       | _ -> failwith "Invalid arithmetic expression")
  | Leq (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> if n1 <= n2 then True else False
       | _ -> failwith "Invalid arithmetic expression")
  | Gt (a1, a2) ->
      (match (lstep_aexp a1, lstep_aexp a2) with
       | (Int n1, Int n2) -> if n1 > n2 then True else False
       | _ -> failwith "Invalid arithmetic expression")
  | Neg b -> (match lstep_bexp b with True -> False | False -> True)
  | And (b1, b2) ->
      match lstep_bexp b1 with
      | True -> lstep_bexp b2
      | False -> False

(* Multi-step semantics for arithmetic expressions *)
let rec multi_step_aexp a =
  try
    let a' = sstep_aexp a in
    multi_step_aexp a'
  with
    NoRuleApplies -> a

(* Multi-step semantics for boolean expressions *)
let rec multi_step_bexp b =
  try
    let b' = sstep_bexp b in
    multi_step_bexp b'
  with
    NoRuleApplies -> b

(* Equivalence proof for arithmetic expressions *)
let aexp_equivalence a =
  let large_step_result = lstep_aexp a in
  let multi_step_result = multi_step_aexp a in
  large_step_result = multi_step_result

(* Equivalence proof for boolean expressions *)
let bexp_equivalence b =
  let large_step_result = lstep_bexp b in
  let multi_step_result = multi_step_bexp b in
  large_step_result = multi_step_result