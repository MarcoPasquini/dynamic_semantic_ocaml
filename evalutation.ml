INCLUDE "abstract_syntax.ml"

(* Runtime errors *)
exception RuntimeError of string

(* Language primitives *)
let is_zero(x) = match x with
| Int(v) -> Bool(v = 0)
| _ -> raise ( RuntimeError "Wrong type")
let int_eq(x,y) =
match (x, y) with
| (Int(v), Int(w)) -> Bool(v = w)
| (_,_) -> raise ( RuntimeError "Wrong type")
let int_plus(x, y) =
match (x, y) with
| (Int(v), Int(w)) -> Int(v + w)
| (_,_) -> raise ( RuntimeError "Wrong type")
let int_sub(x, y) =
match(x, y) with
| (Int(v), Int(w)) -> Int(v - w)
| (_,_) -> raise ( RuntimeError "Wrong type")
let int_times(x, y) =
match(x, y) with
| (Int(v), Int(w)) -> Int(v * w)
| (_,_) -> raise ( RuntimeError "Wrong type")
let int_div(x, y) =
match(x, y) with
| (Int(v), Int(w)) ->
if w<>0 then Int(v / w)
else raise (RuntimeError "Division by zero")
| (_,_) -> raise ( RuntimeError "Wrong type")
let less_than(x, y) =
match (x, y) with
| (Int(v), Int(w)) -> Bool(v < w)
| (_,_) -> raise ( RuntimeError "Wrong type")
let greater_than(x, y) =
match (x, y) with
| (Int(v), Int(w)) -> Bool(v > w)
| (_,_) -> raise ( RuntimeError "Wrong type")
let bool_and(x,y) =
match (x, y) with
| (Bool(v), Bool(w)) -> Bool(v && w)
| (_,_) -> raise ( RuntimeError "Wrong type")
let bool_or(x,y) =
match (x, y) with
| (Bool(v), Bool(w)) -> Bool(v || w)
| (_,_) -> raise ( RuntimeError "Wrong type")
let bool_not(x) =
match (x) with
| Bool(v) -> Bool(not(v))
| _ -> raise ( RuntimeError "Wrong type")

(* Interpreter *)
let rec eval (e:exp) (s:evT env) : evT =
  match e with
  | EInt(n) -> Int(n)
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | EString(s) -> String(s)
  | ECouple(e1, e2) -> Couple(eval e1 s, eval e2 s)
  | Den(i) -> (s i)
  | Concat(e1, e2) ->(
      let v1 = eval e1 s in
      let v2 = eval e2 s in
      match v1, v2 with
      | String s1, String s2 -> String (s1^s2)
      | _ -> raise ( RuntimeError "Wrong type")
    )
  | Prod(e1,e2) -> int_times((eval e1 s), (eval e2 s))
  | Sum(e1, e2) -> int_plus((eval e1 s), (eval e2 s))
  | Diff(e1, e2) -> int_sub((eval e1 s), (eval e2 s))
  | Div(e1, e2) -> int_div((eval e1 s), (eval e2 s))
  | IsZero(e1) -> is_zero (eval e1 s)
  | Eq(e1, e2) -> int_eq((eval e1 s), (eval e2 s))
  | LessThan(e1, e2) -> less_than((eval e1 s),(eval e2 s))
  | GreaterThan(e1, e2) -> greater_than((eval e1 s),(eval e2 s))
  | And(e1, e2) -> bool_and((eval e1 s),(eval e2 s))
  | Or(e1, e2) -> bool_or((eval e1 s),(eval e2 s))
  | Not(e1) -> bool_not(eval e1 s)
  | IfThenElse(e1,e2,e3) ->
  let g = eval e1 s in
  (match (g) with
  | Bool(true) -> eval e2 s
  | Bool(false) -> eval e3 s
  | _ -> raise ( RuntimeError "Wrong type")
  )
  | Let(i, e, ebody) -> eval ebody (bind s i (eval e s))
  | Fun(arg, ebody) -> Closure(arg,ebody,s)
  | DestrLet(i1, i2, e, ebody) -> (
      let c = eval e s in
      match c with
      | Couple(v1, v2) -> eval ebody (bind (bind s i1 v1) i2 v2)
      | _ -> raise ( RuntimeError "Wrong type")
    )
  | Letrec(f, arg, fBody, leBody) ->
  let benv = bind (s) (f) (RecClosure(f, arg, fBody,s)) in
  eval leBody benv
  | Apply(eF, eArg) ->
  let fclosure = eval eF s in
  (match fclosure with
  | Closure(arg, fbody, fDecEnv) ->
  let aVal = eval eArg s in
  let aenv = bind fDecEnv arg aVal in
  eval fbody aenv
  | RecClosure(f, arg, fbody, fDecEnv) ->
  let aVal = eval eArg s in
  let rEnv = bind fDecEnv f fclosure in
  let aenv = bind rEnv arg aVal in
  eval fbody aenv
  | _ -> raise ( RuntimeError "Wrong type")
  )
  