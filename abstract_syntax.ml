(* Identifiers *)
type ide = string;;

(* Types *)
type tname = TInt | TBool | TString | TClosure | TRecClosure | TUnBound

(* Abstract Expressions *)
type exp =
| EInt of int
| CstTrue
| CstFalse
| EString of string
| ECouple of exp * exp
| Den of ide
| Concat of exp * exp
| Sum of exp * exp
| Diff of exp * exp
| Prod of exp * exp
| Div of exp * exp
| IsZero of exp
| Eq of exp * exp
| LessThan of exp * exp
| GreaterThan of exp * exp
| And of exp * exp
| Or of exp * exp
| Not of exp
| IfThenElse of exp * exp * exp
| Let of ide * exp * exp
| DestrLet of ide * ide * exp * exp
| Letrec of ide * ide * exp * exp
| Fun of ide * exp
| Apply of exp * exp

(* Polymorphic environment *)
type 't env = ide -> 't

(* Evaluation types *)
type evT =
| Int of int
| Bool of bool
| String of string
| Couple of evT * evT
| Closure of ide * exp * evT env
| RecClosure of ide * ide * exp * evT env
| UnBound

(* Empty environment *)
let emptyenv = function x -> UnBound

(* Binding between a string x and a primitive value evT *)
let bind (s: evT env) (x: ide) (v: evT) =
function (i: ide) -> if (i = x) then v else (s i)
