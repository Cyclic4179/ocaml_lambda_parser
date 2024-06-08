type rat = int * int (* num, denom *)
type var = string (* new *)
type unary_op = Neg
type binary_op = Add | Sub | Mul | Div
type expr = Const of rat
| UnOp of unary_op * expr
| BinOp of binary_op * expr * expr
| Var of var (* new *)
| Func of var * expr (* new *)
| Bind of var * expr * expr (* new *)
| App of expr * expr (* new *)
| Ite of expr * expr * expr (* new *)
type value = Rat of rat | Fun of var * state * expr (* new *)
and state = var -> value option (* new *)

val eval_expr : state -> expr -> value (* main exercise *)
val numerator : expr option (* bonus exercise *)

val pretty_print_mode : int
