type rat = int * int
type var = string
type unary_op = Neg
type binary_op = Add | Sub | Mul | Div
type expr = Const of rat
          | UnOp of unary_op * expr
          | BinOp of binary_op * expr * expr
          | Var of var
          | Func of var * expr
          | Bind of var * expr * expr
          | App of expr * expr
          | Ite of expr * expr * expr
type value = Rat of rat | Fun of var * state * expr
and state = var -> value option


let string_of_binary_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
let pretty_string_of_binary_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
let rec ocaml_string_of_expr = function
  | Const (n, d) -> "Const (" ^ string_of_int n ^ ", " ^ string_of_int d ^ ")"
  | UnOp (Neg, e) -> "UnOp (Neg, " ^ ocaml_string_of_expr e ^ ")"
  | BinOp (b_op, e1, e2) -> "BinOp (" ^ string_of_binary_op b_op ^ ", " ^ ocaml_string_of_expr e1 ^ ", " ^ ocaml_string_of_expr e2 ^ ")"
  | Var v -> "Var \"" ^ v ^ "\""
  | Func (v, e) -> "Func (\"" ^ v ^ "\", " ^ ocaml_string_of_expr e ^ ")"
  | Bind (v, e1, e2) -> "Bind (\"" ^ v ^ "\", " ^ ocaml_string_of_expr e1 ^ ", " ^ ocaml_string_of_expr e2 ^ ")"
  | App (e1, e2) -> "App (" ^ ocaml_string_of_expr e1 ^ ", " ^ ocaml_string_of_expr e2 ^ ")"
  | Ite (i, t, e) -> "Ite (" ^ ocaml_string_of_expr i ^ ", " ^ ocaml_string_of_expr t ^ ", " ^ ocaml_string_of_expr e ^ ")"
let rec string_of_expr = function
  | Const (n, d) -> "Const (" ^ string_of_int n ^ ", " ^ string_of_int d ^ ")"
  | UnOp (Neg, e) -> "UnOp (Neg, " ^ string_of_expr e ^ ")"
  | BinOp (b_op, e1, e2) -> "BinOp (" ^ string_of_binary_op b_op ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Var v -> "Var " ^ v
  | Func (v, e) -> "Func (" ^ v ^ ", " ^ string_of_expr e ^ ")"
  | Bind (v, e1, e2) -> "Bind (" ^ v ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | App (e1, e2) -> "App (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Ite (i, t, e) -> "Ite (" ^ string_of_expr i ^ ", " ^ string_of_expr t ^ ", " ^ string_of_expr e ^ ")"
let rec pretty_string_of_expr = function
  | Const (n, d) -> "(" ^ string_of_int n ^ "/" ^ string_of_int d ^ ")"
  | UnOp (Neg, e) -> "- " ^ pretty_string_of_expr e
  | BinOp (b_op, e1, e2) -> pretty_string_of_expr e1 ^ " " ^ pretty_string_of_binary_op b_op ^ " " ^ pretty_string_of_expr e2
  | Var v -> v
  | Func (v, e) -> "fun " ^ v ^ " -> " ^ pretty_string_of_expr e
  | Bind (v, e1, e2) -> "let " ^ v ^ " = " ^ pretty_string_of_expr e1 ^ " in " ^ pretty_string_of_expr e2
  | App (e1, e2) -> pretty_string_of_expr e1 ^ " " ^ pretty_string_of_expr e2
  | Ite (i, t, e) -> "if " ^ pretty_string_of_expr i ^ " != 0 then " ^ pretty_string_of_expr t ^ " else " ^ pretty_string_of_expr e
let print_expr e = print_string @@ pretty_string_of_expr e; print_newline ()
