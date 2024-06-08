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

(** Change this if you want Artemis to show you non-prettified printouts of expressions.
  - 0 (default): fully prettified
  - 1: prettified but with all brackets
  - 2: as OCaml expressions *)
let pretty_print_mode = 0

(* since you already implemented rational expressions last week,
we've added the sample solution for that part here; expand and adapt as needed! *)

(** greatest common denominator of x and y *)
let gcd x y =
  (* gcd': assume x >= y >= 0 and x > 0 *)
  let rec gcd' x = function
    | 0 -> x
    | y -> gcd' y (x mod y)
  in
  match (abs x, abs y) with
    | (0, 0) -> failwith "gcd(0, 0) is undefined"
    | (x', y') when x' > y' -> gcd' x' y'
    | (x', y') -> gcd' y' x'

(** negate a rational *)
let negate (n, d) = (-n, d)

(** take the reciprocal of a rational *)
let reciprocal = function
  | (0, _) -> failwith "divide by zero"
  | (n, d) -> (d, n)

(** input: any rat (n, d) with d <> 0
    output: a rat (n', d') with gcd n' d' = 1 and d' > 0 and n*d' = n'*d *)
let rec simplify = function
  | (_, 0) -> failwith "cannot simplify invalid rational number: denominator is zero"
  | (0, _) -> (0, 1)
  | (n, d) when d < 0 -> simplify (-n, -d)
  | (n, d) -> let f = gcd n d in (n / f, d / f)

let add (n1, d1) (n2, d2) = simplify (n1*d2 + n2*d1, d1*d2)
let sub x y = add x (negate y)
let mul (n1, d1) (n2, d2) = simplify (n1*n2, d1*d2)
let div x y = mul x (reciprocal y)

(** your task begins here! *)
let rec eval_expr (st : state) = function
  | Const r -> Rat (simplify r)
  | UnOp (Neg, e) -> (match eval_expr st e with
    | Rat r -> Rat (negate r)
    | Fun _ -> failwith "type error: attempt to negate function!")
  | BinOp (op, e1, e2) -> (match eval_expr st e1, eval_expr st e2 with
    | Rat r1, Rat r2 -> Rat ((match op with Add -> add | Sub -> sub | Mul -> mul | Div -> div) r1 r2)
    | (Fun _ | Rat _), (Fun _ | Rat _) -> failwith "type error: attempt to add/subtract/multiply/divide function(s)!")
  | Var v ->
          begin match st v with
          | Some x -> x
          | None -> failwith ("value error: no such variable: " ^ v)
          end
  | Func (v, e) -> Fun (v, st, e)
  | Bind (v, e1, e2) ->
          let r1 = eval_expr st e1 in
          eval_expr (fun i -> if i = v then Some r1 else st i) e2
  | App (e1, e2) ->
          let res1 = eval_expr st e1 in
          let res2 = eval_expr st e2 in
          begin match res1 with
          | Fun (vF, stF, eF) -> eval_expr (fun i -> if i = vF then Some res2 else stF i) eF
          | Rat _ -> failwith "type error: cannot apply to rat"
          end
  | Ite (c, t, e) ->
          begin match eval_expr st c with
          | Rat (0, _) -> eval_expr st e
          | _ -> eval_expr st t
          end


(** The following applies to [numerator] only:
    Bonus exercise, it is not relevant for solving the main exercise and will not be relevant for the exam:
    please see Artemis if you want to participate! Otherwise leave this at [None]. *)
let numerator : expr option = Some begin
Func ("a",
 Ite (Var "a",
  Ite (BinOp (Sub, Var "a", Const (1, 1)),
   Ite (BinOp (Sub, Var "a", Const (-1, 1)),
    Bind ("inner_loop",
     Func ("self",
      Func ("loop",
       Func ("i",
        Func ("j",
         Ite (BinOp (Sub, Var "j", Var "i"),
          Ite (BinOp (Sub, BinOp (Div, Var "i", Var "j"), Var "a"),
           Ite (BinOp (Sub, BinOp (Div, Var "j", Var "i"), Var "a"),
            Ite
             (BinOp (Sub, UnOp (Neg, BinOp (Div, Var "i", Var "j")), Var "a"),
             Ite
              (BinOp (Sub, UnOp (Neg, BinOp (Div, Var "j", Var "i")), Var "a"),
              App
               (App (App (App (Var "self", Var "self"), Var "loop"), Var "i"),
               BinOp (Add, Var "j", Const (1, 1))),
              UnOp (Neg, Var "j")),
             UnOp (Neg, Var "i")),
            Var "j"),
           Var "i"),
          App (App (App (Var "loop", Var "loop"), Var "self"),
           BinOp (Add, Var "i", Const (1, 1)))))))),
     Bind ("loop",
      Func ("self",
       Func ("inner_loop",
        Func ("i",
         App
          (App (App (App (Var "inner_loop", Var "inner_loop"), Var "self"),
            Var "i"),
          Const (1, 1))))),
      App (App (App (Var "loop", Var "loop"), Var "inner_loop"), Const (2, 1)))),
    Const (-1, 1)),
   Const (1, 1)),
  Const (0, 1)))
end


(*let exadsf = App (Func ("i", UnOp (Neg, Var "i")), Const (1,2))*)


(* the examples from the exercise description *)

(**{[ _a + _b ]}*)
let ex1 = BinOp (Add, Var "_a", Var "_b")

(**{[ let x = 2/7 in x ]}*)
let ex2 = Bind ("x", Const (2, 7), Var "x")

(**{[ if 3/4 then 1/2 else 2/1 ]}*)
let ex3 = Ite (Const (3, 4), Const (1, 2), Const (2, 1))

(**{[ if 1/4 * 0/1 then 1/2 else 2/1 ]}*)
let ex4 = Ite (BinOp (Mul, Const (1, 4), Const (0, 1)), Const (1, 2), Const (2, 1))

(**{[ fun z -> 1/3 - z ]}*)
let ex5 = Func ("z", BinOp (Sub, Const (1, 3), Var "z"))

(**{[
let f = fun x -> x * x in
f (1/2 - 1/4)
]}*)
let ex6 = Bind ("f", Func ("x", BinOp (Mul, Var "x", Var "x")), App (Var "f", BinOp (Sub, Const (1, 2), Const (1, 4))))

(**{[
let add = fun x -> fun y -> x + y in
let x = 2/3 in
add x 4/3
]}*)
let ex7 = Bind ("add", Func ("x", Func ("y", BinOp (Add, Var "x", Var "y"))), Bind ("x", Const (2,3), App (App (Var "add", Var "x"), Const (4, 3))))

(**{[ (let x = 2/1 in fun a -> let x = a + x in x * x) 10/2 ]}*)
let ex8 = App (Bind ("x", Const (2,1), Func ("a", Bind ("x", BinOp (Add, Var "a", Var "x"), BinOp (Mul, Var "x", Var "x")))), Const (10, 2))

(* ignore warnings about unused values *)
let _ = ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8

(* the rest of the file contains a variation of the pretty-printing function found on Artemis,
use it during your testing, if you'd like *)

(** function composition *)
let (<.) f g x = f (g x)

type assoc = AssocL | AssocR | NoAssoc

let string_of_var (x : var) = x
let string_of_binop = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
let string_of_unop = function Neg -> "-"

let is_unary_prefix Neg = true
let is_unary_associative Neg = true
let unary_precedence Neg = 6

let binary_precedence = function (Add | Sub) -> 6 | (Mul | Div) -> 7
let binary_assoc (Add | Sub | Mul | Div) = AssocL

let bracket_shows shows = List.cons "(" <. shows <. List.cons ")"

let shows_of_rat_lr : int -> int -> rat -> string list -> string list =
  fun _ _ (n, d) -> List.cons @@ Printf.sprintf "(%d/%d)" n d

let defend_all, attack_all = 11, 11
let defend_none, attack_none = 0, 0

let bracket_lr (lpower : int) (rpower : int) (ltough : int) (rtough : int) assoc =
  (* If a side requires brackets it is called "defeated".
    A side is defeated an soon as the attacking toughness is at least as large as the defending toughness,
    unless the operator associates appropriately. For example, in: (1 + 2) + 3 on the inner +
    we have rpower = rtough = 6, but no defeat because of left-associativity *)
  let is_defeat power tough assoc_ref =
    if power = tough && (power = attack_all || power = attack_none) then assoc = assoc_ref
    else if assoc = assoc_ref then power > tough
    else power >= tough
  in
  let ldefeat = is_defeat lpower ltough AssocR in
  let rdefeat = is_defeat rpower rtough AssocL in
  if ldefeat || rdefeat
    then true, bracket_shows
    else false, Fun.id

(**
string representation of expressions inspired by Haskell's showsPrec
  unlike in Haskell, the result is a list of strings to be concatenated
  (hopefully) in linear time by String.concat *)
let rec shows_of_expr_lr (lpower : int) (rpower : int) =
  (* uncomment the next line to always put brackets *)
  (* let lpower, rpower = attack_all, attack_all in *)
  let open List in
  let shows_fully_protected e = shows_of_expr_lr attack_none attack_none e in
  let when_unbracketed b power = if b then 0 else power in
  function
  | Const r             -> shows_of_rat_lr lpower rpower r
  | UnOp (op, e)        ->
    let is_pre = is_unary_prefix op in
    let is_assoc = is_unary_associative op in
    let prec = unary_precedence op in
    let ltough, rtough = if is_pre then (0, prec) else (prec, 0) in
    let assoc = if is_assoc then (if is_pre then AssocL else AssocR) else NoAssoc in
    let brackets_required, continue = bracket_lr lpower rpower ltough rtough assoc in
    continue
      (cons (string_of_unop op)
      <. shows_of_expr_lr
      (if is_pre then prec else when_unbracketed brackets_required lpower)
      (if not is_pre then prec else when_unbracketed brackets_required rpower)
      e)
  | BinOp (op, e1, e2)  ->
    let prec = binary_precedence op in
    let brackets_required, continue = bracket_lr lpower rpower prec prec (binary_assoc op) in
    continue
      (shows_of_expr_lr (when_unbracketed brackets_required lpower) prec e1
      <. cons (" " ^ string_of_binop op ^ " ")
      <. shows_of_expr_lr prec (when_unbracketed brackets_required rpower) e2)
  | Var x               -> cons (string_of_var x)
  | Func (x, e)         ->
    let brackets_required, continue = bracket_lr lpower rpower defend_all defend_none NoAssoc in
    continue
      (cons ("fun " ^ string_of_var x ^ " -> ")
      <. shows_of_expr_lr 0 (when_unbracketed brackets_required rpower) e)
  | App (f, x) ->
    let brackets_required, continue = bracket_lr lpower rpower 10 10 AssocL in
    continue
      (shows_of_expr_lr (when_unbracketed brackets_required lpower) 10 f
      <. cons " "
      <. shows_of_expr_lr 10 (when_unbracketed brackets_required rpower) x)
  | Bind (x, e, e_in)   ->
    let brackets_required, continue = bracket_lr lpower rpower defend_all defend_none NoAssoc in
    continue
      (cons ("let " ^ string_of_var x ^ " = ")
      <. shows_fully_protected e
      <. cons " in "
      <. shows_of_expr_lr attack_none (when_unbracketed brackets_required rpower) e_in)
  | Ite (c, t, f) ->
    let brackets_required, _ = bracket_lr lpower rpower defend_all defend_none NoAssoc in
    cons "if "
    <. shows_fully_protected c
    <. cons " then "
    <. shows_fully_protected t
    <. cons " else "
    <. shows_fully_protected f
    <. if brackets_required then cons " end" else Fun.id

let string_of_x_from_shows_of_x shows_f x = String.concat "" (shows_f attack_none attack_none x [])
let string_of_expr = string_of_x_from_shows_of_x shows_of_expr_lr
let string_of_rat = string_of_x_from_shows_of_x shows_of_rat_lr
let string_of_val = function
  | Fun (v, _, e) -> string_of_expr (Func (v, e)) ^ " <with state>"
  | Rat r -> string_of_rat r

(* ignore warnings about unused values *)
let _ = string_of_expr, string_of_rat, string_of_val

