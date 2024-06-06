%{
open Printf

let ite_reduce testa testb t e =
    match (testa, testb) with
    | Rat (0, _), _ -> Expr.Ite (testb, t, e)
    | _, Rat (0, _) -> Expr.Ite (testa, t, e)
    | _ -> Expr.Ite (Expr.BinOp (Expr.Sub, testa, testb), t, e)
%}

%token <int * int> CONST
%token <string> VAR
%token PLUS MINUS TIMES DIV
%token LET IN
%token IF THEN ELSE
%token FUN RARROW
%token EQ
%token NEQ
%token LPAREN RPAREN
%token BEGIN END
(*%token COMMENTSTART COMMENTEND*)
%token EOL

%nonassoc FUN RARROW    (* TODO: really nonassoc? *)
%nonassoc IF THEN ELSE  (* TODO: really nonassoc? *)
%nonassoc LET IN        (* TODO: really nonassoc? *)
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start input
%type <unit> input

%% /* Grammar rules and actions follow */
input:
    /* empty */		        { }
  | input finished_expr		{ }
;

finished_expr:
    EOL                     { }
  | expr EOL                { print_endline @@ Expr.pretty_string_of_expr $1; flush stdout }
;

expr:
    CONST                     { let (n,d) = $1 in Expr.Const (n,d) }
  | VAR                     { Expr.Var $1 }
  | LPAREN expr RPAREN      { $2 }
  | BEGIN expr END          { $2 }
  (*| COMMENTSTART string COMMENTEND
                            { $2 }*)
  | expr PLUS expr          { Expr.BinOp (Expr.Add, $1, $3) }
  | expr MINUS expr         { Expr.BinOp (Expr.Sub, $1, $3) }
  | expr TIMES expr         { Expr.BinOp (Expr.Mul, $1, $3) }
  | expr DIV expr           { Expr.BinOp (Expr.Div, $1, $3) }
  | MINUS expr %prec UMINUS { Expr.UnOp (Expr.Neg, $2) }
  | LET VAR EQ expr IN expr { Expr.Bind ($2, $4, $6) }
  (*| IF expr THEN expr ELSE expr
                            { Expr.Ite ($2, $4, $6) }*)
  | IF expr NEQ expr THEN expr ELSE expr
                            { ite_reduce $2 $4 $6 $8 }
  | IF expr EQ expr THEN expr ELSE expr
                            { ite_reduce $2 $4 $8 $6 }
  | FUN VAR RARROW expr     { Expr.Func ($2, $4) }
  | expr expr               { Expr.App ($1, $2) }
;

(*string:
    /* empty */		        { }
  |  string                  { }
;*)

(*line:     NEWLINE		{ }
  | exp NEWLINE		{ printf "\t%.10g\n" $1; flush stdout }
;

exp:      NUM			{ $1 }
  | exp exp PLUS		{ $1 +. $2 }
  | exp exp MINUS		{ $1 -. $2 }
  | exp exp MULTIPLY		{ $1 *. $2 }
  | exp exp DIVIDE		{ $1 /. $2 }
  /* Exponentiation */
  | exp exp CARET		{ $1 ** $2 }
  /* Unary minus    */
  | exp UMINUS		{ -. $1 }
;*)
%%
