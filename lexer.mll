(* Lexical analyzer returns one of the tokens:
   the token NUM of a floating point number,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET, UMINUS),
   or NEWLINE.  It skips all blanks and tabs, unknown characters
   and raises End_of_file on EOF. *)
{
  open Parser (* Assumes the parser file is "rtcalc.mly". *)
}
let digit = ['0'-'9']
let var_char = ['a'-'z' '_']

rule token = parse
  | [' ' '\t']	    { token lexbuf }
  | '\n'	        { EOL }
  | '(' ('-'? digit+ as num) '/' (digit+ as denom) ')'
                    { CONST (int_of_string num, int_of_string denom) }
  | '-'? digit+ as num
                    { CONST (int_of_string num, 1) }
  | var_char+ as var
                    {
                        match var with
                        | "let" -> LET
                        | "in" -> IN
                        | "if" -> IF
                        | "then" -> THEN
                        | "else" -> ELSE
                        | "fun" -> FUN
                        | "begin" -> BEGIN
                        | "end" -> END
                        | _ -> VAR var
                    }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIV }
  (*| "let"           { LET }
  | "in"            { IN }*)
  | '='             { EQ }
  | "<>"
  | "!="            { NEQ }
  | "->"            { RARROW }
  (*| "(*"            { COMMENTSTART }
  | "*)"            { COMMENTEND }*)
  (*| "." digit+
  | digit+ "." digit* as num
		{ NUM (float_of_string num) }*)
  (*| '+'		{ PLUS }
  | '-'		{ MINUS }
  | '*'		{ MULTIPLY }
  | '/'		{ DIVIDE }
  | '^'		{ CARET }
  | 'n'		{ UMINUS }*)
  | _ as c	{ raise (Failure ("unknown symbol: " ^ Char.escaped c)) }
  | eof		{ raise End_of_file }
