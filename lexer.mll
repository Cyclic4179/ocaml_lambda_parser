{
  open Parser (* Assumes the parser file is "parser.mly". *)
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
  | _ as c	{ raise (Failure ("unknown symbol: " ^ Char.escaped c)) }
  | eof		{ raise End_of_file }
