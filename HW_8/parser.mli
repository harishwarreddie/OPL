type token =
  | INTEGER of (
# 7 "parser.mly"
        int
# 6 "parser.mli"
)
  | VAR of (
# 8 "parser.mly"
        string
# 11 "parser.mli"
)
  | PLUS
  | DASH
  | LPAREN
  | RPAREN
  | COMMA
  | COLON
  | DOT
  | EQUALS
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | FST
  | SND
  | LAMBDA
  | LET
  | IN
  | EOF
  | AND
  | INT
  | BOOL
  | ARROW
  | STAR

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
