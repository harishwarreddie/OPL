type token =
  | INTEGER of (
# 7 "parser.mly"
        int
# 6 "parser.ml"
)
  | VAR of (
# 8 "parser.mly"
        string
# 11 "parser.ml"
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

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
  open Ast
  open Printf
  open Lexing
# 44 "parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* DASH *);
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* COMMA *);
  264 (* COLON *);
  265 (* DOT *);
  266 (* EQUALS *);
  267 (* TRUE *);
  268 (* FALSE *);
  269 (* IF *);
  270 (* THEN *);
  271 (* ELSE *);
  272 (* FST *);
  273 (* SND *);
  274 (* LAMBDA *);
  275 (* LET *);
  276 (* IN *);
    0 (* EOF *);
  277 (* AND *);
  278 (* INT *);
  279 (* BOOL *);
  280 (* ARROW *);
  281 (* STAR *);
    0|]

let yytransl_block = [|
  257 (* INTEGER *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\005\000\005\000\006\000\
\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\006\000\001\000\004\000\001\000\006\000\001\000\003\000\003\000\
\003\000\003\000\001\000\002\000\002\000\002\000\001\000\001\000\
\001\000\001\000\001\000\005\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\017\000\016\000\000\000\018\000\019\000\000\000\
\000\000\000\000\000\000\000\000\022\000\002\000\004\000\000\000\
\000\000\015\000\000\000\000\000\013\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\021\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\000\000\000\020\000\000\000\000\000\000\000\001\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000\016\000\017\000\018\000"

let yysindex = "\010\000\
\036\255\000\000\000\000\000\000\036\255\000\000\000\000\036\255\
\093\255\093\255\000\255\003\255\000\000\000\000\000\000\014\255\
\081\255\000\000\006\255\001\255\000\000\000\000\010\255\011\255\
\073\255\073\255\073\255\073\255\000\000\000\000\036\255\073\255\
\055\255\036\255\093\255\093\255\093\255\093\255\017\255\005\255\
\000\000\004\255\000\000\073\255\036\255\014\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\031\000\046\000\061\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\063\000\000\000"

let yygindex = "\000\000\
\251\255\248\255\000\000\234\255\017\000\253\255"

let yytablesize = 342
let yytable = "\019\000\
\011\000\023\000\020\000\006\000\024\000\021\000\022\000\025\000\
\026\000\040\000\001\000\030\000\031\000\029\000\032\000\007\000\
\025\000\026\000\033\000\044\000\034\000\046\000\043\000\045\000\
\041\000\039\000\000\000\000\000\042\000\027\000\009\000\029\000\
\029\000\029\000\029\000\000\000\003\000\004\000\027\000\047\000\
\005\000\035\000\036\000\037\000\038\000\008\000\006\000\007\000\
\008\000\000\000\000\000\009\000\010\000\011\000\012\000\003\000\
\004\000\000\000\000\000\005\000\010\000\000\000\005\000\000\000\
\000\000\006\000\007\000\008\000\000\000\000\000\009\000\010\000\
\011\000\003\000\004\000\000\000\000\000\005\000\000\000\000\000\
\000\000\003\000\004\000\006\000\007\000\005\000\000\000\000\000\
\009\000\010\000\028\000\006\000\007\000\003\000\004\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\006\000\
\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\011\000\011\000\000\000\011\000\011\000\
\000\000\006\000\006\000\000\000\000\000\000\000\011\000\011\000\
\000\000\006\000\007\000\007\000\011\000\007\000\007\000\006\000\
\000\000\011\000\000\000\000\000\000\000\007\000\007\000\000\000\
\000\000\009\000\009\000\007\000\009\000\009\000\000\000\000\000\
\007\000\000\000\000\000\000\000\009\000\009\000\000\000\000\000\
\008\000\008\000\009\000\008\000\008\000\000\000\000\000\009\000\
\000\000\000\000\000\000\008\000\008\000\000\000\000\000\010\000\
\010\000\008\000\010\000\010\000\005\000\005\000\008\000\000\000\
\000\000\000\000\010\000\010\000\005\000\000\000\000\000\000\000\
\010\000\000\000\005\000\000\000\000\000\010\000"

let yycheck = "\005\000\
\000\000\002\001\008\000\000\000\002\001\009\000\010\000\003\001\
\004\001\032\000\001\000\006\001\007\001\017\000\014\001\000\000\
\003\001\004\001\009\001\015\001\010\001\044\000\006\001\020\001\
\033\000\031\000\255\255\255\255\034\000\025\001\000\000\035\000\
\036\000\037\000\038\000\255\255\001\001\002\001\025\001\045\000\
\005\001\025\000\026\000\027\000\028\000\000\000\011\001\012\001\
\013\001\255\255\255\255\016\001\017\001\018\001\019\001\001\001\
\002\001\255\255\255\255\005\001\000\000\255\255\000\000\255\255\
\255\255\011\001\012\001\013\001\255\255\255\255\016\001\017\001\
\018\001\001\001\002\001\255\255\255\255\005\001\255\255\255\255\
\255\255\001\001\002\001\011\001\012\001\005\001\255\255\255\255\
\016\001\017\001\010\001\011\001\012\001\001\001\002\001\255\255\
\255\255\005\001\255\255\255\255\255\255\255\255\255\255\011\001\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\255\255\006\001\007\001\
\255\255\006\001\007\001\255\255\255\255\255\255\014\001\015\001\
\255\255\014\001\003\001\004\001\020\001\006\001\007\001\020\001\
\255\255\025\001\255\255\255\255\255\255\014\001\015\001\255\255\
\255\255\003\001\004\001\020\001\006\001\007\001\255\255\255\255\
\025\001\255\255\255\255\255\255\014\001\015\001\255\255\255\255\
\003\001\004\001\020\001\006\001\007\001\255\255\255\255\025\001\
\255\255\255\255\255\255\014\001\015\001\255\255\255\255\003\001\
\004\001\020\001\006\001\007\001\006\001\007\001\025\001\255\255\
\255\255\255\255\014\001\015\001\014\001\255\255\255\255\255\255\
\020\001\255\255\020\001\255\255\255\255\025\001"

let yynames_const = "\
  PLUS\000\
  DASH\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  COLON\000\
  DOT\000\
  EQUALS\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FST\000\
  SND\000\
  LAMBDA\000\
  LET\000\
  IN\000\
  EOF\000\
  AND\000\
  INT\000\
  BOOL\000\
  ARROW\000\
  STAR\000\
  "

let yynames_block = "\
  INTEGER\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 19 "parser.mly"
                                        ( Let(_2,_4,_6) )
# 249 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lexp) in
    Obj.repr(
# 20 "parser.mly"
                                        ( _1 )
# 256 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'lexp) in
    Obj.repr(
# 22 "parser.mly"
                            ( Lam (_2,_4) )
# 264 "parser.ml"
               : 'lexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cexp) in
    Obj.repr(
# 23 "parser.mly"
                            ( _1 )
# 271 "parser.ml"
               : 'lexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'oexp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'oexp) in
    Obj.repr(
# 25 "parser.mly"
                                   ( If (_2,_4,_6) )
# 280 "parser.ml"
               : 'cexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'oexp) in
    Obj.repr(
# 26 "parser.mly"
                                   ( _1 )
# 287 "parser.ml"
               : 'cexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'oexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 28 "parser.mly"
                                   ( Plus(_1,_3) )
# 295 "parser.ml"
               : 'oexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'oexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 29 "parser.mly"
                                   ( Times(_1,_3) )
# 303 "parser.ml"
               : 'oexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'oexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 30 "parser.mly"
                                   ( Minus(_1,_3) )
# 311 "parser.ml"
               : 'oexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'appexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 31 "parser.mly"
                                   ( Eq (_1,_3) )
# 319 "parser.ml"
               : 'oexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 32 "parser.mly"
                                   ( _1 )
# 326 "parser.ml"
               : 'oexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 34 "parser.mly"
                                   ( App(_1,_2) )
# 334 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 35 "parser.mly"
                                   ( Fst _2 )
# 341 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 36 "parser.mly"
                                   ( Snd _2 )
# 348 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aexp) in
    Obj.repr(
# 37 "parser.mly"
                                   ( _1 )
# 355 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
                                   ( Var _1 )
# 362 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 40 "parser.mly"
                                   ( Int _1 )
# 369 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                                   ( True )
# 375 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                                   ( False )
# 381 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 43 "parser.mly"
                                   ( Pair(_2,_4) )
# 389 "parser.ml"
               : 'aexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 44 "parser.mly"
                                   ( _2 )
# 396 "parser.ml"
               : 'aexp))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.exp)
