type token =
  | VAR of (int)
  | RAT of (Num.num)
  | LPAREN
  | RPAREN
  | EQU
  | DIS
  | LEQ
  | GEQ
  | LT
  | GT
  | PLUS
  | IMPLY
  | AND
  | OR
  | NOT
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "Code/Lexers_Parsers/parser_simplex.mly"
open General.Tseitin;;
open Num;;
# 25 "Code/Lexers_Parsers/parser_simplex.ml"
let yytransl_const = [|
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* EQU *);
  262 (* DIS *);
  263 (* LEQ *);
  264 (* GEQ *);
  265 (* LT *);
  266 (* GT *);
  267 (* PLUS *);
  268 (* IMPLY *);
  269 (* AND *);
  270 (* OR *);
  271 (* NOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* RAT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\003\000\003\000\003\000\004\000\005\000\
\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\003\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\019\000\000\000\
\003\000\004\000\005\000\000\000\000\000\000\000\007\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\009\000\000\000\016\000\015\000\
\011\000\012\000\013\000\014\000\017\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\011\000\012\000"

let yysindex = "\255\255\
\003\000\000\000\001\255\007\255\007\255\000\000\000\000\001\000\
\000\000\000\000\000\000\018\255\010\255\000\255\000\000\007\255\
\007\255\007\255\000\000\009\255\033\255\034\255\035\255\036\255\
\037\255\038\255\000\000\003\255\000\000\028\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\034\000\002\000\000\000\000\000\000\000\017\000"

let yytablesize = 279
let yytable = "\001\000\
\006\000\013\000\006\000\027\000\008\000\014\000\015\000\010\000\
\003\000\004\000\031\000\016\000\017\000\018\000\016\000\017\000\
\018\000\028\000\029\000\030\000\026\000\005\000\020\000\021\000\
\022\000\023\000\024\000\025\000\018\000\018\000\018\000\018\000\
\018\000\018\000\032\000\033\000\034\000\035\000\036\000\003\000\
\017\000\019\000\037\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\004\000\003\000\004\000\008\000\008\000\
\008\000\010\000\010\000\010\000\016\000\017\000\018\000\005\000\
\008\000\005\000\008\000\008\000\000\000\000\000\010\000"

let yycheck = "\001\000\
\000\000\001\001\000\000\004\001\000\000\004\000\005\000\000\000\
\002\001\003\001\002\001\012\001\013\001\014\001\012\001\013\001\
\014\001\016\000\017\000\018\000\011\001\015\001\005\001\006\001\
\007\001\008\001\009\001\010\001\005\001\006\001\007\001\008\001\
\009\001\010\001\002\001\002\001\002\001\002\001\002\001\002\001\
\013\001\008\000\026\000\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\002\001\003\001\002\001\003\001\
\004\001\002\001\003\001\004\001\012\001\013\001\014\001\015\001\
\012\001\015\001\014\001\015\001\255\255\255\255\015\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  EQU\000\
  DIS\000\
  LEQ\000\
  GEQ\000\
  LT\000\
  GT\000\
  PLUS\000\
  IMPLY\000\
  AND\000\
  OR\000\
  NOT\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  RAT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'form) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (((Num.num*int) list) *(Num.num*Num.num)*int) General.Tseitin.formula list) in
    Obj.repr(
# 24 "Code/Lexers_Parsers/parser_simplex.mly"
                 ( _1::_2 )
# 190 "Code/Lexers_Parsers/parser_simplex.ml"
               : (((Num.num*int) list) *(Num.num*Num.num)*int) General.Tseitin.formula list))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "Code/Lexers_Parsers/parser_simplex.mly"
         ( [] )
# 196 "Code/Lexers_Parsers/parser_simplex.ml"
               : (((Num.num*int) list) *(Num.num*Num.num)*int) General.Tseitin.formula list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom1) in
    Obj.repr(
# 30 "Code/Lexers_Parsers/parser_simplex.mly"
           ( Lit (_1, 0) )
# 203 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom2) in
    Obj.repr(
# 31 "Code/Lexers_Parsers/parser_simplex.mly"
           ( _1 )
# 210 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom3) in
    Obj.repr(
# 32 "Code/Lexers_Parsers/parser_simplex.mly"
                  ( _1 )
# 217 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'form))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'form) in
    Obj.repr(
# 33 "Code/Lexers_Parsers/parser_simplex.mly"
                      ( _2 )
# 224 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'form))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'form) in
    Obj.repr(
# 34 "Code/Lexers_Parsers/parser_simplex.mly"
             ( Not (_2,0) )
# 231 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'form) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'form) in
    Obj.repr(
# 35 "Code/Lexers_Parsers/parser_simplex.mly"
                 ( Or (_1, _3, 0) )
# 239 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'form) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'form) in
    Obj.repr(
# 36 "Code/Lexers_Parsers/parser_simplex.mly"
                  ( And (_1, _3, 0) )
# 247 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'form) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'form) in
    Obj.repr(
# 37 "Code/Lexers_Parsers/parser_simplex.mly"
                   ( Or (Not (_1,0), _3, 0) )
# 255 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'form))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Num.num) in
    Obj.repr(
# 40 "Code/Lexers_Parsers/parser_simplex.mly"
                ( (_1,(_3,Num.num_of_int 0),1) )
# 263 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'atom1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Num.num) in
    Obj.repr(
# 41 "Code/Lexers_Parsers/parser_simplex.mly"
                 ( (_1,(_3,Num.num_of_int 0),2) )
# 271 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'atom1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Num.num) in
    Obj.repr(
# 42 "Code/Lexers_Parsers/parser_simplex.mly"
               ( (_1,(_3,Num.num_of_string "-1"),3) )
# 279 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'atom1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Num.num) in
    Obj.repr(
# 43 "Code/Lexers_Parsers/parser_simplex.mly"
                ( (_1,(_3,Num.num_of_int 1),4) )
# 287 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'atom1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Num.num) in
    Obj.repr(
# 47 "Code/Lexers_Parsers/parser_simplex.mly"
                ( Or(Lit((_1,(_3,Num.num_of_string "-1"),3),0), Lit((_1,(_3,Num.num_of_int 1),4),0),0) )
# 295 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'atom2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sum) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Num.num) in
    Obj.repr(
# 50 "Code/Lexers_Parsers/parser_simplex.mly"
                 ( And(Lit((_1,(_3,Num.num_of_int 0),1),0),Lit((_1,(_3,Num.num_of_int 0),2),0),0) )
# 303 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'atom3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Num.num) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'sum) in
    Obj.repr(
# 53 "Code/Lexers_Parsers/parser_simplex.mly"
                   ( (_1,_2)::_4 )
# 312 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'sum))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Num.num) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 54 "Code/Lexers_Parsers/parser_simplex.mly"
           ( [(_1,_2)] )
# 320 "Code/Lexers_Parsers/parser_simplex.ml"
               : 'sum))
(* Entry formula *)
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
let formula (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (((Num.num*int) list) *(Num.num*Num.num)*int) General.Tseitin.formula list)
