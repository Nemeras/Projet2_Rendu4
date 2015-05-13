type token =
  | INIT of (int*int)
  | LIT of (int)
  | EOC
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "Code/Lexers_Parsers/parser_cnf.mly"
open General.Cnf
# 12 "Code/Lexers_Parsers/parser_cnf.ml"
let yytransl_const = [|
  259 (* EOC *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INIT *);
  258 (* LIT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\000\000\000\000\004\000\002\000\003\000"

let yydgoto = "\002\000\
\004\000\007\000"

let yysindex = "\001\000\
\002\255\000\000\000\000\254\254\254\254\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\002\000"

let yytablesize = 7
let yytable = "\005\000\
\006\000\001\000\003\000\005\000\000\000\000\000\008\000"

let yycheck = "\002\001\
\003\001\001\000\001\001\000\000\255\255\255\255\005\000"

let yynames_const = "\
  EOC\000\
  EOF\000\
  "

let yynames_block = "\
  INIT\000\
  LIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int*int) in
    Obj.repr(
# 17 "Code/Lexers_Parsers/parser_cnf.mly"
         ( { clauses = [] ; v = fst _1 ; v_real = 0 ; c = snd _1 ; c_real = 0 } )
# 68 "Code/Lexers_Parsers/parser_cnf.ml"
               : General.Cnf.cnf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : General.Cnf.cnf) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 18 "Code/Lexers_Parsers/parser_cnf.mly"
              ( _1.clauses <- (fst _2)::_1.clauses ;
			  _1.c_real <- _1.c_real + 1 ; _1.v_real <- max _1.v_real (snd _2) ;
			  _1 )
# 78 "Code/Lexers_Parsers/parser_cnf.ml"
               : General.Cnf.cnf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 24 "Code/Lexers_Parsers/parser_cnf.mly"
              ( _1::(fst _2), max (abs _1) (snd _2) )
# 86 "Code/Lexers_Parsers/parser_cnf.ml"
               : 'clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "Code/Lexers_Parsers/parser_cnf.mly"
        ( [], 0 )
# 92 "Code/Lexers_Parsers/parser_cnf.ml"
               : 'clause))
(* Entry cnf *)
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
let cnf (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : General.Cnf.cnf)
