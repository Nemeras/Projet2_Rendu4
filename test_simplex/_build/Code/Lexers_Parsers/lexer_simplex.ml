# 1 "Code/Lexers_Parsers/lexer_simplex.mll"
 
open Parser_simplex;;

# 6 "Code/Lexers_Parsers/lexer_simplex.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\241\255\002\000\003\000\004\000\246\255\018\000\029\000\
    \045\000\025\000\011\000\011\000\252\255\253\255\254\255\255\255\
    \251\255\250\255\249\255\056\000\079\000\089\000\245\255\244\255\
    \242\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\000\
    \255\255\012\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\007\000\008\000\255\255\255\255\
    \255\255";
  Lexing.lex_default = 
   "\255\255\000\000\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\000\000\000\000\
    \000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\015\000\015\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \015\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \013\000\012\000\000\000\005\000\000\000\008\000\000\000\010\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\016\000\000\000\004\000\009\000\003\000\024\000\
    \023\000\022\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\019\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\018\000\
    \000\000\000\000\000\000\000\000\011\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\017\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\000\000\000\000\000\000\000\000\000\000\014\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\000\000\000\000\000\000\000\000\000\000\
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
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\255\255\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\011\000\255\255\000\000\000\000\000\000\002\000\
    \003\000\004\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\009\000\
    \255\255\255\255\255\255\255\255\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\010\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\000\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\255\255\255\255\255\255\255\255\255\255\
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
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 11 "Code/Lexers_Parsers/lexer_simplex.mll"
                   ( token lexbuf )
# 134 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 1 ->
# 12 "Code/Lexers_Parsers/lexer_simplex.mll"
         ( NOT )
# 139 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 2 ->
# 13 "Code/Lexers_Parsers/lexer_simplex.mll"
         ( LPAREN )
# 144 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 3 ->
# 14 "Code/Lexers_Parsers/lexer_simplex.mll"
         ( RPAREN )
# 149 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 4 ->
# 15 "Code/Lexers_Parsers/lexer_simplex.mll"
           ( OR )
# 154 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 5 ->
# 16 "Code/Lexers_Parsers/lexer_simplex.mll"
           ( AND )
# 159 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 6 ->
# 17 "Code/Lexers_Parsers/lexer_simplex.mll"
          ( IMPLY )
# 164 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 7 ->
let
# 18 "Code/Lexers_Parsers/lexer_simplex.mll"
           s
# 170 "Code/Lexers_Parsers/lexer_simplex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 18 "Code/Lexers_Parsers/lexer_simplex.mll"
               ( RAT (Num.num_of_string s) )
# 174 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 8 ->
let
# 19 "Code/Lexers_Parsers/lexer_simplex.mll"
               s
# 180 "Code/Lexers_Parsers/lexer_simplex.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) lexbuf.Lexing.lex_curr_pos in
# 19 "Code/Lexers_Parsers/lexer_simplex.mll"
                  ( VAR (int_of_string s) )
# 184 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 9 ->
# 20 "Code/Lexers_Parsers/lexer_simplex.mll"
         ( PLUS )
# 189 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 10 ->
# 21 "Code/Lexers_Parsers/lexer_simplex.mll"
           ( LEQ )
# 194 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 11 ->
# 22 "Code/Lexers_Parsers/lexer_simplex.mll"
          ( GEQ )
# 199 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 12 ->
# 23 "Code/Lexers_Parsers/lexer_simplex.mll"
         ( EQU )
# 204 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 13 ->
# 24 "Code/Lexers_Parsers/lexer_simplex.mll"
          ( DIS )
# 209 "Code/Lexers_Parsers/lexer_simplex.ml"

  | 14 ->
# 25 "Code/Lexers_Parsers/lexer_simplex.mll"
         ( EOF )
# 214 "Code/Lexers_Parsers/lexer_simplex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;
