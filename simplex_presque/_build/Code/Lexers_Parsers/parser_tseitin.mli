type token =
  | VAR of (int)
  | LPAREN
  | RPAREN
  | IMPLY
  | AND
  | OR
  | NOT
  | EOF

val formula :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int General.Tseitin.formlist
