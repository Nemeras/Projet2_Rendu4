type token =
  | VAR of (int)
  | LPAREN
  | RPAREN
  | EQU
  | DIS
  | IMPLY
  | AND
  | OR
  | NOT
  | EOF

val formula :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (int*int) General.Tseitin.formula list
