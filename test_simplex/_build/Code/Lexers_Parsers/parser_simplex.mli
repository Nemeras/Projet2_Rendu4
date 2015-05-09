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

val formula :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (((Num.num*int) list) *(Num.num*Num.num)*int) General.Tseitin.formula list
