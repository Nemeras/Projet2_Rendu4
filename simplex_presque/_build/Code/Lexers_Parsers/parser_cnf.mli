type token =
  | INIT of (int*int)
  | LIT of (int)
  | EOC
  | EOF

val cnf :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> General.Cnf.cnf
