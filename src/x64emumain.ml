open Lexing

type 'a result =
| LexingError of string * position
| ParsingError of string * position option
| LoadingError of string * position
| Ok of 'a

let load_the_code (code : string) = 
  let lexbuf = Lexing.from_string code in
  try
    let prog = Parser.prog Lexer.token lexbuf in
    Ok (Machine.create_machine 20 10 prog)
  with
  | Lexer.Error (msg, p) -> LexingError (msg, p)
  | Parser.Error -> ParsingError ("Parse Error", None)
  | Common.ParseError (msg, p) -> ParsingError (msg, Some p)
  | Machine.ErrorInitializingMachine (msg, p) -> LoadingError (msg, p)