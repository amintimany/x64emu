open Lexing

type 'a result =
| LexingError of string * position
| ParsingError of string * position option
| Ok of 'a

let load_the_code (code : string) = 
  let lexbuf = Lexing.from_string code in
  try
    let prog = Parser.prog Lexer.token lexbuf in
    Ok (X86.string_of_prog prog)
  with
  | Lexer.Error (msg, p) -> LexingError (msg, p)
  | Parser.Error -> ParsingError ("Parse Error", None)
  | Common.ParseError (msg, p) -> ParsingError (msg, Some p)