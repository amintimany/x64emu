open Lexing

module Binops = Binops
module Machine = Machine
module X86 = X86

type 'a result =
| LexingError of string * position
| ParsingError of string * position option
| LoadingError of string * position option
| Ok of 'a

let load_the_code (code : string) (entry_point : string) = 
  let lexbuf = Lexing.from_string code in
  try
    let prog = Parser.prog Lexer.token lexbuf in
    Ok (Machine.create_machine 20 10 prog entry_point)
  with
  | Lexer.Error (msg, p) -> LexingError (msg, p)
  | Parser.Error -> ParsingError ("Parse Error.", None)
  | Common.ParseError (msg, p) -> ParsingError (msg, Some p)
  | Machine.ErrorInitializingMachine (msg, p) -> LoadingError (msg, p)