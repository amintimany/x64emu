open Lexing

type result =
| Ok of (Parser.token * position) list
| LexingError of string * position

let format_position p = Printf.sprintf "%d : %d" p.pos_lnum (p.pos_cnum - p.pos_bol + 1)

let load_the_code (code : string) = 
  let lexbuf = Lexing.from_string code in
  try
    let ltks = Parser.token_list Lexer.token lexbuf in
    Ok ltks
  with
    Lexer.Error (msg, p) -> LexingError (msg, p)