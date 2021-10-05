open Lexing

let load_the_code (code : string) = 
  let lexbuf = Lexing.from_string code in
  let ltks = Parser.token_list Lexer.token lexbuf in
  List.fold_left (fun str (tk, p) -> 
    str ^ Printf.sprintf "(%s, %d : %d)\n" (Lexer.token_to_string tk) p.pos_lnum (p.pos_cnum - p.pos_bol + 1))
    "" ltks