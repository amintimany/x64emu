{
  open Parser

  exception Error of string * Lexing.position

}

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']

let integer = digit+
let ident = ('_'|letter)('_'|letter|digit)*

rule token = parse
  eof              { EOF }
(*| [' ''\t']        { token lexbuf }
| "rec"            { REC }
| "ask"            { ASK }
| "tell"           { TELL }
| integer as i     { INT (int_of_string i) }
| ident as id      { ID id }
| '\n'             { Lexing.new_line lexbuf; NEWLINE }
| '+'              { PLUS }
| '-'              { MINUS }
| '*'              { TIMES }
| '%'              { REMAINDER }
| ":="             { ASSIGN }
| '('              { LPAREN }
| ')'              { RPAREN }
| '{'              { LBRACE }
| '}'              { RBRACE }
| '.'              { DOT }
| "//"             { single_line_comment lexbuf }
| "/*"             { multi_line_comment 0 lexbuf }
| '/'              { DIVIDE }
| _ as c           { raise (Error ("Unexpected character " ^ (String.make 1 c), lexbuf.lex_curr_p)) }

and single_line_comment = parse
eof                { EOF }
| '\n'             { Lexing.new_line lexbuf; NEWLINE }
| _                { single_line_comment lexbuf }

and multi_line_comment level = parse
  eof              { raise (Error ("File ended before the comment did.\n", lexbuf.lex_curr_p)) }
| "*/"             { if level = 0 then token lexbuf else multi_line_comment (level - 1) lexbuf }
| "/*"             { multi_line_comment (level + 1) lexbuf }
| '\n'             { Lexing.new_line lexbuf; multi_line_comment level lexbuf }
| _                { multi_line_comment level lexbuf }
*)

{
  let token_to_string tk =
    match tk with
    | EOF -> "EOF"
    | INT i -> "INT(" ^ (Int64.to_string i) ^ ")"
    | LABEL lbl -> "LABEL(" ^ lbl ^ ")"
    | SHRQ -> "SHRQ"
    | SHLQ -> "SHLQ"
    | SARQ -> "SARQ"
    | SETE -> "SETE"
    | SETNE -> "SETNE"
    | SETL -> "SETL"
    | SETLE -> "SETLE"
    | SETG -> "SETG"
    | SETGE -> "SETGE"
    | CALLQ -> "CALLQ"
    | RETQ -> "RETQ"
    | PUSHQ -> "PUSHQ"
    | POPQ -> "POPQ"
    | ADDQ -> "ADDQ"
    | SUBQ -> "SUBQ"
    | IMULQ -> "IMULQ"
    | CTOQ -> "CTOQ"
    | IDIVQ -> "IDIVQ"
    | ORQ -> "ORQ"
    | XORQ -> "XORQ"
    | ANDQ -> "ANDQ"
    | NOTQ -> "NOTQ"
    | NEGQ -> "NEGQQ"
    | MOVQ -> "MOVQ"
    | CMPQ -> "CMPQ"
    | JNE -> "JNE"
    | JE -> "JE"
    | JLE -> "JLE"
    | JL -> "JL"
    | JG -> "JG"
    | JGE -> "JGE"
    | JMP -> "JMP"
    | LEAQ -> "LEAQ"
    | NEWLINE -> "NEWLINE"
    | INCQ -> "INCQ"
    | DECQ -> "DECQ"
}