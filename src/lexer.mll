{
  open Parser

  exception Error of string * Lexing.position

}

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']

let integer = digit+
let ident = ('_'|letter)('_'|letter|digit)*

rule token = parse
| eof              { EOF }
| '\n'             { Lexing.new_line lexbuf; NEWLINE }
| [' ' '\t']       { token lexbuf }
| digit+ as i      { match Int64.of_string_opt i with Some j -> INT j | None -> raise (Error ("invalid number: \"" ^ i ^ "\".", lexbuf.lex_curr_p)) }
| "9223372036854775808"
                   { INT64MIN }
| '-'              { MINUS }
| ident as lbl     { LABEL lbl }
| '.'              { DOT }
| '%'              { PERCENT }
| '$'              { DOLLAR }
| ':'              { COLON }
| ','              { COMMA }
| '('              { LPAREN }
| ')'              { RPAREN }
| "shrq"           { SHRQ }
| "shlq"           { SHLQ }
| "sarq"           { SARQ }
| "sete"           { SETE }
| "setne"          { SETNE }
| "setl"           { SETL }
| "setle"          { SETLE }
| "setg"           { SETG }
| "setge"          { SETGE }
| "callq"          { CALLQ }
| "retq"           { RETQ }
| "pushq"          { PUSHQ }
| "popq"           { POPQ }
| "addq"           { ADDQ }
| "subq"           { SUBQ }
| "imulq"          { IMULQ }
| "ctoq"           { CTOQ }
| "idivq"          { IDIVQ }
| "orq"            { ORQ }
| "xorq"           { XORQ }
| "andq"           { ANDQ }
| "notq"           { NOTQ }
| "movq"           { MOVQ }
| "cmpq"           { CMPQ }
| "jne"            { JNE }
| "je"             { JE }
| "jg"             { JG }
| "jge"            { JGE }
| "jl"             { JL }
| "jle"            { JLE }
| "leaq"           { LEAQ }
| "incq"           { INCQ }
| "decq"           { DECQ }
| ';'|'#'          { comment lexbuf }
| _ as c           { raise (Error ("invalid character: '" ^ (String.make 1 c) ^ "'.", lexbuf.lex_curr_p)) }
and
comment = parse
| eof              { EOF }
| '\n'             { Lexing.new_line lexbuf; NEWLINE }
| _                { comment lexbuf }

{
  let token_to_string tk =
    match tk with
    | EOF -> "EOF"
    | INT i -> "INT(" ^ (Int64.to_string i) ^ ")"
    | LABEL lbl -> "LABEL(" ^ lbl ^ ")"
    | DOT -> "DOT"
    | PERCENT -> "PERCENT"
    | DOLLAR -> "DOLLAR"
    | COLON -> "COLON"
    | COMMA -> "COMMA"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | INT64MIN -> "INT64MIN"
    | MINUS -> "MINUS"
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