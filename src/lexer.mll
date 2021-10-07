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
| digit+ as i      { if i = "9223372036854775808" then
                       INT64MIN
                     else
                       match Int64.of_string_opt i with
                       | Some j -> INT j
                       | None -> raise (Error ("Invalid number: \"" ^ i ^ "\".", lexbuf.lex_curr_p)) }
| '-'              { MINUS }
| ident as lbl     { LABEL lbl }
| '"'              { let buf = Buffer.create 100 in
                     let curp = lexbuf.lex_curr_p in
                     lex_string buf lexbuf;
                     lexbuf.lex_start_p <- curp;
                     STRING (Buffer.contents buf) }
| '.'              { DOT }
| '%'              { PERCENT }
| '$'              { DOLLAR }
| ':'              { COLON }
| ','              { COMMA }
| '*'              { ASTRISK }
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
| "cqto"           { CQTO }
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
| _ as c           { raise (Error ("Invalid character: '" ^ (String.make 1 c) ^ "'.", lexbuf.lex_curr_p)) }
and
comment = parse
| eof              { EOF }
| '\n'             { Lexing.new_line lexbuf; NEWLINE }
| _                { comment lexbuf }
and
lex_string buf = parse
| eof              { raise (Error ("File ended before string did.", lexbuf.lex_curr_p)) }
| '\\''\\' as s    { Buffer.add_string buf s; lex_string buf lexbuf }
| '\\''"' as s     { Buffer.add_string buf s; lex_string buf lexbuf }
| _ as c           { Buffer.add_char buf c; lex_string buf lexbuf }
| '\n'             { raise (Error ("Newline within string is not supported.", lexbuf.lex_curr_p)) }
| '"'              { () }

{
  let token_to_string tk =
    match tk with
    | EOF -> "EOF"
    | INT i -> "INT(" ^ (Int64.to_string i) ^ ")"
    | LABEL lbl -> "LABEL(" ^ lbl ^ ")"
    | STRING str -> "STRING(\"" ^ str ^ "\")"
    | DOT -> "DOT"
    | PERCENT -> "PERCENT"
    | DOLLAR -> "DOLLAR"
    | COLON -> "COLON"
    | COMMA -> "COMMA"
    | ASTRISK -> "ASTRISK"
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
    | CQTO -> "CQTO"
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