{
  open Parser

  exception Error of string * Lexing.position

}

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']

let integer = digit+
let ident = ('_'|letter)('_'|letter|digit|'@'|'$')*

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
| '*'              { ASTERISK }
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
| "negq"           { NEGQ }
| "imulq"          { IMULQ }
| "cqto"           { CQTO }
| "idivq"          { IDIVQ }
| "orq"            { ORQ }
| "xorq"           { XORQ }
| "andq"           { ANDQ }
| "notq"           { NOTQ }
| "movq"           { MOVQ }
| "cmpq"           { CMPQ }
| "jmp"            { JMP }
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
| ident as lbl     { LABEL lbl }
| _ as c           { raise (Error ("Invalid character: '" ^ (String.make 1 c) ^ "'.", lexbuf.lex_curr_p)) }
and
comment = parse
| eof              { EOF }
| '\n'             { Lexing.new_line lexbuf; NEWLINE }
| _                { comment lexbuf }
and
lex_string buf = parse
| eof              { raise (Error ("File ended before string did.", lexbuf.lex_curr_p)) }
| '\n'             { raise (Error ("Newline within string is not supported.", lexbuf.lex_curr_p)) }
| '\\'(digit as i1)(digit as i2)(digit as i3)
                   { match int_of_string_opt (String.make 1 i1), int_of_string_opt (String.make 1 i2), int_of_string_opt (String.make 1 i3) with 
                     | Some n1, Some n2, Some n3 -> 
                        let n = n3 + (n2 * 8) + (n1 * 64) in
                        if n < 256 && 0 <= n then
                          let c = Char.chr n in
                          Buffer.add_char buf c; lex_string buf lexbuf
                        else
                          raise (Error (Printf.sprintf "Invalid escape sequence \"%c%c%c\"." i1 i2 i3, lexbuf.lex_curr_p))
                     | _ -> raise (Error (Printf.sprintf "Invalid escape sequence \"%c%c%c\"." i1 i2 i3, lexbuf.lex_curr_p)) }
| '"'              { () }
| _ as c           { Buffer.add_char buf c; lex_string buf lexbuf }

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
    | ASTERISK -> "ASTERISK"
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