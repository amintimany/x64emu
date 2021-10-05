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
| "movq"           { MOVQ }

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