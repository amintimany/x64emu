%{

%}


%token EOF
%token NEWLINE
%token <int64> INT
%token <string> LABEL
%token MOVQ PUSHQ POPQ
%token LEAQ
%token INCQ DECQ NEGQ NOTQ
%token ADDQ SUBQ IMULQ XORQ ORQ ANDQ
%token SHLQ SARQ SHRQ
%token JMP JE JNE JG JGE JL JLE
%token CMPQ SETE SETNE SETG SETGE SETL SETLE
%token CALLQ RETQ CTOQ IDIVQ

%type <(token * Lexing.position) list> token_list

%start token_list

%%

// entries just to test the lexer.

any_token:
| NEWLINE       { (NEWLINE, $startpos) }
| i = INT       { (INT i, $startpos) }
| lbl = LABEL   { (LABEL lbl, $startpos) }
| MOVQ          { (MOVQ, $startpos)}
| PUSHQ         { (PUSHQ, $startpos)}
| POPQ          { (POPQ, $startpos)}
| LEAQ          { (LEAQ, $startpos)}
| INCQ          { (INCQ, $startpos)}
| DECQ          { (DECQ, $startpos)}
| NEGQ          { (NEGQ, $startpos)}
| NOTQ          { (NOTQ, $startpos)}
| ADDQ          { (ADDQ, $startpos)}
| SUBQ          { (SUBQ, $startpos)}
| IMULQ         { (IMULQ, $startpos)}
| XORQ          { (XORQ, $startpos)}
| ORQ           { (ORQ, $startpos)}
| ANDQ          { (ANDQ, $startpos)}
| SHLQ          { (SHLQ, $startpos)}
| SARQ          { (SARQ, $startpos)}
| SHRQ          { (SHRQ, $startpos)}
| JMP           { (JMP, $startpos)}
| JE            { (JE, $startpos)}
| JNE           { (JNE, $startpos)}
| JG            { (JG, $startpos)}
| JGE           { (JGE, $startpos)}
| JL            { (JL, $startpos)}
| JLE           { (JLE, $startpos)}
| CMPQ          { (CMPQ, $startpos)}
| SETE          { (SETE, $startpos)}
| SETNE         { (SETNE, $startpos)}
| SETG          { (SETG, $startpos)}
| SETGE         { (SETGE, $startpos)}
| SETL          { (SETL, $startpos)}
| SETLE         { (SETLE, $startpos)}
| CALLQ         { (CALLQ, $startpos)}
| RETQ          { (RETQ, $startpos)}
| CTOQ          { (CTOQ, $startpos)}
| IDIVQ         { (IDIVQ, $startpos)}


token_list :
| p = any_token* EOF { p }