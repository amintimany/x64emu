%{

%}


%token EOF
%token NEWLINE
%token <int64> INT
%token <string> LABEL
%token DOT PERCENT DOLLAR COLON COMMA
%token INT64MIN (* This is produced when we encounter the number 9223372036854775808 which is does not fit in a 64 bit integer but its negation does (it is the least value that does). *)
%token MINUS (* Used for negative numbers. *)
%token LPAREN RPAREN
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
| DOT           { (DOT, $startpos) }
| PERCENT       { (PERCENT, $startpos) }
| DOLLAR        { (DOLLAR, $startpos) }
| COLON         { (COLON, $startpos)}
| COMMA         { (COMMA, $startpos)}
| LPAREN        { (LPAREN, $startpos) }
| RPAREN        { (RPAREN, $startpos) }
| INT64MIN      { (INT64MIN, $startpos) }
| MINUS         { (MINUS, $startpos) }
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