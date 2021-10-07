%{

exception ParseError of string * Lexing.position

type section_type = TextSec | DataSec

type data_decl = QUAD of int64 | ASCIZ of string

type line =
| SectionDecl of section_type
| Globalize of X86.lbl
| DataDecl of data_decl
| LabelDecl of X86.lbl
| Instruction of X86.ins

let process_section lbl pos = 
    match lbl with
    | "text" -> SectionDecl TextSec
    | "data" -> SectionDecl DataSec
    | l -> raise (ParseError ("Unkown section declared: \"." ^ l ^ "\"", pos))

let process_declaration_command cmd lbl pos = 
    match cmd with
    | "global" -> Globalize lbl
    | l -> raise (ParseError ("Unkown command/data declaration: \"." ^ l ^ "\"", pos))

let process_int_decl cmd i pos =
    match cmd with
    | "quad" -> DataDecl (QUAD i)
    | l -> raise (ParseError ("Unkown integer data declaration: \"." ^ l ^ "\", did you mean \".quad\"?", pos))

let process_string_decl cmd s pos =
    match cmd with
    | "asciz" -> DataDecl (ASCIZ s)
    | l -> raise (ParseError ("Unkown string data declaration: \"." ^ l ^ "\", did you mean \".asciz\"?", pos))

let process_register l pos =
    match l with
    | "rip" -> X86.Rip
    | "rax" -> X86.Rax
    | "rbx" -> X86.Rbx 
    | "rcx" -> X86.Rcx
    | "rdx" -> X86.Rdx
    | "rsi" -> X86.Rsi
    | "rdi" -> X86.Rdi
    | "rbp" -> X86.Rbp
    | "rsp" -> X86.Rsp
    | "r08" -> X86.R08
    | "r09" -> X86.R09
    | "r10" -> X86.R10
    | "r11" -> X86.R11
    | "r12" -> X86.R12
    | "r13" -> X86.R13
    | "r14" -> X86.R14
    | "r15" -> X86.R15
    | l -> raise (ParseError ("Unkown register: \"%" ^ l ^ "\"", pos))

exception UNIMPLEMENTED

let process_lines (_ : line list) : X86.prog =
    raise UNIMPLEMENTED
%}


%token EOF
%token NEWLINE
%token <int64> INT
%token <string> LABEL
%token <string> STRING
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
%token CALLQ RETQ CQTO IDIVQ

%type <(token * Lexing.position) list> token_list
%type <X86.prog> prog

%start token_list
%start prog

%%

opcode:
| MOVQ                   { X86.Movq }
| PUSHQ                  { X86.Pushq }
| POPQ                   { X86.Popq }
| LEAQ                   { X86.Leaq }
| INCQ                   { X86.Incq }
| DECQ                   { X86.Decq }
| NEGQ                   { X86.Negq }
| NOTQ                   { X86.Notq }
| ADDQ                   { X86.Addq }
| SUBQ                   { X86.Subq }
| IMULQ                  { X86.Imulq }
| XORQ                   { X86.Xorq }
| ORQ                    { X86.Orq }
| ANDQ                   { X86.Andq }
| SHLQ                   { X86.Shlq }
| SARQ                   { X86.Sarq }
| SHRQ                   { X86.Shrq }
| JMP                    { X86.Jmp }
| JE                     { X86.J (X86.Eq) }
| JNE                    { X86.J (X86.Neq) }
| JG                     { X86.J (X86.Gt) }
| JGE                    { X86.J (X86.Ge) }
| JL                     { X86.J (X86.Lt) }
| JLE                    { X86.J (X86.Le) }
| CMPQ                   { X86.Cmpq }
| SETE                   { X86.Set (X86.Eq) }
| SETNE                  { X86.Set (X86.Neq) }
| SETG                   { X86.Set (X86.Gt) }
| SETGE                  { X86.Set (X86.Ge) }
| SETL                   { X86.Set (X86.Lt) }
| SETLE                  { X86.Set (X86.Le) }
| CALLQ                  { X86.Callq }
| RETQ                   { X86.Retq }
| CQTO                   { X86.Cqto }
| IDIVQ                  { X86.Idivq }

integer:
| i=INT                  { i }
| MINUS i=INT            { Int64.neg i }
| MINUS INT64MIN         { -9223372036854775808L }

immediate:
| i=integer              { X86.Lit i }
| l=LABEL                { X86.Lbl l }

register:
| PERCENT l=LABEL        { process_register l $startpos }

operand:
| DOLLAR imm=immediate   { X86.Imm imm }
| r=register             { X86.Reg r }
| imm=immediate          { X86.Ind1 imm }
| LPAREN r=register RPAREN
                         { X86.Ind2 r }
| imm=immediate LPAREN r=register RPAREN
                         { X86.Ind3 (imm, r) }


line:
| DOT l=LABEL           { process_section l $startpos }
| DOT c=LABEL l=LABEL   { process_declaration_command c l $startpos }
| DOT c=LABEL i=INT     { process_int_decl c i $startpos }
| DOT c=LABEL s=STRING  { process_string_decl c s $startpos }
| l=LABEL COLON         { LabelDecl l }
| opc=opcode ops=separated_list(COMMA, operand)
                        { Instruction (opc, ops) }

lines:
| NEWLINE* ln=line lns=after_line
                         { ln :: lns }

after_line:
| NEWLINE* EOF           { [] }
| NEWLINE lns=lines      { lns }

prog: 
| NEWLINE* EOF           { [] }
| lns = lines            { process_lines lns }


(* A simple parser that just parses a list of tokens; useful for debugging the lexer/parser. *)
any_token:
| NEWLINE       { (NEWLINE, $startpos) }
| i = INT       { (INT i, $startpos) }
| lbl = LABEL   { (LABEL lbl, $startpos) }
| s = STRING    { (STRING s, $startpos) }
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
| CQTO          { (CQTO, $startpos)}
| IDIVQ         { (IDIVQ, $startpos)}


token_list :
| p = any_token* EOF { p }