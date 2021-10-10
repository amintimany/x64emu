%{

open Common

type section_type = TextSec | DataSec

type line =
| SectionDecl of section_type * Lexing.position
| Globalize of X86.lbl * Lexing.position
| DataDecl of X86.data * Lexing.position
| Instruction of X86.ins * Lexing.position
| LabelDecl of X86.lbl * Lexing.position

let process_section lbl pos = 
    match lbl with
    | "text" -> SectionDecl (TextSec, pos)
    | "data" -> SectionDecl (DataSec, pos)
    | l -> raise (ParseError ("Unkown section declared: \"." ^ l ^ "\"", pos))

let process_declaration_command cmd lbl pos = 
    match cmd with
    | "global" | "globl" -> Globalize (lbl, pos)
    | l -> raise (ParseError ("Unkown command/data declaration: \"." ^ l ^ "\"", pos))

let process_int_decl cmd i pos =
    match cmd with
    | "quad" -> DataDecl (X86.Quad (X86.Lit i, pos), pos)
    | l -> raise (ParseError ("Unkown integer data declaration: \"." ^ l ^ "\", did you mean \".quad\"?", pos))

let process_string_decl cmd s pos =
    match cmd with
    | "asciz" -> DataDecl (X86.Asciz (s, pos), pos)
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
    | "r8" -> X86.R08
    | "r9" -> X86.R09
    | "r10" -> X86.R10
    | "r11" -> X86.R11
    | "r12" -> X86.R12
    | "r13" -> X86.R13
    | "r14" -> X86.R14
    | "r15" -> X86.R15
    | l -> raise (ParseError ("Unkown register: \"%" ^ l ^ "\"", pos))

let process_lines (lns : line list) : X86.prog =
  let res = ref [] in
  let current_section = ref None in
  let all_labels = ref [] in
  let global_labels = ref [] in
  let current_block_label = ref None in
  let current_block_instrs = ref [] in
  let current_block_data_decls = ref [] in
  let can_make_block () =
    match !current_block_label with 
    | None -> false
    | Some _ ->
        match !current_section with
        | None -> false
        | Some DataSec -> true
        | Some TextSec -> true
  in
  let make_block () =
    let label = 
      match !current_block_label with 
      | None -> raise InternalParsingError
      | Some l -> l
    in
    match !current_section with
    | None -> raise InternalParsingError
    | Some DataSec -> X86.Asm.data label !current_block_data_decls
    | Some TextSec -> 
        match List.find_opt (fun l -> l = label) !global_labels with
        | None -> X86.Asm.text label !current_block_instrs
        | Some _ -> X86.Asm.gtext label !current_block_instrs
  in
  let add_insrt_to_block instr pos =
    match !current_section with
    | None -> raise (ParseError ("Instruction encounterd outside any section!", pos))
    | Some DataSec -> raise (ParseError ("Instruction in data section!", pos))
    | Some TextSec ->
        match !current_block_label with
        | None -> raise (ParseError ("Instruction added before any label!", pos))
        | Some _ -> current_block_instrs := !current_block_instrs @ [instr]
  in
  let add_data_decl_to_block dd pos =
    match !current_section with
    | None -> raise (ParseError ("Data declaration encounterd outside any section!", pos))
    | Some TextSec -> raise (ParseError ("Data declaration in text section!", pos))
    | Some DataSec ->
        match !current_block_label with
        | None -> raise (ParseError ("Data added before any label was declared!", pos))
        | Some _ -> current_block_data_decls := !current_block_data_decls @ [dd]
  in
  let enter_section sec =
    if can_make_block () then res := !res @ [make_block ()];
    current_section := Some sec;
    current_block_label := None;
    current_block_instrs := [];
    current_block_data_decls := []
  in
  let add_label l pos =
    (match List.find_opt (fun s -> s = l) !all_labels with
    | Some _ -> raise (ParseError ("Label \""^ l ^"\" is already declared!", pos))
    | None -> ()); 
    all_labels := l :: !all_labels;
    match !current_section with
    | None -> raise (ParseError ("Label declared outside any section!", pos))
    | Some sec -> if can_make_block () then enter_section sec; current_block_label := Some l
  in
  let rec proc_lines = function
    | [] -> if can_make_block () then res := !res @ [make_block ()]; !res
    | ln :: lns -> 
      (match ln with
      | SectionDecl (sec, _) -> enter_section sec
      | Globalize (l, _) -> global_labels := l :: !global_labels
      | DataDecl (dd, pos) -> add_data_decl_to_block dd pos
      | Instruction (instr, pos) -> add_insrt_to_block instr pos
      | LabelDecl (l, pos) -> add_label l pos
      );proc_lines lns
  in
  proc_lines lns
%}


%token EOF
%token NEWLINE
%token <int64> INT
%token <string> LABEL
%token <string> STRING
%token DOT PERCENT DOLLAR COLON COMMA ASTRISK
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

non_jump_opcode:
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
| CMPQ                   { X86.Cmpq }
| SETE                   { X86.Set (X86.Eq) }
| SETNE                  { X86.Set (X86.Neq) }
| SETG                   { X86.Set (X86.Gt) }
| SETGE                  { X86.Set (X86.Ge) }
| SETL                   { X86.Set (X86.Lt) }
| SETLE                  { X86.Set (X86.Le) }
| RETQ                   { X86.Retq }
| CQTO                   { X86.Cqto }
| IDIVQ                  { X86.Idivq }

jump_opcode:
| CALLQ                  { X86.Callq }
| JMP                    { X86.Jmp }
| JE                     { X86.J (X86.Eq) }
| JNE                    { X86.J (X86.Neq) }
| JG                     { X86.J (X86.Gt) }
| JGE                    { X86.J (X86.Ge) }
| JL                     { X86.J (X86.Lt) }
| JLE                    { X86.J (X86.Le) }


integer:
| i=INT                  { i }
| MINUS i=INT            { Int64.neg i }
| MINUS INT64MIN         { -9223372036854775808L }

immediate:
| i=integer              { X86.Lit i }
| l=LABEL                { X86.Lbl l }

register:
| PERCENT l=LABEL        { process_register l $startpos }

non_jump_operand:
| DOLLAR imm=immediate   { X86.Imm imm }
| r=register             { X86.Reg r }
| imm=immediate          { X86.Ind1 imm }
| LPAREN r=register RPAREN
                         { X86.Ind2 r }
| imm=immediate LPAREN r=register RPAREN
                         { X86.Ind3 (imm, r) }

jump_operand:
| imm=immediate          { X86.Imm imm }
| ASTRISK r=register     { X86.Reg r }
| ASTRISK imm=immediate  { X86.Ind1 imm }
| ASTRISK LPAREN r=register RPAREN
                         { X86.Ind2 r }
| ASTRISK imm=immediate LPAREN r=register RPAREN
                         { X86.Ind3 (imm, r) }


line:
| DOT l=LABEL           { process_section l $startpos }
| DOT c=LABEL l=LABEL   { process_declaration_command c l $startpos }
| DOT c=LABEL i=INT     { process_int_decl c i $startpos }
| DOT c=LABEL s=STRING  { process_string_decl c s $startpos }
| l=LABEL COLON         { LabelDecl (l, $startpos) }
| opc=jump_opcode ops=separated_list(COMMA, jump_operand)
                        { Instruction ((opc, ops, $startpos), $startpos) }
| opc=non_jump_opcode ops=separated_list(COMMA, non_jump_operand)
                        { Instruction ((opc, ops, $startpos), $startpos) }

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
| ASTRISK       { (ASTRISK, $startpos)}
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