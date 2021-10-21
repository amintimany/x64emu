open Binops

module Memory = Bigarray.Array1
module LabelMap = Map.Make(String)

type memory = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Memory.t

type machine_mode = Normal | ExceptionHasOccured

exception ErrorInitializingMachine of string * Lexing.position option

exception InternalError

exception Error of string * Lexing.position option

(* Uused to indicate what has been updated during a step of computation. *)
type machine_component =
| MCReg of X86.reg
| MCFlags
| MCMem of int (* address in memory *)

type machine = {
(* The rip register. This is a special register in our implementation. It cannot be directly read or written to.
   It can only be changed by (non-indirect) jumps to labels in the program. *)
rip : int ref;
(* The rest of the registers *)
rax : int64 ref;
rbx : int64 ref;
rcx : int64 ref;
rdx : int64 ref;
rsi : int64 ref;
rdi : int64 ref;
rbp : int64 ref;
rsp : int64 ref;
r8 : int64 ref;
r9 : int64 ref;
r10 : int64 ref;
r11 : int64 ref;
r12 : int64 ref;
r13 : int64 ref;
r14 : int64 ref;
r15 : int64 ref;
(* flags *)
flags : flags;
(* heap boundary *)
heap_boundary : int;
(* stack boundary  *)
stack_boundary : int;
(* mode *)
mode : machine_mode ref;
(* memory *)
memory : memory;
(* program *)
prog : X86.ins array;
(* labels *)
prog_labels : int LabelMap.t;
data_labels : int LabelMap.t;
}

let last_stack_address machine = Memory.dim machine.memory

let bytes_of_memory (n : int) (addr : int) =
    let rec bom i acc =
        if i = n then acc else bom (i + 1) (MCMem (addr + i) :: acc)
    in
    bom 0 []

let load_64bits_from_memory memory addr opos =
    if addr < 0 || Memory.dim memory <= addr + 7 then raise (Error ("Invalid memory access. Attempted to access memory at address " ^ (int64_to_hex_string (Int64.of_int addr)) ^ ".", opos));
    let bits = Array.init 64 (fun _ -> false) in
    bit_array_set_byte bits (Memory.get memory addr) 0;
    bit_array_set_byte bits (Memory.get memory (addr + 1)) 8;
    bit_array_set_byte bits (Memory.get memory (addr + 2)) 16;
    bit_array_set_byte bits (Memory.get memory (addr + 3)) 24;
    bit_array_set_byte bits (Memory.get memory (addr + 4)) 32;
    bit_array_set_byte bits (Memory.get memory (addr + 5)) 40;
    bit_array_set_byte bits (Memory.get memory (addr + 6)) 48;
    bit_array_set_byte bits (Memory.get memory (addr + 7)) 56;
    bits

let store_64bits_to_memory memory addr bits opos =
    if addr < 0 || Memory.dim memory <= addr + 7 then raise (Error ("Invalid memory access. Attempted to write to memory at address " ^ (int64_to_hex_string (Int64.of_int addr)) ^ ".", opos));
    if Array.length bits < 64 then raise InternalError;
    Memory.set memory addr (bit_array_get_byte bits 0);
    Memory.set memory (addr + 1) (bit_array_get_byte bits 8);
    Memory.set memory (addr + 2) (bit_array_get_byte bits 16);
    Memory.set memory (addr + 3) (bit_array_get_byte bits 24);
    Memory.set memory (addr + 4) (bit_array_get_byte bits 32);
    Memory.set memory (addr + 5) (bit_array_get_byte bits 40);
    Memory.set memory (addr + 6) (bit_array_get_byte bits 48);
    Memory.set memory (addr + 7) (bit_array_get_byte bits 56)
    
type resolved_operand = Lit of int64 | Reg of (X86.reg * int64 ref) | Addr of int

let resolve_register machine reg pos =
    match reg with
    | X86.Rip -> raise (Error ("The %rip register cannot be accessed directly.", Some pos))
    | X86.Rax -> machine.rax
    | X86.Rbx -> machine.rbx
    | X86.Rcx -> machine.rcx
    | X86.Rdx -> machine.rdx
    | X86.Rsi -> machine.rsi
    | X86.Rdi -> machine.rdi
    | X86.Rbp -> machine.rbp
    | X86.Rsp -> machine.rsp
    | X86.R08 -> machine.r8
    | X86.R09 -> machine.r9
    | X86.R10 -> machine.r10
    | X86.R11 -> machine.r11
    | X86.R12 -> machine.r12
    | X86.R13 -> machine.r13
    | X86.R14 -> machine.r14
    | X86.R15 -> machine.r15

let resolve_immediate machine imm pos =
    match imm with
    | X86.Lit x -> Lit x
    | X86.Lbl l -> 
        match LabelMap.find_opt l machine.data_labels with
        | Some i -> Addr i
        | None -> raise (Error ("Label \"" ^ l ^ "\" is not a declared data label.", Some pos))

let resolve_operand machine operand pos =
    match operand with
    | X86.Imm imm -> resolve_immediate machine imm pos
    | X86.Reg r -> Reg (r, resolve_register machine r pos)
    | X86.Ind1 _ -> raise (Error ("Direct memory address with a literal address is not supported.", Some pos)) 
    | X86.Ind2 r -> Addr (Int64.to_int !(resolve_register machine r pos))
    | X86.Ind3 (imm, r) ->
        (match resolve_immediate machine imm pos with
        | Lit n ->
            begin
                match r with
                | X86.Rip -> raise (Error ("rip-relative adderssing is not supported with a literal number as offset.", Some pos)) 
                | _ -> Addr (Int64.to_int (Int64.add !(resolve_register machine r pos) n))
            end
        | Addr addr ->
            begin
                match r with
                | X86.Rip -> Addr addr
                | _ -> 
                    let offset = bits_to_int64 (load_64bits_from_memory machine.memory addr (Some pos)) in
                    Addr (Int64.to_int (Int64.add !(resolve_register machine r pos) offset))
            end;
        | _ -> raise InternalError)

let execute_movq machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Movq operation expects exacly 2 operands.", Some pos))
    in
    match (left_arg, right_arg) with
    | Lit n, Reg (r, rr) -> rr := n; [MCReg r]
    | Lit n, Addr a -> store_64bits_to_memory machine.memory a (int64_to_bits n) (Some pos); bytes_of_memory 8 a
    | _, Lit _ -> raise (Error ("The destination of a Movq operation cannot be a literal number.", Some pos))
    | Reg (_, sr), Reg (d, dr) -> dr := !sr; [MCReg d]
    | Reg (_, rr), Addr a -> store_64bits_to_memory machine.memory a (int64_to_bits !rr) (Some pos); bytes_of_memory 8 a
    | Addr a, Reg (r, rr) -> rr := bits_to_int64 (load_64bits_from_memory machine.memory a (Some pos)); [MCReg r]
    | Addr _, Addr _ -> raise (Error ("Movq operation does not support moving from memory to memory.", Some pos))

let perform_push machine bits pos =
    machine.rsp := Int64.sub !(machine.rsp) 8L;
    let rsp = Int64.to_int !(machine.rsp) in
    if rsp < machine.stack_boundary then raise (Error ("Stack overflow occured.", Some pos));
    store_64bits_to_memory machine.memory rsp bits (Some pos);
    MCReg X86.Rsp :: bytes_of_memory 8 rsp
    
let execute_pushq machine args pos =
    let arg =
        match args with
        | [a] -> (resolve_operand machine a pos)
        | _ -> raise (Error ("Pushq operation expects exacly 1 operand.", Some pos))
    in
    match arg with
    | Lit n -> perform_push machine (int64_to_bits n) pos
    | Reg (_, rr) -> perform_push machine (int64_to_bits !rr) pos
    | Addr a -> 
        let bits_to_push = load_64bits_from_memory machine.memory a (Some pos) in
        perform_push machine bits_to_push pos

let perform_pop machine pos =
    if (Int64.to_int !(machine.rsp)) > (last_stack_address machine) - 8 then raise (Error ("Program attempted top pop while the stack does not have enough data.", Some pos));
    let rsp = Int64.to_int !(machine.rsp) in
    let bits = load_64bits_from_memory machine.memory rsp (Some pos) in
    machine.rsp := Int64.add !(machine.rsp) 8L; bits

let execute_popq machine args pos =
    let arg =
        match args with
        | [a] -> (resolve_operand machine a pos)
        | _ -> raise (Error ("Popq operation expects exacly 1 operand.", Some pos))
    in
    match arg with
    | Lit _ -> raise (Error ("The destination of the Popq operation cannot be a literal number.", Some pos))
    | Reg (r, rr) -> rr := bits_to_int64 (perform_pop machine pos); [MCReg X86.Rsp; MCReg r]
    | Addr a -> store_64bits_to_memory machine.memory a (perform_pop machine pos) (Some pos);  [MCReg X86.Rsp] @ bytes_of_memory 8 a

let execute_leaq machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Leaq operation expects exacly 2 operands.", Some pos))
    in
    match (left_arg, right_arg) with
    | Addr a, Reg (r, rr) -> rr := Int64.of_int a; [MCReg r]
    | _, _ -> raise (Error ("The arguemtns to Leaq must consist of a memory address (source) and a register (destination).", Some pos))

let execute_incq machine args pos =
    let arg =
        match args with
        | [a] -> (resolve_operand machine a pos)
        | _ -> raise (Error ("Incq operation expects exacly 1 operand.", Some pos))
    in
    let bits_for_one = int64_to_bits 1L in
    match arg with
    | Lit _ -> raise (Error ("The destination of the Incq operation cannot be a literal number.", Some pos))
    | Reg (r, rr) -> 
        let bits = int64_to_bits !rr in
        Binops.bits_add bits bits_for_one bits machine.flags;
        rr := bits_to_int64 bits; [MCFlags; MCReg r]
    | Addr a -> 
        let bits = load_64bits_from_memory machine.memory a (Some pos) in
        Binops.bits_add bits bits_for_one bits machine.flags;
        store_64bits_to_memory machine.memory a bits (Some pos);
        MCFlags :: bytes_of_memory 8 a
    
let execute_decq machine args pos =
    let arg =
        match args with
        | [a] -> (resolve_operand machine a pos)
        | _ -> raise (Error ("Incq operation expects exacly 1 operand.", Some pos))
    in
    let bits_for_one = int64_to_bits 1L in
    let bits_for_minus_one = int64_to_bits 1L in
    Binops.bits_not bits_for_minus_one machine.flags;
    Binops.bits_add bits_for_minus_one bits_for_one bits_for_minus_one machine.flags;
    match arg with
    | Lit _ -> raise (Error ("The destination of the Incq operation cannot be a literal number.", Some pos))
    | Reg (r, rr) -> 
        let bits = int64_to_bits !rr in
        Binops.bits_add bits bits_for_minus_one bits machine.flags;
        rr := bits_to_int64 bits; [MCFlags; MCReg r]
    | Addr a -> 
        let bits = load_64bits_from_memory machine.memory a (Some pos) in
        Binops.bits_add bits bits_for_minus_one bits machine.flags;
        store_64bits_to_memory machine.memory a bits (Some pos);
        MCFlags :: bytes_of_memory 8 a

let execute_negq machine args pos =
    let arg =
        match args with
        | [a] -> (resolve_operand machine a pos)
        | _ -> raise (Error ("Negq operation expects exacly 1 operand.", Some pos))
    in
    let bits_for_one = int64_to_bits 1L in
    match arg with
    | Lit _ -> raise (Error ("The destination of the negq operation cannot be a literal number.", Some pos))
    | Reg (r, rr) -> 
        let bits = int64_to_bits !rr in Binops.bits_not bits machine.flags;
        Binops.bits_add bits bits_for_one bits machine.flags;
        rr := bits_to_int64 bits; [MCFlags; MCReg r]
    | Addr a -> 
        let bits = load_64bits_from_memory machine.memory a (Some pos) in Binops.bits_not bits machine.flags;
        Binops.bits_add bits bits_for_one bits machine.flags;
        store_64bits_to_memory machine.memory a bits (Some pos);
        MCFlags :: bytes_of_memory 8 a
    
let execute_notq machine args pos =
    let arg =
        match args with
        | [a] -> resolve_operand machine a pos
        | _ -> raise (Error ("Notq operation expects exacly 1 operand.", Some pos))
    in
    match arg with
    | Lit _ -> raise (Error ("The destination of the notq operation cannot be a literal number.", Some pos))
    | Reg (r, rr) ->
        let bits = int64_to_bits !rr in
        Binops.bits_not bits machine.flags;
        rr := bits_to_int64 bits; [MCFlags; MCReg r]
    | Addr a -> 
        let bits = load_64bits_from_memory machine.memory a (Some pos) in
        Binops.bits_not bits machine.flags;
        store_64bits_to_memory machine.memory a bits (Some pos);
        MCFlags :: bytes_of_memory 8 a

let execute_addq machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Addq operation expects exacly 2 operands.", Some pos))
    in
    match (left_arg, right_arg) with
    | Lit n, Reg (r, rr) ->
        let bits_src = int64_to_bits n in
        let bits_dest = int64_to_bits !rr in
        bits_add bits_src bits_dest bits_dest machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | Lit n, Addr a -> 
        let bits_src = int64_to_bits n in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        bits_add bits_src bits_dest bits_dest machine.flags;
        store_64bits_to_memory machine.memory a bits_dest (Some pos);
        MCFlags :: bytes_of_memory 8 a
    | _, Lit _ -> raise (Error ("The destination of addq operation cannot be a literal number.", Some pos))
    | Reg (_, sr), Reg (d, dr) ->
        let bits_src = int64_to_bits !sr in
        let bits_dest = int64_to_bits !dr in
        bits_add bits_src bits_dest bits_dest machine.flags;
        dr := bits_to_int64 bits_dest; [MCFlags; MCReg d]
    | Reg (_, rr), Addr a ->
        let bits_src = int64_to_bits !rr in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        bits_add bits_src bits_dest bits_dest machine.flags;
        store_64bits_to_memory machine.memory a bits_dest (Some pos);
        MCFlags :: bytes_of_memory 8 a
    | Addr a, Reg (r, rr) ->
        let bits_src = (load_64bits_from_memory machine.memory a (Some pos)) in
        let bits_dest = int64_to_bits !rr in
        bits_add bits_src bits_dest bits_dest machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | Addr _, Addr _ -> raise (Error ("Addq operation does not support two memory operands.", Some pos))
    
let execute_subq machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Subq operation expects exacly 2 operands.", Some pos))
    in
    let bits_for_one = int64_to_bits 1L in
    match (left_arg, right_arg) with
    | Lit n, Reg (r, rr) ->
        let bits_src = int64_to_bits n in
        let bits_dest = int64_to_bits !rr in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | Lit n, Addr a -> 
        let bits_src = int64_to_bits n in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags;
        store_64bits_to_memory machine.memory a bits_dest (Some pos);
        MCFlags :: bytes_of_memory 8 a
    | _, Lit _ -> raise (Error ("The destination of subq operation cannot be a literal number.", Some pos))
    | Reg (_, sr), Reg (d, dr) ->
        let bits_src = int64_to_bits !sr in
        let bits_dest = int64_to_bits !dr in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags;
        dr := bits_to_int64 bits_dest; [MCFlags; MCReg d]
    | Reg (_, rr), Addr a ->
        let bits_src = int64_to_bits !rr in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags;
        store_64bits_to_memory machine.memory a bits_dest (Some pos);
        MCFlags :: bytes_of_memory 8 a
    | Addr a, Reg (r, rr) ->
        let bits_src = (load_64bits_from_memory machine.memory a (Some pos)) in
        let bits_dest = int64_to_bits !rr in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | Addr _, Addr _ -> raise (Error ("Subq operation does not support two memory operands.", Some pos))

let execute_bin_log f machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Binary logical operations expect exacly 2 operands.", Some pos))
    in
    match (left_arg, right_arg) with
    | Lit n, Reg (r, rr) ->
        let bits_src = int64_to_bits n in
        let bits_dest = int64_to_bits !rr in
        bits_bin_log_op bits_src bits_dest bits_dest f machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | Lit n, Addr a -> 
        let bits_src = int64_to_bits n in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        bits_bin_log_op bits_src bits_dest bits_dest f machine.flags;
        store_64bits_to_memory machine.memory a bits_dest (Some pos);
        MCFlags :: bytes_of_memory 8 a
    | _, Lit _ -> raise (Error ("The destination of a binary logical operation cannot be a literal number.", Some pos))
    | Reg (_, sr), Reg (d, dr) ->
        let bits_src = int64_to_bits !sr in
        let bits_dest = int64_to_bits !dr in
        bits_bin_log_op bits_src bits_dest bits_dest f machine.flags;
        dr := bits_to_int64 bits_dest; [MCFlags; MCReg d]
    | Reg (_, rr), Addr a ->
        let bits_src = int64_to_bits !rr in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        bits_bin_log_op bits_src bits_dest bits_dest f machine.flags;
        store_64bits_to_memory machine.memory a bits_dest (Some pos);
        MCFlags :: bytes_of_memory 8 a
    | Addr a, Reg (r, rr) ->
        let bits_src = (load_64bits_from_memory machine.memory a (Some pos)) in
        let bits_dest = int64_to_bits !rr in
        bits_bin_log_op bits_src bits_dest bits_dest f machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | Addr _, Addr _ -> raise (Error ("Binary logical operations do not support two memory operands.", Some pos))

let execute_xorq machine args pos =
    execute_bin_log (fun b1 b2 -> b1 <> b2) machine args pos

let execute_orq machine args pos =
    execute_bin_log (fun b1 b2 -> b1 || b2) machine args pos
    
let execute_andq machine args pos =
    execute_bin_log (fun b1 b2 -> b1 && b2) machine args pos

let execute_shlq machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Shlq operation expects exacly 2 operands.", Some pos))
    in
    match (left_arg, right_arg) with
    | Lit n, Reg (r, rr) ->
        let bits_dest = int64_to_bits !rr in
        shift_left (Int64.to_int n) bits_dest machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | _, _ -> raise (Error ("Shlq operation only supports shifting registers by literal numbers.", Some pos))

let execute_sarq machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Sarq operation expects exacly 2 operands.", Some pos))
    in
    match (left_arg, right_arg) with
    | Lit n, Reg (r, rr) ->
        let bits_dest = int64_to_bits !rr in
        shift_right_arithmetic (Int64.to_int n) bits_dest machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | _, _ -> raise (Error ("Sarq operation only supports shifting registers by literal numbers.", Some pos))

let execute_shrq machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Sarq operation expects exacly 2 operands.", Some pos))
    in
    match (left_arg, right_arg) with
    | Lit n, Reg (r, rr) ->
        let bits_dest = int64_to_bits !rr in
        shift_right (Int64.to_int n) bits_dest machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | _, _ -> raise (Error ("Sarq operation only supports shifting registers by literal numbers.", Some pos))

let execute_cmpq machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Cmpq operation expects exacly 2 operands.", Some pos))
    in
    let bits_for_one = int64_to_bits 1L in
    match (left_arg, right_arg) with
    | Lit n, Reg (_, rr) ->
        let bits_src = int64_to_bits n in
        let bits_dest = int64_to_bits !rr in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags; [MCFlags]
    | Lit n, Addr a -> 
        let bits_src = int64_to_bits n in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags;
        [MCFlags]
    | _, Lit _ -> raise (Error ("The destination of cmpq operation cannot be a literal number.", Some pos))
    | Reg (_, sr), Reg (_, dr) ->
        let bits_src = int64_to_bits !sr in
        let bits_dest = int64_to_bits !dr in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags;
        [MCFlags]
    | Reg (_, rr), Addr a ->
        let bits_src = int64_to_bits !rr in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags;
        [MCFlags]
    | Addr a, Reg (_, rr) ->
        let bits_src = (load_64bits_from_memory machine.memory a (Some pos)) in
        let bits_dest = int64_to_bits !rr in
        bits_not bits_src machine.flags;
        bits_add bits_src bits_for_one bits_src machine.flags;
        bits_add bits_src bits_dest bits_dest machine.flags;
        [MCFlags]
    | Addr _, Addr _ -> raise (Error ("Cmpq operation does not support two memory operands.", Some pos))

let check_cond machine cnd =
    match cnd with
    | X86.Eq -> !(machine.flags.flag_ZF)
    | X86.Neq -> not !(machine.flags.flag_ZF)
    | X86.Gt -> (not !(machine.flags.flag_ZF)) && (machine.flags.flag_SF = machine.flags.flag_OF)
    | X86.Ge -> machine.flags.flag_SF = machine.flags.flag_OF
    | X86.Lt -> machine.flags.flag_SF <> machine.flags.flag_OF
    | X86.Le -> !(machine.flags.flag_ZF) || (machine.flags.flag_SF <> machine.flags.flag_OF)

let execute_jump ocnd machine args pos =
    let arg = 
        match args with
        | [a] -> a
        | _ -> raise (Error ("Jump operations expect exacly 1 operands.", Some pos))
    in
    match arg with
    | X86.Imm (X86.Lbl l) ->
        begin
            match LabelMap.find_opt l machine.prog_labels with
            | Some addr -> 
                begin
                    match ocnd with
                    | None -> machine.rip := addr
                    | Some cnd -> if check_cond machine cnd then machine.rip := addr else machine.rip := !(machine.rip) + 1
                end
            | None -> raise (Error ("Program attepted to jump to unknown label \"" ^ l ^ "\".", Some pos))
        end; []
    | _ -> raise (Error ("We only support jumping to labels in the text section.", Some pos))
    
let execute_set cnd machine args pos =
    let arg = 
        match args with
        | [a] -> resolve_operand machine a pos
        | _ -> raise (Error ("Set operations expect exacly 1 operand.", Some pos))
    in
    match arg with
    | Lit _ -> raise (Error ("The destination of a set operation cannot be a literal number.", Some pos))
    | Reg _ -> raise (Error ("The destination of a set operation cannot be a 64 bit register; the result is a single byte. This operation just sets the byte to 1 if the condition holds and otherwise to 0.", Some pos))
    | Addr a -> if check_cond machine cnd then Memory.set machine.memory a 1 else Memory.set machine.memory a 0; [MCMem a]

let execute_callq machine args pos =
    let arg = 
        match args with
        | [a] -> a
        | _ -> raise (Error ("Callq operation expects exacly 1 operand.", Some pos))
    in
    match arg with
    | X86.Imm (X86.Lbl l) ->
        begin
            match LabelMap.find_opt l machine.prog_labels with
            | Some addr -> 
                begin
                    let res = perform_push machine (int64_to_bits (Int64.of_int (!(machine.rip) + 1))) pos in
                    machine.rip := addr; res
                end
            | None -> raise (Error ("Program attepted to call unknown function \"" ^ l ^ "\".", Some pos))
        end
    | _ -> raise (Error ("We only support calling labels in the text section.", Some pos))

let execute_retq machine args pos =
    (match args with
    | [] -> ()
    | _ -> raise (Error ("Callq operation expects exacly 0 operands.", Some pos)));
    machine.rip := Int64.to_int (bits_to_int64 (perform_pop machine pos)); [MCReg X86.Rsp]

let execute_imulq machine args pos =
    let left_arg, right_arg =
        match args with
        | [l; r] -> (resolve_operand machine l pos, resolve_operand machine r pos)
        | _ -> raise (Error ("Imulq operation expects exacly 2 operands.", Some pos))
    in
    match (left_arg, right_arg) with
    | Lit n, Reg (r, rr) ->
        let bits_src = int64_to_bits n in
        let bits_dest = int64_to_bits !rr in
        signed_mul bits_src bits_dest bits_dest machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | Lit n, Addr a -> 
        let bits_src = int64_to_bits n in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        signed_mul bits_src bits_dest bits_dest machine.flags;
        store_64bits_to_memory machine.memory a bits_dest (Some pos);
        MCFlags :: bytes_of_memory 8 a
    | _, Lit _ -> raise (Error ("The destination of imulq operation cannot be a literal number.", Some pos))
    | Reg (_, sr), Reg (d, dr) ->
        let bits_src = int64_to_bits !sr in
        let bits_dest = int64_to_bits !dr in
        signed_mul bits_src bits_dest bits_dest machine.flags;
        dr := bits_to_int64 bits_dest; [MCFlags; MCReg d]
    | Reg (_, rr), Addr a ->
        let bits_src = int64_to_bits !rr in
        let bits_dest = (load_64bits_from_memory machine.memory a (Some pos)) in
        signed_mul bits_src bits_dest bits_dest machine.flags;
        store_64bits_to_memory machine.memory a bits_dest (Some pos);
        MCFlags :: bytes_of_memory 8 a
    | Addr a, Reg (r, rr) ->
        let bits_src = (load_64bits_from_memory machine.memory a (Some pos)) in
        let bits_dest = int64_to_bits !rr in
        signed_mul bits_src bits_dest bits_dest machine.flags;
        rr := bits_to_int64 bits_dest; [MCFlags; MCReg r]
    | Addr _, Addr _ -> raise (Error ("Imulq operation does not support two memory operands.", Some pos))

let execute_cqto machine args pos =
    (match args with
    | [] -> ()
    | _ -> raise (Error ("Cqto operation expects exacly 0 operands.", Some pos)));
    let bits = int64_to_bits !(machine.rax) in
    let msb = bits.(Array.length bits - 1) in
    let bits_rdx = Array.map (fun _ -> msb) bits in
    machine.rdx := bits_to_int64 bits_rdx;
    [MCReg X86.Rdx]

let execute_idivq machine args pos =
    let arg =
        match args with
        | [a] -> resolve_operand machine a pos
        | _ -> raise (Error ("Notq operation expects exacly 1 operand.", Some pos))
    in
    let divisor =
        match arg with
        | Lit _ -> raise (Error ("The source of the idivq operation cannot be a literal number.", Some pos))
        | Reg (_, rr) -> Big_int.big_int_of_int64 !rr
        | Addr a -> Big_int.big_int_of_int64 (bits_to_int64 (load_64bits_from_memory machine.memory a (Some pos)))
    in
    let dividend = Big_int.add_big_int (Big_int.shift_left_big_int (Big_int.big_int_of_int64 !(machine.rdx)) 64) (Big_int.big_int_of_int64 !(machine.rax)) in
    try
        let (bq, br) = Big_int.quomod_big_int dividend divisor in
        match Big_int.int64_of_big_int_opt bq, Big_int.int64_of_big_int_opt br with
        | Some q, Some r -> machine.rax := q; machine.rdx := r; [MCReg X86.Rdx; MCReg X86.Rax]
        | _, _ -> raise (Error ("Division overflow.", Some pos))
    with
    |Division_by_zero -> raise (Error ("Division by zero.", Some pos))

let decode_and_execute machine instr =
    let step_rip () =
        machine.rip := !(machine.rip) + 1
    in
    let (opcode, args, pos) = instr in
    match opcode with
    | X86.Movq ->
        let res = execute_movq machine args pos in step_rip (); res
    | X86.Pushq ->
        let res = execute_pushq machine args pos in step_rip (); res
    | X86.Popq ->
        let res = execute_popq machine args pos in step_rip (); res
    | X86.Leaq ->
        let res = execute_leaq machine args pos in step_rip (); res
    | X86.Incq ->
        let res = execute_incq machine args pos in step_rip (); res
    | X86.Decq ->
        let res = execute_decq machine args pos in step_rip (); res
    | X86.Negq ->
        let res = execute_negq machine args pos in step_rip (); res
    | X86.Notq ->
        let res = execute_notq machine args pos in step_rip (); res
    | X86.Addq ->
        let res = execute_addq machine args pos in step_rip (); res
    | X86.Subq ->
        let res = execute_subq machine args pos in step_rip (); res
    | X86.Imulq ->
        let res = execute_imulq machine args pos in step_rip (); res
    | X86.Xorq ->
        let res = execute_xorq machine args pos in step_rip (); res
    | X86.Orq ->
        let res = execute_orq machine args pos in step_rip (); res
    | X86.Andq ->
        let res = execute_andq machine args pos in step_rip (); res
    | X86.Shlq ->
        let res = execute_shlq machine args pos in step_rip (); res
    | X86.Sarq ->
        let res = execute_sarq machine args pos in step_rip (); res
    | X86.Shrq ->
        let res = execute_shrq machine args pos in step_rip (); res
    | X86.Jmp -> execute_jump None machine args pos (* jump/call/return operations set the rip appropriately. *)
    | X86.J cnd -> execute_jump (Some cnd) machine args pos (* jump/call/return operations set the rip appropriately. *)
    | X86.Cmpq ->
        let res = execute_cmpq machine args pos in step_rip (); res
    | X86.Set cnd ->
        let res = execute_set cnd machine args pos in step_rip (); res
    | X86.Callq -> execute_callq machine args pos (* jump/call/return operations set the rip appropriately. *)
    | X86.Retq -> execute_retq machine args pos (* jump/call/return operations set the rip appropriately. *)
    | X86.Cqto ->
        let res = execute_cqto machine args pos in step_rip (); res
    | X86.Idivq ->
        let res = execute_idivq machine args pos in step_rip (); res
    | X86.Comment _ -> raise InternalError
    
let take_step machine =
    match !(machine.mode) with
    | ExceptionHasOccured ->
        raise (Error ("The machine has crashed and therefore cannot take any steps anymore. Try reloading the code to start over. ", None))
    | Normal ->
        let cur_rip = !(machine.rip) in
        if (0 <= cur_rip && cur_rip < Array.length machine.prog) then
            begin
                try decode_and_execute machine (machine.prog.(cur_rip)) with
                | ex -> machine.mode := ExceptionHasOccured; raise ex
            end
        else
            begin
            machine.mode := ExceptionHasOccured;
            raise (Error ("Rip register is pointing outside the text section. The rip register is " ^(string_of_int cur_rip) ^ " while the program consists of " ^ (string_of_int (Array.length machine.prog)) ^ " instructions.", None))
            end

(* Creates a machine and loads the X86 program. *)
let create_machine (address_bits : int) (stack_size_bits : int) (prog : X86.prog) (entry_point : string) : machine =
    (* Adjust address_bits to between 16 and 26 bits, i.e., 64 KB to 64 MB. *)
    let address_bits_adjusted = if address_bits < 16 then 16 else if address_bits > 26 then 26 else address_bits in
    let memory_size = 1 lsl address_bits_adjusted in
    let inital_stack_pointer = Int64.of_int memory_size in
    (* Adjust address_bits to between 10 and 16 bits, i.e., 1 KB to 64 KB. *)
    let stack_size_bits_adjusted = if stack_size_bits < 10 then 10 else if stack_size_bits > 16 then 16 else stack_size_bits in
    let last_stack_address = memory_size - (1 lsl stack_size_bits_adjusted) in
    let mem = Memory.create Bigarray.Int8_unsigned Bigarray.c_layout memory_size in
    let prg_list = ref [] in
    let prg_lbls = ref LabelMap.empty in
    let data_lbls = ref LabelMap.empty in
    let least_free_address = ref 0 in
    let add_text_block lbl instrs =
        prg_lbls := LabelMap.add lbl (List.length !prg_list) !prg_lbls;
        prg_list := List.rev_append instrs !prg_list
    in
    let add_data_block lbl data =
        data_lbls := LabelMap.add lbl !least_free_address !data_lbls;
        let add_data_decl dt =
            match dt with
            | X86.Quad (X86.Lit n, _) ->
                begin
                    let offset = !least_free_address in
                    least_free_address := offset + 8;
                    store_64bits_to_memory mem offset (int64_to_bits n) None
                end
            | X86.Quad (X86.Lbl _, pos) -> raise (ErrorInitializingMachine ("Data declarations of the form \".quad label\" are not supported.", Some pos))
            | X86.Asciz (s, _) ->
                let offset = !least_free_address in
                least_free_address := offset + (String.length s + 1);
                String.iteri (fun i c -> Memory.set mem (offset + i) (Char.code c)) s;
                Memory.set mem (offset + String.length s) 0
        in
        List.iter add_data_decl data
    in
    let add_elem {X86.lbl; X86.asm; _} =
        match asm with
        | X86.Text instrs -> add_text_block lbl instrs
        | X86.Data data -> add_data_block lbl data
    in
    let initialize_machine prg = List.iter add_elem prg in
    initialize_machine prog;
    let inital_rip = 
        match LabelMap.find_opt entry_point !prg_lbls with
        | None -> raise (ErrorInitializingMachine ("Invalid Entry point: \"" ^ entry_point ^ "\".", None))
        | Some i -> i
    in
    let ms =
      {rip = ref inital_rip; rax = ref 0L; rbx = ref 0L; rcx = ref 0L; rdx = ref 0L; rsi = ref 0L; rdi = ref 0L;
       rbp = ref 0L; rsp = ref inital_stack_pointer; r8 = ref 0L; r9 = ref 0L; r10 = ref 0L; r11 = ref 0L; r12 = ref 0L;
       r13 = ref 0L; r14 = ref 0L; r15 = ref 0L;
       flags = make_flags ();
       heap_boundary = !least_free_address;
       stack_boundary = last_stack_address;
       mode = ref Normal;
       memory = mem;
       prog = Array.of_list (List.rev !prg_list);
       prog_labels = !prg_lbls;
       data_labels = !data_lbls}
    in
    if ms.heap_boundary > ms.stack_boundary then raise (ErrorInitializingMachine ("The heap and can stack overlap in the constructed machine.", None));
    ignore (execute_pushq ms [X86.Imm (X86.Lit (Int64.of_int (memory_size + 1024)))] Lexing.dummy_pos);
    ms