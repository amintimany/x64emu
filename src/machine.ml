module Memory = Bigarray.Array1
module LabelMap = Map.Make(String)

type memory = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Memory.t

type segfault = BadJump

type machine_mode = Normal | ExceptionHasOccured | SegFault of segfault

exception ErrorInitializingMachine of string * Lexing.position option

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
flag_OF : bool ref;
flag_SF : bool ref;
flag_ZF : bool ref;
flag_CF : bool ref;
flag_PF : bool ref;
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

let int64_to_bits (n : int64) : bool array =
    let res = Array.init 64 (fun _ -> false) in
    let rec int64_to_bits_aux i (mask : int64) =
        if Int64.logand n mask = 0L then () else res.(i) <- true;
        if i = 63 then () else int64_to_bits_aux (i + 1) (Int64.shift_left mask 1)
    in
    int64_to_bits_aux 0 1L;
    res

let bits_to_int64 (b : bool array) : int64 =
    let res = ref 0L in
    let rec bits_to_int64_aux i =
        res := Int64.shift_left !res 1;
        if b.(i) then res := Int64.add !res 1L else ();
        if i = 0 then () else bits_to_int64_aux (i - 1)
    in
    bits_to_int64_aux 63;
    !res

(* Note: this converts little-endian. *)
let int64_to_bytes (i : int64) : (bool array * bool array * bool array * bool array * bool array * bool array * bool array * bool array) =
    let bits = int64_to_bits i in
    (* get bits with offset *)
    let gbo offset i = bits.(offset + i) in
    (Array.init 8 (gbo 0), Array.init 8 (gbo 8), Array.init 8 (gbo 16), Array.init 8 (gbo 24), 
     Array.init 8 (gbo 32), Array.init 8 (gbo 40), Array.init 8 (gbo 48), Array.init 8 (gbo 56))

(* Note: this converts little-endian. *)
let bytes_to_int64 (b0, b1, b2, b3, b4, b5, b6, b7) : int64 =
    let get_bit i =
      match i with
      | _ when (i < 8) -> b0.(i)
      | _ when (8 <= i && i < 16) -> b1.(i - 8)
      | _ when (16 <= i && i < 24) -> b2.(i - 16)
      | _ when (24 <= i && i < 32) -> b3.(i - 24)
      | _ when (32 <= i && i < 40) -> b4.(i - 32)
      | _ when (40 <= i && i < 48) -> b5.(i - 40)
      | _ when (48 <= i && i < 56) -> b6.(i - 48)
      | _ when (56 <= i && i < 64) -> b7.(i - 56)
      | _ -> false
    in
    bits_to_int64 (Array.init 64 get_bit)

let bit_array_get_byte (b : bool array) (offset : int) =
    let res = ref 0 in
    let rec bit_array_get_byte_aux i =
        res := !res lsl 1;
        if b.(offset + i) then res := !res + 1 else ();
        if i = 0 then () else bit_array_get_byte_aux (i - 1)
    in
bit_array_get_byte_aux 7; !res

let bit_array_set_byte (b : bool array) (n : int) (offset : int) =
    let rec bit_array_set_byte_aux i (mask : int) =
        if n land mask = 0 then () else b.(offset + i) <- true;
        if i = 7 then () else bit_array_set_byte_aux (i + 1) (mask lsl 1)
      in
      bit_array_set_byte_aux 0 1

let byte_to_hex (b : bool array) (offset : int) =
    let n = bit_array_get_byte b offset in
    let nibble_to_hex nb =
        match nb with
        | 0 -> "0"
        | 1 -> "1"
        | 2 -> "2"
        | 3 -> "3"
        | 4 -> "4"
        | 5 -> "5"
        | 6 -> "6"
        | 7 -> "7"
        | 8 -> "8"
        | 9 -> "9"
        | 10 -> "A"
        | 11 -> "B"
        | 12 -> "C"
        | 13 -> "D"
        | 14 -> "E"
        | 15 -> "F"
        | _ -> raise (ErrorInitializingMachine ("Internal error, please report", None))
    in
    nibble_to_hex (n / 16) ^ nibble_to_hex (n mod 16)

let int_byte_to_hex (n : int) =
    let bits = Array.init 8 (fun _ -> false) in
    bit_array_set_byte bits n 0;
    "0x" ^ (byte_to_hex bits 0)

let int64_to_hex_string (n : int64) =
    let bits = int64_to_bits n in
    "0x" ^ (byte_to_hex bits 56) ^ (byte_to_hex bits 48) ^ (byte_to_hex bits 40) ^ (byte_to_hex bits 32) ^ (byte_to_hex bits 24) ^ (byte_to_hex bits 16) ^ (byte_to_hex bits 8) ^  (byte_to_hex bits 0)
    

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
                    let (b0, b1, b2, b3, b4, b5, b6, b7) = int64_to_bytes n in
                    Memory.set mem offset (bit_array_get_byte b0 0);
                    Memory.set mem (offset + 1) (bit_array_get_byte b1 0);
                    Memory.set mem (offset + 2) (bit_array_get_byte b2 0);
                    Memory.set mem (offset + 3) (bit_array_get_byte b3 0);
                    Memory.set mem (offset + 4) (bit_array_get_byte b4 0);
                    Memory.set mem (offset + 5) (bit_array_get_byte b5 0);
                    Memory.set mem (offset + 6) (bit_array_get_byte b6 0);
                    Memory.set mem (offset + 7) (bit_array_get_byte b7 0);
                end
            | X86.Quad (X86.Lbl _, pos) -> raise (ErrorInitializingMachine ("Data declarations of the form \".quad label\" are not supported", Some pos))
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
        | None -> raise (ErrorInitializingMachine ("Invalid Entry point: \"" ^ entry_point ^ "\"", None))
        | Some i -> i
    in
    let ms =
      {rip = ref inital_rip; rax = ref 0L; rbx = ref 0L; rcx = ref 0L; rdx = ref 0L; rsi = ref 0L; rdi = ref 0L;
       rbp = ref 0L; rsp = ref inital_stack_pointer; r8 = ref 0L; r9 = ref 0L; r10 = ref 0L; r11 = ref 0L; r12 = ref 0L;
       r13 = ref 0L; r14 = ref 0L; r15 = ref 0L;
       flag_OF = ref false; flag_SF = ref false; flag_ZF = ref false; flag_CF = ref false;
       flag_PF = ref false;
       heap_boundary = !least_free_address;
       stack_boundary = last_stack_address;
       mode = ref Normal;
       memory = mem;
       prog = Array.of_list (List.rev !prg_list);
       prog_labels = !prg_lbls;
       data_labels = !data_lbls}
    in
    if ms.heap_boundary > ms.stack_boundary then raise (ErrorInitializingMachine ("The heap and can stack overlap in the constructed machine.", None));
    ms