type flags = { flag_OF : bool ref; flag_SF : bool ref; flag_ZF : bool ref; flag_CF : bool ref; flag_PF : bool ref; }

let make_flags () = { flag_OF = ref false; flag_SF  = ref false; flag_ZF  = ref false; flag_CF  = ref false; flag_PF = ref false; }

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
        | _ -> assert false
    in
    nibble_to_hex (n / 16) ^ nibble_to_hex (n mod 16)

let int_byte_to_hex (n : int) =
    let bits = Array.init 8 (fun _ -> false) in
    bit_array_set_byte bits n 0;
    "0x" ^ (byte_to_hex bits 0)

let _64bits_to_hex bits =
    "0x" ^ (byte_to_hex bits 56) ^ (byte_to_hex bits 48) ^ (byte_to_hex bits 40) ^ (byte_to_hex bits 32) ^ (byte_to_hex bits 24) ^ (byte_to_hex bits 16) ^ (byte_to_hex bits 8) ^  (byte_to_hex bits 0)

let int64_to_hex_string (n : int64) = let bits = int64_to_bits n in _64bits_to_hex bits    

let calculate_static_flags b flags =
    flags.flag_SF := b.(Array.length b - 1);
    flags.flag_ZF := true;
    flags.flag_PF := true;
    let rec loop i =
        if i >= Array.length b then () else
        begin
            if b.(i) then 
                begin
                    flags.flag_ZF := false;
                    flags.flag_PF := not !(flags.flag_PF)
                end;
            loop (i + 1)
        end
    in
    loop 0

let bits_not b flags =
    let rec loop i =
        if i >= Array.length b then () else
        begin
            b.(i) <- not b.(i);
            loop (i + 1)
        end
    in
    loop 0;
    flags.flag_OF := false;
    flags.flag_CF := false;
    calculate_static_flags b flags

let bits_bin_log_op b1 b2 b3 f flags =
    assert (Array.length b1 = Array.length b2);
    assert (Array.length b1 = Array.length b3);
    let rec loop i =
        if i >= Array.length b1 then () else
        begin
            b3.(i) <- f b1.(i) b2.(i);
            loop (i + 1)
        end
    in
    loop 0;
    flags.flag_OF := false;
    flags.flag_CF := false;
    calculate_static_flags b3 flags
    
let bits_add b1 b2 b3 flags =
    assert (Array.length b1 = Array.length b2);
    assert (Array.length b1 = Array.length b3);
    let same_sign =
        b1.(Array.length b1 - 1) = b2.(Array.length b2 - 1)
    in
    let b1signbit = b1.(Array.length b1 - 1) in
    flags.flag_CF := false;
    let rec loop i =
        if i >= Array.length b1 then () else
        begin
            (match b1.(i), b2.(i), !(flags.flag_CF) with
            | true, true, true -> b3.(i) <- true
            | true, true, false
            | true, false, true
            | false, true, true -> b3.(i) <- false; flags.flag_CF := true
            | true, false, false
            | false, true, false
            | false, false, true -> b3.(i) <- true; flags.flag_CF := false
            | false, false, false -> b3.(i) <- false);
            loop (i + 1)
        end;
    in
    loop 0;
    flags.flag_OF := same_sign && b1signbit != b3.(Array.length b3 - 1);
    calculate_static_flags b3 flags

let shift_left n b flags =
    let set_to_zero () =
        let rec loop i =
            if i = Array.length b then () else
            begin
                b.(i) <- false;
                loop (i + 1)
            end;
        in
        loop 0
    in
    let shift_one () =
        flags.flag_CF := b.(Array.length b - 1); 
        let rec loop i =
            if i = -1 then () else
            begin
                b.(i) <- if i = 0 then false else b.(i - 1);
                loop (i - 1)
            end;
        in
        loop (Array.length b - 1)
    in
    if n >= Array.length b then set_to_zero () else
    begin
        let rec loop i =
            if i = 0 then () else
            begin
                shift_one (); loop (i - 1)
            end;
        in
        loop n
    end;
    if n = 1 then flags.flag_OF := b.(Array.length b - 1) <> !(flags.flag_CF);
    calculate_static_flags b flags

let shift_right n b flags =
    let msb = b.(Array.length b - 1) in
    let set_to_zero () =
        let rec loop i =
            if i >= Array.length b then () else
            begin
                b.(i) <- false;
                loop (i + 1)
            end;
        in
        loop 0
    in
    let shift_one () =
        flags.flag_CF := b.(0); 
        let rec loop i =
            if i >= Array.length b then () else
            begin
                b.(i) <- if i = Array.length b - 1 then false else b.(i + 1);
                loop (i + 1)
            end;
        in
        loop 0
    in
    if n > Array.length b then set_to_zero () else
    begin
        let rec loop i =
            if i = 0 then () else
            begin
                shift_one (); loop (i - 1)
            end;
        in
        loop n
    end;
    if n = 1 then flags.flag_OF := msb;
    calculate_static_flags b flags

let shift_right_arithmetic n b flags =
    let msb = b.(Array.length b - 1) in
    let set_all_to_msb () =
        let rec loop i =
            if i >= Array.length b then () else
            begin
                b.(i) <- msb;
                loop (i + 1)
            end;
        in
        loop 0
    in
    let shift_one () =
        flags.flag_CF := b.(0); 
        let rec loop i =
            if i >= Array.length b then () else
            begin
                b.(i) <- if i = Array.length b - 1 then msb else b.(i + 1);
                loop (i + 1)
            end;
        in
        loop 0
    in
    if n > Array.length b then set_all_to_msb () else
    begin
        let rec loop i =
            if i = 0 then () else
            begin
                shift_one (); loop (i - 1)
            end;
        in
        loop n
    end;
    if n = 1 then flags.flag_OF := false;
    calculate_static_flags b flags

(* this operation overwrites both b1 and b2. *)
let unsigned_mul b1 b2 b3 flags =
    assert (Array.length b1 = Array.length b2);
    assert (Array.length b1 = Array.length b3);
    let rec loop i =
        if i = Array.length b2 then () else
        begin
             if b2.(i) then bits_add b1 b3 b3 flags;
             shift_left 1 b1 flags;
             loop (i + 1)
        end
    in
    loop 0

let signed_mul b1 b2 b3 flags =
    assert (Array.length b1 = Array.length b2);
    assert (Array.length b1 = Array.length b3);
    let msb1 = b1.(Array.length b1 - 1) in
    let msb2 = b2.(Array.length b2 - 1) in
    let b1_big = Array.init (Array.length b1 * 2) (fun i -> if i < Array.length b1 then b1.(i) else msb1) in
    let b2_big = Array.init (Array.length b2 * 2) (fun i -> if i < Array.length b2 then b2.(i) else msb2) in
    let b3_big = Array.init (Array.length b3 * 2) (fun _ -> false) in
    let one = Array.init (Array.length b1 * 2) (fun i -> if i = 0 then true else false) in
    if msb1 then
        begin
            bits_not b1_big flags;
            bits_add b1_big one b1_big flags
        end;
    if msb2 then
        begin
            bits_not b2_big flags;
            bits_add b2_big one b2_big flags
        end;
    unsigned_mul b1_big b2_big b3_big flags;
    if msb1 <> msb2 then
        begin
            bits_not b3_big flags;
            bits_add b3_big one b3_big flags
        end;
    let rec copy_loop i =
        if i = Array.length b3 then () else
        begin
            b3.(i) <- b3_big.(i); copy_loop (i + 1)
        end
    in
    copy_loop 0;
    let msb3 = b3.(Array.length b3 - 1) in
    let rec check_loop i =
        if i = Array.length b3_big then false else
        begin
            if b3_big.(i) <> msb3 then true else check_loop (i + 1)
        end
    in
    let overflow = check_loop (Array.length b3) in
    flags.flag_OF := overflow; flags.flag_CF := overflow;
    calculate_static_flags b3 flags