open Js_of_ocaml
open Lexing
open X64emumain

module Html = Dom_html

let success_message =
  "<div class=\"alert alert-dismissible alert-success\"><strong>Successfully loaded the code.</strong></div>"

let format_position p = Printf.sprintf "%d : %d" p.pos_lnum (p.pos_cnum - p.pos_bol + 1)

let make_error tag msg p =
  match p with
  | Some pos ->
      Printf.sprintf
      "<div class=\"alert alert-dismissible alert-danger\"><strong>%s</strong>: %s @ %s</div>"
      tag msg (format_position pos)
  | None ->
    Printf.sprintf
      "<div class=\"alert alert-dismissible alert-danger\"><strong>%s</strong>: %s</div>" tag msg

let make_register_display doc name x64emu_register_table =
  let th = Html.createTh doc in
  th##.scope := Js.string "row";
  th##.innerHTML := Js.string name;
  let td = Html.createTd doc in
  let tr_int = Html.createTr doc in
  let td_int = Html.createTd doc in
  let tr_hex = Html.createTr doc in
  let td_hex = Html.createTd doc in
  Dom.appendChild tr_int th;
  Dom.appendChild tr_int td_int;
  Dom.appendChild tr_hex td;
  Dom.appendChild tr_hex td_hex;
  Dom.appendChild x64emu_register_table tr_int;
  Dom.appendChild x64emu_register_table tr_hex;
  let update_reg n =
    td_int##.innerHTML := Js.string (Int64.to_string n);
    td_hex##.innerHTML := Js.string (Binops.int64_to_hex_string n)
  in
  update_reg

let make_register_display_for_machine doc machine x64emu_register_table =
  x64emu_register_table##.innerHTML := Js.string "";
  let raxupd = make_register_display doc "%rax" x64emu_register_table in
  let rbxupd = make_register_display doc "%rbx" x64emu_register_table in
  let rcxupd = make_register_display doc "%rcx" x64emu_register_table in
  let rdxupd = make_register_display doc "%rdx" x64emu_register_table in
  let rsiupd = make_register_display doc "%rsi" x64emu_register_table in
  let rdiupd = make_register_display doc "%rdi" x64emu_register_table in
  let rbpupd = make_register_display doc "%rbp" x64emu_register_table in
  let rspupd = make_register_display doc "%rsp" x64emu_register_table in
  let r8upd = make_register_display doc "%r8" x64emu_register_table in
  let r9upd = make_register_display doc "%r9" x64emu_register_table in
  let r10upd = make_register_display doc "%r10" x64emu_register_table in
  let r11upd = make_register_display doc "%r11" x64emu_register_table in
  let r12upd = make_register_display doc "%r12" x64emu_register_table in
  let r13upd = make_register_display doc "%r13" x64emu_register_table in
  let r14upd = make_register_display doc "%r14" x64emu_register_table in
  let r15upd = make_register_display doc "%r15" x64emu_register_table in
  (fun () ->
    raxupd !(machine.Machine.rax);
    rbxupd !(machine.Machine.rbx);
    rcxupd !(machine.Machine.rcx);
    rdxupd !(machine.Machine.rdx);
    rsiupd !(machine.Machine.rsi);
    rdiupd !(machine.Machine.rdi);
    rbpupd !(machine.Machine.rbp);
    rspupd !(machine.Machine.rsp);
    r8upd !(machine.Machine.r8);
    r9upd !(machine.Machine.r9);
    r10upd !(machine.Machine.r10);
    r11upd !(machine.Machine.r11);
    r12upd !(machine.Machine.r12);
    r13upd !(machine.Machine.r13);
    r14upd !(machine.Machine.r14);
    r15upd !(machine.Machine.r15))

let make_flags_display_for_machine doc machine x64emu_flags_table =
  x64emu_flags_table##.innerHTML := Js.string "";
  let tr_head = Html.createTr doc in
  tr_head##.innerHTML := Js.string "<th>OF</th><th>SF</th><th>ZF</th><th>CF</th><th>PF</th>";
  Dom.appendChild x64emu_flags_table tr_head;
  let tr = Html.createTr doc in
  let td_OF = Html.createTd doc in
  let td_SF = Html.createTd doc in
  let td_ZF = Html.createTd doc in
  let td_CF = Html.createTd doc in
  let td_PF = Html.createTd doc in
  Dom.appendChild tr td_OF;
  Dom.appendChild tr td_SF;
  Dom.appendChild tr td_ZF;
  Dom.appendChild tr td_CF;
  Dom.appendChild tr td_PF;
  Dom.appendChild x64emu_flags_table tr;
  (fun () ->
    td_OF##.innerHTML := Js.string (if !(machine.Machine.flags.Binops.flag_OF) then "1" else "0");
    td_SF##.innerHTML := Js.string (if !(machine.Machine.flags.Binops.flag_SF) then "1" else "0");
    td_ZF##.innerHTML := Js.string (if !(machine.Machine.flags.Binops.flag_ZF) then "1" else "0");
    td_CF##.innerHTML := Js.string (if !(machine.Machine.flags.Binops.flag_CF) then "1" else "0");
    td_PF##.innerHTML := Js.string (if !(machine.Machine.flags.Binops.flag_PF) then "1" else "0"))

let make_heap_cell_display doc address machine memory_table =
  let tr = Html.createTr doc in
  let td_label = Html.createTd doc in
  let td_address = Html.createTd doc in
  let td_value = Html.createTd doc in
  Dom.appendChild tr td_label;
  Dom.appendChild tr td_address;
  Dom.appendChild tr td_value;
  let make_address (n : int) =
    Binops.int64_to_hex_string (Int64.of_int n)
  in
  let make_value (n : int) =
    Binops.int_byte_to_hex n
  in
  td_address##.innerHTML := Js.string (make_address address);
  td_value##.innerHTML := Js.string (make_value (Machine.Memory.get machine.Machine.memory address));
  Dom.appendChild memory_table tr;
  (td_label, fun () -> td_value##.innerHTML := Js.string (make_value (Machine.Memory.get machine.Machine.memory address)))

let make_heap_display_for_machine doc display_limit machine x64emu_heap_table =
x64emu_heap_table##.innerHTML := Js.string "";
let tr_head = Html.createTr doc in
tr_head##.innerHTML := Js.string "<th>Label</th><th>Address</th><th>Data</th>";
Dom.appendChild x64emu_heap_table tr_head;
let update_funs = ref [||] in
fun () ->
  let last_limit = !display_limit in
  display_limit := if machine.Machine.heap_boundary + 1 < last_limit then last_limit else machine.Machine.heap_boundary + 1;
  let new_limit = !display_limit in
  if new_limit > last_limit then
    begin
      let tmp = Array.init (new_limit - last_limit) (fun _ -> ()) in
      let newcells = Array.mapi (fun i _ -> make_heap_cell_display doc (last_limit + i) machine x64emu_heap_table) tmp in
      Machine.LabelMap.iter
      (fun lbl addr ->
        if last_limit <= addr && addr < new_limit then
          let (tdl, _) = newcells.(addr - last_limit) in tdl##.innerHTML := Js.string lbl
        else ()) machine.Machine.data_labels;
      update_funs := Array.concat [!update_funs; (Array.map (fun (_, upd) -> upd) newcells)]
    end;
  Array.iter (fun upd -> upd ()) !update_funs
    
let make_stack_display_for_machine doc display_limit machine x64emu_stack_table x64emu_scroll_to_rsp =
  let last_row = ref Js.null in
  x64emu_stack_table##.innerHTML := Js.string "";
  let make_tr_label num = "x64emu_stack_row_" ^ string_of_int num in
  let make_address (n : int) =
    Binops.int64_to_hex_string (Int64.of_int n)
  in
  let make_value (n : int) =
    Binops.int_byte_to_hex n
  in
  let update_row (td_value, address) =
    td_value##.innerHTML := Js.string (make_value (Machine.Memory.get machine.Machine.memory address))
  in
  let make_stack_row address =
    let tr = Html.createTr doc in
    let td_address = Html.createTd doc in
    let td_value = Html.createTd doc in
    Dom.appendChild tr td_address;
    Dom.appendChild tr td_value;
    tr##.id := Js.string (make_tr_label address);
    td_address##.innerHTML := Js.string (make_address address);
    td_value##.innerHTML := Js.string (make_value (Machine.Memory.get machine.Machine.memory address));
    Dom.insertBefore x64emu_stack_table tr !last_row; last_row := Js.some tr;
    (td_value, address)
  in
  let tr_head = Html.createTr doc in
  tr_head##.innerHTML := Js.string "<th>Address</th><th>Data</th>";
  Dom.appendChild x64emu_stack_table tr_head;
  let all_rows = ref [||] in
  let toggle_table_active_fun = Js.Unsafe.variable "toggle_table_active" in
  let bring_to_view_fun = Js.Unsafe.variable "bring_to_view" in
  let bring_to_view num = ignore (Js.Unsafe.fun_call bring_to_view_fun [|Js.Unsafe.inject (Js.string (make_tr_label num))|]) in
  let last_rsp = ref (Int64.to_int !(machine.Machine.rsp)) in
  let update_table_active num = 
    let lrsp = !last_rsp in
    if 0 <= lrsp && lrsp < Machine.Memory.dim machine.memory then
      begin
        ignore (Js.Unsafe.fun_call toggle_table_active_fun [|Js.Unsafe.inject (Js.string (make_tr_label !last_rsp)); Js.Unsafe.inject (Js.bool false)|])
      end;
      if 0 <= num && num < Machine.Memory.dim machine.memory then
        begin
          ignore (Js.Unsafe.fun_call toggle_table_active_fun [|Js.Unsafe.inject (Js.string (make_tr_label num)); Js.Unsafe.inject (Js.bool true)|]);
          x64emu_scroll_to_rsp##.onclick := Html.handler (fun _ -> bring_to_view num; Js.bool false)
        end;
      last_rsp := num
  in
  fun () ->
    let last_limit = !display_limit in
    let current_rsp = Int64.to_int !(machine.Machine.rsp) in
    let candidate_new_limit = if (current_rsp - 160) > (machine.Machine.stack_boundary) then (current_rsp - 160) else (machine.Machine.stack_boundary) in
    display_limit := if candidate_new_limit > last_limit then last_limit else candidate_new_limit;
    let new_limit = !display_limit in
    if new_limit < last_limit then
      begin
        let newcells = Array.init (last_limit - new_limit) (fun i -> make_stack_row (last_limit - i - 1)) in
        all_rows := Array.concat [!all_rows; newcells]
      end;
    Array.iter (fun tda -> update_row tda) !all_rows; update_table_active current_rsp


let make_program_display_for_machine doc machine x64emu_program_table x64emu_scroll_to_rip =
  x64emu_program_table##.innerHTML := Js.string "";
  let tr_head = Html.createTr doc in
  tr_head##.innerHTML := Js.string "<th>Label</th><th>Instruction</th>";
  Dom.appendChild x64emu_program_table tr_head;
  let make_tr_label num = "x64emu_prog_instr_" ^ string_of_int num in
  let make_instr num ins =
    let tr = Html.createTr doc in
    let label_td = Html.createTd doc in
    let instr_td = Html.createTd doc in
    instr_td##.innerHTML := Js.string (X86.string_of_ins ins);
    tr##.id := Js.string (make_tr_label num);
    Dom.appendChild tr label_td;
    Dom.appendChild tr instr_td;
    Dom.appendChild x64emu_program_table tr;
    (tr, label_td)
  in
  let res = Array.mapi make_instr machine.Machine.prog in
  Machine.LabelMap.iter (fun lbl addr -> let (_, td) = res.(addr) in td##.innerHTML := Js.string lbl) (machine.Machine.prog_labels);
  let last_rip = ref !(machine.Machine.rip) in
  let toggle_table_active_fun = Js.Unsafe.variable "toggle_table_active" in
  let bring_to_view_fun = Js.Unsafe.variable "bring_to_view" in
  let bring_to_view num = ignore (Js.Unsafe.fun_call bring_to_view_fun [|Js.Unsafe.inject (Js.string (make_tr_label num))|]) in
  let update_table_active num =
    let lrip = !last_rip in
    if 0 <= lrip && lrip < Array.length machine.prog then
      begin
        ignore (Js.Unsafe.fun_call toggle_table_active_fun [|Js.Unsafe.inject (Js.string (make_tr_label lrip)); Js.Unsafe.inject (Js.bool false)|])
      end;
      if 0 <= num && num < Array.length machine.prog then
        begin
          ignore (Js.Unsafe.fun_call toggle_table_active_fun [|Js.Unsafe.inject (Js.string (make_tr_label num)); Js.Unsafe.inject (Js.bool true)|]);
          x64emu_scroll_to_rip##.onclick := Html.handler (fun _ -> bring_to_view num; Js.bool false)
        end;
      last_rip := num
  in
  (fun () -> update_table_active !(machine.Machine.rip))

let make_machine_display doc machine x64emu_register_table x64emu_program_table x64emu_scroll_to_rip x64emu_flags_table x64emu_heap_table x64emu_stack_table x64emu_scroll_to_rsp =
  let register_update = make_register_display_for_machine doc machine x64emu_register_table in
  let program_update = make_program_display_for_machine doc machine x64emu_program_table x64emu_scroll_to_rip in
  let flags_update = make_flags_display_for_machine doc machine x64emu_flags_table in
  let heap_update = make_heap_display_for_machine doc (ref 0) machine x64emu_heap_table in
  let stack_update = make_stack_display_for_machine doc (ref (Machine.Memory.dim machine.memory)) machine x64emu_stack_table x64emu_scroll_to_rsp in
  (fun () -> register_update (); program_update (); flags_update (); heap_update (); stack_update ())

let onload _ =
let doc = Html.document in
let x64emu_load_result =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_load_result")) Html.CoerceTo.div (fun _ -> assert false)
in
let x64emu_entry_point =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_entry_point")) Html.CoerceTo.input (fun _ -> assert false)
in
let x64emu_load_code_button =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_load_code_button")) Html.CoerceTo.button (fun _ -> assert false)
in
let x64emu_register_table =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_register_table")) Html.CoerceTo.tbody (fun _ -> assert false)
in
let x64emu_program_table =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_program_table")) Html.CoerceTo.tbody (fun _ -> assert false)
in
let x64emu_flags_table =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_flags_table")) Html.CoerceTo.tbody (fun _ -> assert false)
in
let x64emu_heap_table =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_heap_table")) Html.CoerceTo.tbody (fun _ -> assert false)
in
let x64emu_stack_table =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_stack_table")) Html.CoerceTo.tbody (fun _ -> assert false)
in
let x64emu_take_steps =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_take_steps")) Html.CoerceTo.button (fun _ -> assert false)
in
let x64emu_num_steps =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_num_steps")) Html.CoerceTo.input (fun _ -> assert false)
in
let x64emu_scroll_to_rip =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_scroll_to_rip")) Html.CoerceTo.button (fun _ -> assert false)
in
let x64emu_scroll_to_rsp =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_scroll_to_rsp")) Html.CoerceTo.button (fun _ -> assert false)
in
let clean_up () =
  x64emu_scroll_to_rsp##.onclick := Html.handler (fun _ -> Js.bool false);
  x64emu_scroll_to_rip##.onclick := Html.handler (fun _ -> Js.bool false);
  x64emu_take_steps##.onclick := Html.handler (fun _ -> Js.bool false);
  x64emu_stack_table##.innerHTML := Js.string "";
  x64emu_heap_table##.innerHTML := Js.string "";
  x64emu_flags_table##.innerHTML := Js.string "";
  x64emu_program_table##.innerHTML := Js.string "";
  x64emu_register_table##.innerHTML := Js.string "";
  x64emu_load_result##.innerHTML := Js.string ""
in
let get_num_steps () = int_of_string_opt (Js.to_string x64emu_num_steps##.value) in
let editor = Js.Unsafe.variable "editor" in
let () = x64emu_load_code_button##.onclick := 
Html.handler (fun _ ->
  clean_up ();
  let src = Js.Unsafe.meth_call editor "getValue" [||] in
  let src_str = Js.to_string src in
  let entry_point = Js.to_string x64emu_entry_point##.value in
  let response = 
    if entry_point = "" then 
      begin x64emu_load_result##.innerHTML := Js.string (make_error "Invalid entry point" "\"\"" None); None end
    else
      Some (X64emumain.load_the_code src_str entry_point )
  in
  let () =
    match response with
    | None -> ()
    | Some LexingError (err, p) -> x64emu_load_result##.innerHTML := Js.string (make_error "Lexing Error" err (Some p))
    | Some ParsingError (err, p) -> x64emu_load_result##.innerHTML := Js.string (make_error "Parsing Error" err p)
    | Some LoadingError (err, p) -> x64emu_load_result##.innerHTML := Js.string (make_error "Error in loading the program" err p)
    | Some Ok machine ->
      let update_display =
        make_machine_display doc machine x64emu_register_table x64emu_program_table x64emu_scroll_to_rip x64emu_flags_table x64emu_heap_table x64emu_stack_table x64emu_scroll_to_rsp
      in
      update_display ();
      x64emu_take_steps##.onclick :=
      Html.handler (fun _ ->
        (match get_num_steps () with
        | Some n ->
          let rounds_left = ref n in
          begin
            try
              let rec loop () =
                if !rounds_left = 0 then () else (Machine.take_step machine; rounds_left := !rounds_left - 1; loop ()) 
              in
              loop ()
            with
            | Machine.InternalError ->
                Html.window##alert (Js.string "An internal error has occured. This is likely a bug, please report.")
            | Machine.Error (msg, opos) ->
              x64emu_load_result##.innerHTML := Js.string (make_error "Error while executing the loaded program" msg opos);
              Html.window##alert (Js.string ("An error occured! Only executed " ^ (string_of_int (n - !rounds_left)) ^ " steps successfully. See the error message in the error box."))
          end;
          update_display ()
        | None -> Html.window##alert (Js.string ((Js.to_string x64emu_entry_point##.value) ^ " is not a valid number.")));
        Js.bool false)
  in
  Js.bool false)
in
Js.bool true

let _ = Html.window##.onload := Html.handler onload