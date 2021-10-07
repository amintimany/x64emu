open Js_of_ocaml
open Lexing
open X64emumain

module Html = Dom_html

let success_message =
  "<div class=\"alert alert-dismissible alert-success\"><strong>Successfully loaded the code.</strong></div>"

let make_error tag msg p =
  Printf.sprintf
  "<div class=\"alert alert-dismissible alert-danger\"><strong>%s</strong>: %s @ %d : %d</div>"
  tag msg p.pos_lnum (p.pos_cnum - p.pos_bol + 1)

let onload _ =
let doc = Html.document in
let x64emu_load_result =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_load_result")) Html.CoerceTo.div (fun _ -> assert false)
in
let x64emu_load_code_button =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_load_code_button")) Html.CoerceTo.button (fun _ -> assert false)
in
(* let x64emu_check_box_show_parsing =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_check_box_show_parsing")) Html.CoerceTo.input (fun _ -> assert false)
in *)
let editor = Js.Unsafe.variable "editor" in
let () = x64emu_load_code_button##.onclick := 
Html.handler (fun _ -> 
  let src = Js.Unsafe.meth_call editor "getValue" [||] in
  let src_str = Js.to_string src in
  (* let show_details = 
    Js.to_bool (x64emu_check_box_show_parsing##.checked)
  in *)
  let response = X64emumain.load_the_code src_str in
  let () =
    match response with
    | Ok _ -> x64emu_load_result##.innerHTML := Js.string success_message
    | LexingError (err, p) -> x64emu_load_result##.innerHTML := Js.string (make_error "Lexing Error" err p)
  in
  Js.bool false)
in
Js.bool true

let _ = Html.window##.onload := Html.handler onload