open Js_of_ocaml


module Html = Dom_html

let onload _ =
let doc = Html.document in
let x64emu_load_result =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_load_result")) Dom_html.CoerceTo.div (fun _ -> assert false)
in
let x64emu_load_code_button =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_load_code_button")) Dom_html.CoerceTo.button (fun _ -> assert false)
in
let editor = Js.Unsafe.variable "editor" in
let () = x64emu_load_code_button##.onclick := 
Html.handler (fun _ -> 
  let src = Js.Unsafe.meth_call editor "getValue" [||] in
  let src_str = Js.to_string src in
  let response = X64emumain.load_the_code src_str in
  x64emu_load_result##.innerHTML := Js.string response; Js.bool false) in
Js.bool true

let _ = Html.window##.onload := Html.handler onload