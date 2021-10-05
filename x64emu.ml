open Js_of_ocaml


module Html = Dom_html

let load_the_code (code : string) = code

let onload _ =
let doc = Html.document in
let x64emu_load_result =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_load_result")) Dom_html.CoerceTo.div (fun _ -> assert false)
in
let x64emu_the_code =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_the_code")) Dom_html.CoerceTo.textarea (fun _ -> assert false)
in
let x64emu_load_code_button =
  Js.coerce_opt (doc##getElementById (Js.string "x64emu_load_code_button")) Dom_html.CoerceTo.button (fun _ -> assert false)
in
let () = x64emu_load_code_button##.onclick := Html.handler (fun _ -> let x = Js.string (load_the_code (Js.to_string x64emu_the_code##.value)) in x64emu_load_result##.innerHTML := x; Js.bool false) in
Js.bool true

let _ = Html.window##.onload := Html.handler onload