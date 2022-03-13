open Js_of_ocaml
open Js.Unsafe

type nonrec any = any
type 'x promise = ('x, any) Promise.promise

external cast : 'a -> 'b = "%identity"

let jsbool x = x |> Js.bool |> coerce
let jsstr str = str |> Js.string |> coerce
let ocstr js = js |> coerce |> Js.to_string
let ocint js = js |> cast |> int_of_float

let ocstr' js = js |> Js.to_string
let jsstr' str = str |> Js.string

let sprintf fmt = Format.asprintf fmt
let jsprintf fmt = Format.kasprintf jsstr fmt

let jsvar_opt varident =
  let js = Js.Unsafe.eval_string Format.(
      sprintf "typeof(%s)==='undefined'?null:%s" varident varident) in
  Js.some js |> Js.Opt.to_option

let info fmt =
  let kont msg =
    fun_call (global##.console##.log) [|msg|>Js.string|>coerce|] |> ignore
  in Format.kasprintf kont fmt
let debug fmt =
  let kont msg =
    fun_call (global##.console##.error) [|msg|>Js.string|>coerce|] |> ignore
  in Format.kasprintf kont fmt
let debug_obj x =
  fun_call (global##.console##.error) [|x|>inject|] |> ignore

let json_parse str = fun_call global##.JSON##.parse [| str |]
let json_stringify obj = fun_call global##.JSON##.stringify [| obj |]

let xmlhttprequest() =
  let constr = Js.Unsafe.pure_js_expr "XMLHttpRequest" in
  new%js constr

let http_request
      ?headers:(headers=[])
      ~meth:(meth:[`Get | `Put | `Post])
      ?body ~url () =
  let meth_string = match meth with
    | `Get -> "GET"
    | `Post -> "POST"
    | `Put -> "PUT" in
  let func res rej =
    let open Js.Unsafe in
    let xhr = xmlhttprequest() in
    xhr##open_
      (jsstr meth_string)
      (jsstr url)
      (jsbool true) (* async request? *)
    |> ignore;
    headers |!> (fun (h, v) ->
      xhr##setRequestHeader (jsstr h) (jsstr v)
    );
    (match meth, body with
     | `Get, Some _ -> invalid_arg "GET method cannot take body"
     | `Get, None -> xhr##send
     | _, Some body -> xhr##send (jsstr body)
     | _ -> ());
    xhr##.onreadystatechange := (fun () ->
      match xhr##.readyState |> ocint with
      | 4 ->
         let status = xhr##.status |> ocint in
         if status < 200 || status > 200
         then rej (xhr : any)
         else res (xhr##.responseText |> ocstr)
      | _ -> ()
    ) in
  Promise.make func

let js_of_yojson json : any =
  json |> Yojson.Safe.to_string
  |> jsstr |> json_parse
let yojson_of_js any : Yojson.Safe.t =
  any |> json_stringify |> ocstr
  |> Yojson.Safe.from_string
