open Agency_email.Ses_backend
open Lwt.Infix

let pp_sexp = Sexplib.Sexp.pp_hum
let print_sexp = Format.printf "%a@." pp_sexp

let config = {
    aws_access_key_id = Sys.getenv "AWS_ACCESS_KEY_ID";
    aws_secret_access_key = Sys.getenv "AWS_SECRET_ACCESS_KEY";
    aws_region = `NorthVirginia;
    aws_host = None;
    aws_endpoint = None;
  }

open SESV2Backend

exception MissingArgument

let send_email_demo () =
  let read_prompt text =
    print_string text;
    flush stdout;
    read_line ()
  in
  let to_email =
    match ArgOptions.get_option (StringOption "-to") with
    | Some addr -> addr
    | None -> raise MissingArgument
  in
  let from_email =
    match ArgOptions.get_option (StringOption "-from") with
    | Some addr -> addr
    | None -> raise MissingArgument
  in
  let subject = read_prompt "Subject: " in
  let main_text = read_prompt "Text: " in
  send_email ~config ~from:from_email ~recipients:[to_email] ~cc:[] ~bcc:[]
    ~subject ~body:(`TextBody main_text) >>=
    fun msgid ->
    (match msgid with
     | Ok id -> print_endline ("Message ID: " ^ id)
     | Error err -> print_endline ("Error: " ^ err));
    Lwt.return_unit

let _ = Lwt_main.run (send_email_demo ())
