open Agency_persistency

open Lwt.Infix
(* open Cohttp_lwt_unix *)
open Dynamo_backend
(* open DynamoBackend *)

let pp_sexp = Sexplib.Sexp.pp_hum 
let print_sexp = Format.printf "%a@." pp_sexp

let store = {
  aws_access_key_id = Sys.getenv "ACCESS_KEY_ID";
  aws_secret_access_key = Sys.getenv "SECRET_ACCESS_KEY";
  aws_region = `NorthVirginia;
  aws_endpoint = None;

  dynamo_table_name = "tsca-dev-test";
  
  dynamo_key_attribute_name = "object-key";
  dynamo_key_type = `Hash;

  dynamo_object_attribute_name = "object-value";
  dynamo_object_type = `String
}

(* let print_describe =
  describe_table store >>=
  fun (resp, body) -> print_sexp (Response.sexp_of_t resp); Cohttp_lwt.Body.to_string body >|= print_endline

let print_get =
  get_item store "key1" >>=
   fun (resp, body) -> print_sexp (Response.sexp_of_t resp); Cohttp_lwt.Body.to_string body >|= print_endline *)

let get_it =
  DynamoBackend.put_object_return_old ~store:store ~key:"key1" "testVal12" >>=
  fun res ->
  (match res with
   | Error _ -> print_endline "error"
   | Ok None -> print_endline "not overwriting"
   | Ok (Some r) -> print_endline r);
  DynamoBackend.get_object_opt ~store:store ~key:"key1" >>=
  fun res ->
  (match res with
  | Error _ -> print_endline "error"
  | Ok None -> print_endline "not found"
  | Ok (Some r) -> print_endline r); Lwt.return_unit

let _ = Lwt_main.run get_it
