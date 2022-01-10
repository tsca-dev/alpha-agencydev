open Agency_persistency
open Lwt.Infix
open Dynamo_backend

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

exception Bad_result

let put_get key value =
  DynamoBackend.put_object ~store ~key value >>=
  fun _ -> DynamoBackend.get_object_opt ~store ~key >>=
  fun res ->
  DynamoBackend.delete_object ~store ~key >>=
  fun _ ->
  match res with
   | Error _ -> Lwt.fail Bad_result
   | Ok None -> Lwt.fail Bad_result
   | Ok (Some r) -> Lwt.return r

let run_put_get key value = Lwt_main.run (put_get key value)

let put_get_test =
  let test_key = "Test-Key" ^ string_of_int (Random.int 10000) in
  let test_val = "Test-Val" ^ string_of_int (Random.int 10000) in
  Alcotest.
    (test_case ("Put-Get") `Slow
     @@ fun () ->
     (check string) "Checking put-get"
       (run_put_get test_key test_val) test_val
    )

let () =
  Alcotest.run __FILE__
    [ "put-get", [put_get_test] ]
