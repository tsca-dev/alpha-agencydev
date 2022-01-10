open Base
open Sign
open Lwt.Infix
open Cohttp_lwt_unix

module type StorageBackend = sig
  type store
  type config

  (* call DescribeTable *)
  val open_store : config -> store aresult

  (* Query for has_object => get_object_* *)
  val has_object :
    store:store -> key:string
    -> bool aresult
  val get_object_opt :
    store:store -> key:string
    -> string option aresult
  val get_object_err :
    store:store -> key:string
    -> string aresult

  (* PutItem for put_object_* *)
  val put_object :
    store:store -> key:string
    -> string -> unit aresult
  val put_object_return_old :
    store:store -> key:string
    -> string -> string option aresult
  (** returning the old object,
      or None if no object associate with the given key *)

  (* DeleteItem *)
  val delete_object :
    store:store -> key:string
    -> unit aresult
    (** Warning: it is not going to return an error if the key does
        not exists. *)
end

type aws_region = [
  | `Tokyo
  | `NorthVirginia
  | `Oregon ]

type dynamo_config = {
    aws_access_key_id : string;
    aws_secret_access_key : string;
    aws_region : aws_region;
    aws_endpoint : string option;

    dynamo_table_name : string;
    
    dynamo_key_attribute_name : string;
    dynamo_key_type : [ `Hash ];

    dynamo_object_attribute_name : string;
    dynamo_object_type : [ `String | `Binary ];
  }

let stringify_object_type = function
  | `String -> "S"
  | `Binary -> "B"

let stringify_region = function
  | `Tokyo -> "ap-northeast-1"
  | `NorthVirginia -> "us-east-1"
  | `Oregon -> "us-west-2"

let default_host region =
  "dynamodb." ^ stringify_region region ^ ".amazonaws.com"

let host_of config =
  match config.aws_endpoint with
  | None -> default_host config.aws_region
  | Some endpoint -> endpoint

let endpoint_of config = "https://" ^ host_of config

let sign_dynamodb_header config header body operation =
  let host = host_of config in
  let (_, _, signed) = sign_request
    ~access_key:config.aws_access_key_id
    ~secret_key:config.aws_secret_access_key
    ~service:"dynamodb"
    ~region:(stringify_region config.aws_region)
    ~host:host
    ~body
    ~target:("DynamoDB_20120810." ^ operation)
    (`POST, Uri.of_string host, header)
  in signed

let make_header config body op = sign_dynamodb_header config [
    ("Accept-Encoding", "identity");
    ("Host", host_of config);
    ("Content-Type", "application/x-amz-json-1.0");
    ("User-Agent", "ocaml");
  ] body op |> Cohttp.Header.of_list

(* Bindings for DynamoDB APIs *)

(* TODO: check attribute names *)
let describe_table config =
  let body =
    `Assoc [("TableName", `String config.dynamo_table_name)] |>
    Yojson.to_string
  in
  let headers = make_header config body "DescribeTable"
  in
  Client.post
    ~headers
    ~body:(Cohttp_lwt.Body.of_string body)
    (Uri.of_string (endpoint_of config)) >>= fun (resp, body) ->
  Lwt.return (resp, body)

let get_item config key =
  let body = `Assoc [
      "TableName", `String config.dynamo_table_name;
      "Key", `Assoc [
        config.dynamo_key_attribute_name, `Assoc [
          "S", `String key
        ]
      ];
    ] |> Yojson.to_string
  in
  let headers = make_header config body "GetItem"
  in
  Client.post
    ~headers
    ~body:(Cohttp_lwt.Body.of_string body)
    (Uri.of_string (endpoint_of config)) >>= fun (resp, body) ->
  Lwt.return (resp, body)

let put_item config key value =
  let body = `Assoc [
      "TableName", `String config.dynamo_table_name;
      "Item", `Assoc [
        config.dynamo_key_attribute_name,
        `Assoc ["S", `String key];
        config.dynamo_object_attribute_name,
        `Assoc [stringify_object_type config.dynamo_object_type,
                `String value]
      ];
      "ReturnValues", `String "ALL_OLD"
    ] |> Yojson.to_string
  in
   let headers = make_header config body "PutItem"
  in
  Client.post
    ~headers
    ~body:(Cohttp_lwt.Body.of_string body)
    (Uri.of_string (endpoint_of config)) >>= fun (resp, body) ->
  Lwt.return (resp, body)

let delete_item config key =
  let body = `Assoc [
      "TableName", `String config.dynamo_table_name;
      "Key", `Assoc [
        config.dynamo_key_attribute_name,
        `Assoc ["S", `String key]
      ]
    ] |> Yojson.to_string
  in
  let headers = make_header config body "DeleteItem"
  in
  Client.post
    ~headers
    ~body:(Cohttp_lwt.Body.of_string body)
    (Uri.of_string (endpoint_of config)) >>= fun (resp, body) ->
  Lwt.return (resp, body)

module DynamoBackend :
  (StorageBackend with type config = dynamo_config
                   and type store  = dynamo_config) = struct
  type store = dynamo_config
  type config = dynamo_config
  
  (* Call describe_table *)
  let open_store config =
    describe_table config >>=
    fun (resp, _) ->
    match Response.status resp with
    | `OK -> Err.pure config
    | _ -> Lwt.return (Error "Abnormal HTTP response code")
  
  let has_object ~store ~key =
    get_item store key >>=
    fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string >>=
    fun res ->
    match res with
    | `Assoc res ->
      (match List.assoc_opt "Item" res with
       | None | Some (`Assoc []) -> Err.pure false
       | Some (`Assoc _) -> Err.pure true
       | Some _ -> Lwt.return (Error "Anomaly: Garbage return value from GetItem query!"))
    | _ -> Lwt.return (Error "Abnormal return value from GetItem query!")
             
  let get_object_opt ~store ~key =
    get_item store key >>=
    fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string >>=
    fun res ->
    match res with
    | `Assoc res ->
      (match List.assoc_opt "Item" res with
       | None | Some (`Assoc []) -> Err.pure None
       | Some (`Assoc a) ->
         (match List.assoc_opt store.dynamo_object_attribute_name a with
          | None -> Err.pure None
          | Some (`Assoc ((_, `String obj_val) :: _)) -> Err.pure (Some obj_val)
          | Some _ -> Lwt.return (Error "error"))              
       | Some _ -> Lwt.return (Error "error"))
    | _ -> Lwt.return (Error "error")
             
  let get_object_err ~store ~key =
     get_item store key >>=
    fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string >>=
    fun res ->
    match res with
    | `Assoc res ->
       (match List.assoc_opt "Item" res with
       | None | Some (`Assoc []) -> Lwt.return (Error "not found")
       | Some (`Assoc a) ->
         (match List.assoc_opt store.dynamo_object_attribute_name a with
          | None ->  Lwt.return (Error "not found")
          | Some (`Assoc ((_, `String obj_val) :: _)) -> Err.pure obj_val
          | Some _ -> Lwt.return (Error "error"))              
       | Some _ -> Lwt.return (Error "error"))
    | _ -> Lwt.return (Error "error")

  let put_object ~store ~key value =
    put_item store key value >>=
    fun (resp, _) ->
    match Response.status resp with
    | `OK -> Err.pure ()
    | _ -> Lwt.return (Error "error")

  let put_object_return_old ~store ~key value =
    put_item store key value >>=
    fun (resp, body) ->
    match Response.status resp with
    | `OK ->
      body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string >>=
      fun result ->
      (match result with
       | `Assoc result ->
         (match List.assoc_opt "Attributes" result with
          | None | Some (`Assoc []) -> Err.pure None
          | Some (`Assoc a) ->
            (match List.assoc_opt store.dynamo_object_attribute_name a with
             | None -> Lwt.return (Error "not found")
             | Some (`Assoc [_, `String old_val]) -> Err.pure (Some old_val)
             | Some _ -> Lwt.return (Error "impossible!"))
          | Some _ -> Lwt.return (Error "impossible!"))
       | _ -> Lwt.return (Error "impossible!"))
    | _ -> Lwt.return (Error "unknown error")

  let delete_object ~store ~key =
    delete_item store key >>=
    fun (resp, _) ->
    match Response.status resp with
    | `OK -> Err.pure ()
    | _ -> Lwt.return (Error "error")
end

type dynamo_store = [`Store]

type dynamo_backend =
  (module Base.StorageBackend
     with type config = dynamo_config
      and type store = dynamo_store)

(* let make_dynamo_backend = [%noimplval] *)
