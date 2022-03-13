open Sexplib.Std
open Broker_models

module Sexp = Sexplib.Sexp

type _ data_type =
  | Broker_store_data
    : BrokerDataModels.broker_store_data data_type
  | Broker_message_data
    : BrokerDataModels.broker_message_data data_type
  | Template_descriptor_data
    : BrokerDataModels.template_descriptor_data data_type
  | Spirit_descriptor_data
    : BrokerDataModels.spirit_descriptor_data data_type
type type_tag = [
  | `Broker_store_data
  | `Broker_message_data
  | `Template_descriptor_data
  | `Spirit_descriptor_data
  ] [@@deriving sexp]

type request =
  | Req_to_michelson : 't data_type*'t -> request
  | Req_of_michelson : 't data_type*string -> request

type response =
  | Res_to_michelson : 't data_type*string -> response
  | Res_of_michelson : 't data_type*'t -> response

module SexpCodec = struct
  type sexp = Sexplib.Sexp.t
  open BrokerDataModels

  let sexp_of_sexp = identity
  let sexp_of_bytes b = Sexplib.Std.sexp_of_string (Bytes.to_string b)
  let bytes_of_sexp s = Sexplib.Std.string_of_sexp s |> Bytes.of_string

  let encode : type t. (t data_type*t) -> sexp = function
    | Broker_store_data, x ->
       sexp_of_broker_store_data x
    | Broker_message_data, x ->
       sexp_of_broker_message_data x
    | Template_descriptor_data, x ->
       sexp_of_template_descriptor_data x
    | Spirit_descriptor_data, x ->
       sexp_of_spirit_descriptor_data x

  let decode : type t. (t data_type*sexp) -> t = function
    | Broker_store_data, s ->
       broker_store_data_of_sexp s
    | Broker_message_data, s ->
       broker_message_data_of_sexp s
    | Template_descriptor_data, s ->
       template_descriptor_data_of_sexp s
    | Spirit_descriptor_data, s ->
       spirit_descriptor_data_of_sexp s

  type request_repr = [
    | `echo of string
    | `to_michelson of type_tag*(* value *) sexp
    | `of_michelson of type_tag*(* value *) string
    | `parse_wrapper_store of string
    ] [@@deriving sexp]

  type response_repr = [
    | `echo_back of string
    | `to_michelson of type_tag*(* value *) string
    | `of_michelson of type_tag*(* value *) sexp
    | `error of string
    | `parse_wrapper_store of bytes*string
    ] [@@deriving sexp]

  let to_type_tag : type t. t data_type -> type_tag =
    function
    | Broker_store_data -> `Broker_store_data
    | Broker_message_data -> `Broker_message_data
    | Template_descriptor_data -> `Template_descriptor_data
    | Spirit_descriptor_data -> `Spirit_descriptor_data

  (* Caution : MAGIC APPLIED - USE WITH CAREs *)
  let of_type_tag : type t. type_tag -> t data_type =
    function
    | `Broker_store_data -> Broker_store_data |> Obj.magic
    | `Broker_message_data -> Broker_message_data |> Obj.magic
    | `Template_descriptor_data -> Template_descriptor_data |> Obj.magic
    | `Spirit_descriptor_data -> Spirit_descriptor_data |> Obj.magic

end

module Requester = struct
  open SexpCodec
  type 'x maybe = ('x, exn) result

  let to_michelson :
        type t. (t data_type*t)
        -> (* cmd *)string*
           ((* resp *)string -> (* result *)string maybe) =
    fun (t, x) ->
    let tag = to_type_tag t in
    let body = encode (t, x) in
    let panic() = failwith "panic @c:to_michelson" in
    begin
      (`to_michelson (to_type_tag t, body))
      |> (sexp_of_request_repr &> Sexp.to_string)
    end,
    begin fun raw ->
    try raw |> Sexp.of_string |> response_repr_of_sexp
        |> function
          | `to_michelson (tag', resp) when tag' = tag ->
             resp |> Result.ok
          | _ -> panic()
    with e -> Result.error e
    end

  let of_michelson :
        type t. (t data_type*string)
        -> string*
           ((* resp *)string -> (* result *)t maybe) =
    fun (t, body) ->
    let tag = to_type_tag t in
    let panic() = failwith "panic @c:of_michelson" in
    begin
    (`of_michelson (to_type_tag t, body))
    |> (sexp_of_request_repr &> Sexp.to_string)
    end,
    begin fun raw ->
    try raw |> Sexp.of_string |> response_repr_of_sexp
        |> function
          | `of_michelson (tag', resp) when tag' = tag ->
             decode (t, resp) |> Result.ok
          | _ -> panic()
    with e -> Result.error e
    end

  let parse_wrapper_store :
        string
        -> (* cmd *)string*
           ((* resp *)string -> (* result *)(bytes*string) maybe) =
    fun input ->
    let panic() = failwith "panic @c:parse_wrapper_store" in
    begin
      (`parse_wrapper_store input)
      |> (sexp_of_request_repr &> Sexp.to_string)
    end,
    begin fun raw ->
    try raw |> Sexp.of_string |> response_repr_of_sexp
        |> function
          | `parse_wrapper_store resp ->
             resp |> Result.ok
          | _ -> panic()
    with e -> Result.error e
    end

end

module MakeResponder (Impl : sig
             val michelson_of_datatype : 't data_type*'t -> string
             val datatype_of_michelson : 't data_type*string -> 't ResultWithErrmsg.t
             val parse_wrapper_store : string -> (bytes*string) ResultWithErrmsg.t
           end) = struct
  open SexpCodec

  let pp_sexp = Sexplib.Sexp.pp_mach
  let pp_type_tag = pp_sexp %% sexp_of_type_tag

  let handler : ?err_logger:(string -> unit) -> string -> string =
    fun ?err_logger ->
    let err_logger = Option.v (constant ()) err_logger in
    fun cmd ->
    try cmd |> Sexp.of_string
        |> request_repr_of_sexp
        |> function
          | `echo body -> `echo_back body |> sexp_of_response_repr |> Sexp.to_string
          | `to_michelson (tag, body) ->
             let t = of_type_tag tag in
             let x = decode (t, body) in
             let resp = Impl.michelson_of_datatype (t, x) in
             `to_michelson (tag, resp)
             |> sexp_of_response_repr |> Sexp.to_string
          | `parse_wrapper_store input ->
             (match Impl.parse_wrapper_store input with
              | Ok resp ->
                 `parse_wrapper_store resp
                 |> sexp_of_response_repr |> Sexp.to_string
              | Error e ->
                 let msg = sprintf
                             "failed parse_wrapper_store: %s; for input : %s"
                             e input in
                 err_logger msg;
                 `error msg |> sexp_of_response_repr |> Sexp.to_string)
          | `of_michelson (tag, body) ->
             let t = of_type_tag tag in
             (match Impl.datatype_of_michelson (t, body) with
              | Ok resp ->
                 `of_michelson (tag, encode (t, resp))
                 |> sexp_of_response_repr |> Sexp.to_string
              | Error e ->
                 let msg = sprintf
                             "failed decode michelson for tag %a : %s; for input : %s"
                             pp_type_tag tag e body in
                 err_logger msg;
                 `error msg |> sexp_of_response_repr |> Sexp.to_string)
    with e ->
      let msg = sprintf "failed handling req : %a; for cmd : %s" pp_full_exn e cmd in
      err_logger msg;
      `error msg |> sexp_of_response_repr |> Sexp.to_string
end

module Client (IoStyle : Io_style.S) = struct
  module type S = sig
    type 'x io = 'x IoStyle.t
    val to_michelson : 't data_type -> 't -> string io
    val of_michelson : 't data_type -> string -> 't io
    val parse_wrapper_store : string -> (bytes * string) io
  end

  module Make (Comm : sig
               open IoStyle
               val exchange_message : string -> string t
             end)
         : S = struct
    open MonadOps(IoStyle)

    type 'x io = 'x IoStyle.t

    let to_michelson : type t. t data_type -> t -> string io =
      fun t x ->
      let cmd, interp = Requester.to_michelson (t, x) in
      Comm.exchange_message cmd
      >>= fun raw ->
      match interp raw with
      | Ok resp -> pure resp
      | Error e -> IoStyle.inject_error' e

    let of_michelson : type t. t data_type -> string -> t io =
      fun t s ->
      let cmd, interp = Requester.of_michelson (t, s) in
      Comm.exchange_message cmd
      >>= fun raw ->
      match interp raw with
      | Ok resp -> pure resp
      | Error e -> IoStyle.inject_error' e

  let parse_wrapper_store : string -> (bytes*string) io =
    fun input ->
    let cmd, interp = Requester.parse_wrapper_store input in
    Comm.exchange_message cmd
    >>= fun raw ->
    match interp raw with
    | Ok resp -> pure resp
    | Error e -> IoStyle.inject_error' e
  end
end
