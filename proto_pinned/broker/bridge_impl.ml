open Bridgelib.Bridge_commands
open Brokerlib

module Impl = struct
  module Conv = DataModelConv

  let ok x = Result.ok x
  let fail x = Result.error x

  let datatype_of_michelson
      : type t. t data_type*string -> t ResultWithErrmsg.t =
    function
    | Broker_store_data, m ->
       Brokerlib.store'_bridge#revert m
       |> Option.get (* XXX bad practice *)
       |> Conv.to_broker_store_data
       |> ok
    | Template_descriptor_data, m ->
       Brokerlib.template_descriptor'_bridge#revert m
       |> Option.get (* XXX bad practice *)
       |> Conv.to_template_descriptor_data
       |> ok
    | Spirit_descriptor_data, m ->
       Brokerlib.spirit_descriptor'_bridge#revert m
       |> Option.get (* XXX bad practice *)
       |> Conv.to_spirit_descriptor_data
       |> ok
    | Broker_message_data, _m ->
       (* we don't need this yet, thus leave unimpl for now *)
       fail "noimpl: datatype_of_michelson(Broker_message_data)"

  let michelson_of_datatype
      : type t. t data_type*t -> string =
    function
    | Broker_message_data, x ->
       Conv.of_broker_message_data x
       |> Brokerlib.message_bridge#convert
    | Broker_store_data, _x ->
       (* we don't need this yet, thus leave unimpl for now *)
       failwith "noimpl: michelson_of_datatype(Broker_store_data)"
    | Template_descriptor_data, _x ->
       (* we don't need this yet, thus leave unimpl for now *)
       failwith "noimpl: michelson_of_datatype(Template_descriptor_data)"
    | Spirit_descriptor_data, _x ->
       (* we don't need this yet, thus leave unimpl for now *)
       failwith "noimpl: michelson_of_datatype(Spirit_descriptor_data)"

  let parse_wrapper_store : string -> (bytes*string) ResultWithErrmsg.t =
    fun str ->
    try
      let wstore = (parse_wrapper_store str)#wstore in
      (wstore,
       Tzutils.Michelson.Handy.unpack_data
         wstore)
      |> Result.ok
    with e ->
      Result.error (sprintf "%a" pp_exn e)
end

module BrokerlibBridgeResponder =
  MakeResponder(Impl)
