module TmplversionTypes = struct
  type mutez = int64
  type spirit_status = {
      sprthash : string;
      tmplhash : string;
      broker : string;
      requester : string;
      avatars : avatar_status list;
    }
  and avatar_status = {
      address : string;
      rclabel : string;
      balance : mutez;
      wstore : bytes;
      wstore_unpacked : string option;
    }
  type 'x aresult = ('x, string) result
  type json = [
        `Assoc of (string * json) list
      | `Bool of bool
      | `Float of float
      | `Int of int
      | `List of json list
      | `Null
      | `String of string ]

  module type TemplateVersionSdk = sig
    val parse_json : string -> json aresult
    val unparse_json : json -> string

    type michelson_expr
    type michelson_type
    type michelson_const

    val parse_expr : string -> michelson_expr
    val parse_type : string -> michelson_type
    val parse_data : string -> michelson_const
    val unparse_expr : michelson_expr -> string
    val unparse_type : michelson_type -> string
    val unparse_data : michelson_const -> string

    val pack_data : ?typ:michelson_type -> michelson_const -> bytes
    val unpack_data : ?typ:michelson_type -> bytes -> michelson_const

    module Michelson_data : sig
      type t = michelson_const

      (* type path = [ `Car | `Cdr | `Deopt | `Left | `Right ]
       * val access : path list -> t -> t *)

      (* timestamp represented as unix timestamp in sec resolution *)
      val to_timestamp : t -> int64 
      val of_timestamp : int64 -> t

      val to_code : t -> michelson_expr
      val of_code : michelson_expr -> t

      val to_address : t -> string
      val of_address : string -> t

      val to_mutez : t -> mutez
      val of_mutez : mutez -> t

      val to_unit : t -> unit
      val of_unit : unit -> t

      val to_bool : t -> bool
      val of_bool : bool -> t

      val to_int : t -> int32 option
      val of_int : int32 -> t
      val to_bigint : t -> string
      val of_bigint : string -> t

      val to_string : t -> string
      val of_string : string -> t

      val to_bytes : t -> bytes
      val of_bytes : bytes -> t

      val to_option : t -> (t option)
      val of_option : (t option) -> t

      val to_list : t -> (t list)
      val of_list : (t list) -> t

      val to_set : t -> (t list)
      val of_set : (t list) -> t

      val to_map : t -> ((t*t) list)
      val of_map : ((t*t) list) -> t

      val to_pair : t -> (t*t)
      val of_pair : (t*t) -> t

      val to_left : t -> t
      val to_right : t -> t
      val to_or : t -> [ `Left of t | `Right of t ]
      val of_or : [ `Left of t | `Right of t ] -> t
    end
  end

  type booksdk = (module TemplateVersionSdk)

  type atom_field_type = [
      `Line of [ `Proportional | `Monospaced ] (* a single line text *)
    | `Text of [ `Proportional | `Monospaced ] (* a multiple-line text *)
    | `Date (* a date picker *)
    | `Toggle (* a checkbox field *)
    | `Timestamp (* a timestamp picker *)
    | `Amount (* a amount field in tez *)
    | `Tzaddress (* a tezos address field *)
    | `Integer of string option (* an integer field with optional unit_desc *)
    ]
  type field_type = [
      `Atom of atom_field_type
    | `List of atom_field_type
    ]

  (** field descirptor of an spell assistant *)
  type field_desc = {
      name : string;
      desc : string;
      typ : field_type;
      mandated : bool;

      placeholder : string option;
      doc : string option;
      requirement_desc : string option;
    }

  type spell_assistant = {
      salabel : string;
      form_title : string;
      form_desc : string;
      form_major_button : string*string option; (* [title, desc] *)
      form_fields : field_desc list;
      form_interpreter :
        booksdk ->
        (string (* field_name *) * string (* user_input *)) list
        ->
        (string
         (* resulting spell when form valid *),

         string*(string*string) list
         (* or [message, (field_name |-> fix_hint)] when form invalid *)
        ) result
    }

  type spirit_interpreter = {
      silabel : string;
      interpreter :
        booksdk ->
        spirit_status ->
        string aresult
    }

  type tmplversion = {
      tmplhash : string;
      tmplversion : string;

      genspell_interpreter :
        booksdk ->
        spell:string
        -> (bytes (* genparam *) * mutez (* total_init_balance *))
           aresult;
      invspell_interpreter :
        booksdk ->
        spell:string
        -> (string (* rclabel *) * bytes (* wparam *) * mutez (* txn_amount *))
           aresult;
      spell_assistants : spell_assistant list;
      spirit_interpreters : spirit_interpreter list;
    }
end

module BookTypes = struct
  open Agency_apis.Aii_dir

  type mutez = AgencyTypes.mutez
  type hyperlink = AgencyTypes.hyperlink = {
      title : string;
      url : string;
      synopsis : string option;
    }

  type provider = AgencyTypes.provider_info = {
      providerident : string;
      display_name : string;
      introduction : string;
      website : string;
      contact : string;
    }

  type book_entry = AgencyTypes.book_entry = {
      bookident : string;
      bookhash : string;
      title : string;
      synopsis : string;
      tmplhash : string;
      provider : string; (* providerident *)
      contract_parameters_en :
        (string*string) list; (* [param, description] *)
      contract_terms_en :
        string list;
      contract_caveats_en :
        string list;
      specifications :
        hyperlink list;
    }

  type tmplversion = TmplversionTypes.tmplversion

  type t = {
      entry : book_entry;
      agency_charge : mutez; (* XXX - from index *)
      provider_charge : mutez; (* XXX - from index *)
      contract_complexity : string;
      certification_status : string;
    }
end

module OcamlBooksdk = struct
  open TmplversionTypes
  let parse_json : string -> json aresult =
    fun str ->
    try Yojson.Basic.from_string str |> Result.ok
    with Yojson.Json_error msg -> Result.error msg
  let unparse_json : json -> string = Yojson.Basic.to_string

  open Tzutils.Micheline
  open Tzutils

  module Protocol = Tezos_protocol_007_PsDELPH1.Protocol
  module MParser = Tezos_client_007_PsDELPH1.Michelson_v1_parser
  module MPrinter = Tezos_client_007_PsDELPH1.Michelson_v1_printer
  module Prims = Protocol.Michelson_v1_primitives
  type prim = Prims.prim
  type michelson_expr = MParser.parsed
  type michelson_type = MParser.parsed
  type michelson_const = (canonical_location, prim) node

  let parse_expr : string -> michelson_expr =
    fun str -> Michelson.parse_expr str |> strip_tzresult_lwt
  let parse_type : string -> michelson_type = parse_expr
  let parse_data : string -> michelson_const =
    fun str ->
    let parsed = Michelson.parse_expr str |> strip_tzresult_lwt in
    root (parsed.expanded)

  let unparse_expr : michelson_expr -> string = fun e -> e.source
  let unparse_type : michelson_expr -> string = unparse_expr
  let unparse_data : michelson_const -> string =
    fun c -> Michelson.string_of_expr (strip_locations c)

  let pack_data ?typ data =
    Michelson.pack_data' ?typ (strip_locations data)
    |> strip_tzresult_lwt
  let unpack_data ?typ bytes =
    let unpacked = Michelson.unpack_data ?typ bytes |> strip_tzresult_lwt in
    root unpacked

  let invalid node fmt =
    Format.kasprintf (fun str ->
        let msg =
          Format.asprintf "%s : %a" str
            MPrinter.print_expr
            (strip_locations node) in
        invalid_arg msg
      ) fmt

  module Michelson_data = struct
    type t = michelson_const

    let to_timestamp : t -> int64 = function
      | Int (_, z) ->
         Z.to_int64 z
      | String (_, str) ->
         Ptime.of_rfc3339 str |> Result.get_ok
         |> fun (x,_,_) -> Ptime.(x |> to_float_s |> Int64.of_float)
      | n -> invalid n "to_timestamp"
    let of_timestamp : int64 -> t =
      fun z -> Int (0, Z.of_int64 z)

    let to_code : t -> michelson_expr = function
      | Seq _ as code ->
         unparse_data code |> parse_expr
      | n -> invalid n "to_code"
    let of_code : michelson_expr -> t = fun e ->
      match root (e.expanded) with
      | Seq _ as code -> code
      | n -> invalid n "of_code"

    let to_address = function
      | String (_, str) -> str
      | Bytes (_, bytes) ->
         Protocol.Contract_repr.(
          Data_encoding.Binary.of_bytes encoding
            bytes |> Result.get_ok
          |> to_b58check)
      | n -> invalid n "to_address"
    let of_address str = String (0, str)

    let to_string = function
      | String (_, str) -> str
      | n -> invalid n "to_string"
    let of_string str = String (0, str)

    let to_mutez = function
      | Int (_, z) -> Z.to_int64 z
      | n -> invalid n "to_mutez"
    let of_mutez i = Int (0, Z.of_int64 i)

    open Prims

    let to_bool = function
      | Prim (_, D_True, _, _) -> true
      | Prim (_, D_False, _, _) -> false
      | n -> invalid n "to_bool"
    let of_bool b =
      let p = if b then D_True else D_False in
      Prim (0, p, [], [])

    let to_unit = function
      | Prim (_, D_Unit, _, _) -> ()
      | n -> invalid n "to_unit"
    let of_unit() = Prim (0, D_Unit, [], [])

    let to_int = function
      | Int (_, z) ->
         (try Some (Z.to_int32 z) with Z.Overflow -> None)
      | n  -> invalid n "to_int"
    let of_int i = Int (0, Z.of_int32 i)
    let to_bigint = function
      | Int (_, z) -> (Z.to_string z)
      | n  -> invalid n "to_bigint"
    let of_bigint str = Int (0, Z.of_string str)

    let to_bytes = function
      | String (_, str) ->
         Commons.bytes_of_prefixed_hex str
      | Bytes (_, b) -> b
      | n -> invalid n "to_bytes"
    let of_bytes b = Bytes (0, b)

    let to_option = function
      | Prim (_, D_None, _, _) -> None
      | Prim (_, D_Some, [t], _) -> Some t
      | n -> invalid n "to_option"
    let of_option = function
      | None -> Prim (0, D_None, [], [])
      | Some t -> Prim (0, D_Some, [t], [])

    let to_list = function
      | Seq (_, ts) -> ts
      | n -> invalid n "to_list"
    let of_list ts = Seq (0, ts)

    let to_set = to_list
    let of_set = of_list

    let to_map = function
      | Seq (_, ts) ->
         let elt = function
           | Prim (_, D_Elt, [k; v], _) -> k,v
           | n -> invalid n "to_map" in
         ts |&> elt
      | n -> invalid n "to_list"
    let of_map ts =
      let elt (k,v) = Prim (0, D_Elt, [k; v], []) in
      Seq (0, ts |&> elt)

    let to_pair = function
      | Prim (_, D_Pair, [l; r], _) -> l, r
      | n -> invalid n "to_pair"
    let of_pair (l,r) = Prim (0, D_Pair, [l; r], [])

    let to_left = function
      | Prim (_, D_Left, [t], _) -> t
      | n -> invalid n "to_left"
    let to_right = function
      | Prim (_, D_Right, [t], _) -> t
      | n -> invalid n "to_left"
    let to_or = function
      | Prim (_, D_Left, [t], _) -> `Left t
      | Prim (_, D_Right, [t], _) -> `Right t
      | n -> invalid n "to_or"
    let of_or = function
      | `Left t -> Prim (0, D_Left, [t], [])
      | `Right t -> Prim (0, D_Right, [t], [])
  end
end

let ocaml_booksdk :TmplversionTypes.booksdk  = (module OcamlBooksdk)
