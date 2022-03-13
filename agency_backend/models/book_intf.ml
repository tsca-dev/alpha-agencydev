module TmplversionTypes = struct
  type mutez = int64 [@@deriving yojson]
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

    val pack_data : ?typ:michelson_type -> michelson_const -> bytes Lwt.t
    val unpack_data : ?typ:michelson_type -> bytes -> michelson_const Lwt.t

    val now : unit -> int64 Lwt.t

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

      val to_key_hash : t -> string
      val of_key_hash : string -> t

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

  open Sexplib.Std
  type atom_field_type = [
      `line of [ `proportional | `monospaced ] (* a single line text *)
    | `text of [ `proportional | `monospaced ] (* a multiple-line text *)
    | `date (* a date picker *)
    | `toggle (* a checkbox field *)
    | `timestamp (* a timestamp picker *)
    | `amount (* a amount field in tez *)
    | `tzaddr (* a tezos address field *)
    | `integer of string option (* an integer field with optional unit_desc *)
    ] [@@deriving sexp, yojson]
  type field_type = [
      `atom of atom_field_type
    | `list of atom_field_type*(* delim *)string
    ] [@@deriving sexp, yojson]

  (** field descirptor of an spell assistant *)
  type field_desc = {
      name : string;
      desc : string;
      typ : field_type;
      mandated : bool;

      placeholder : string option;
      doc : string option;
      requirement_desc : string option;
    } [@@deriving sexp, yojson]

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
        string aresult Lwt.t
    }

  type tmplversion = {
      tmplhash : string;
      tmplversion : string;

      genspell_interpreter :
        booksdk ->
        spell:string
        -> (bytes (* genparam *) * mutez (* total_init_balance *))
           aresult Lwt.t;
      invspell_interpreter :
        booksdk ->
        spell:string
        -> (string (* rclabel *) * bytes (* wparam *) * mutez (* txn_amount *))
           aresult Lwt.t;
      spell_assistants : spell_assistant list;
      spirit_interpreters : spirit_interpreter list;
    }
end

module BookTypes = struct
  type mutez = Agency_types.mutez
  type hyperlink = Agency_types.hyperlink = {
      title : string;
      url : string;
      synopsis : string option;
    }

  type provider = Agency_types.provider_info = {
      providerident : string;
      display_name : string;
      introduction : string;
      website : string;
      contact : string;
    }

  type book_entry = Agency_types.book_entry = {
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
