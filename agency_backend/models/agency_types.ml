open Sexplib.Std

type sprthash = string [@@deriving sexp, yojson]
type tmplhash = string [@@deriving sexp, yojson]
type bookhash = string [@@deriving sexp, yojson]
type providerident = string [@@deriving sexp, yojson]
type hexbytes = string [@@deriving sexp, yojson]
type tzaddr = string [@@deriving sexp, yojson]
type rclabel = string [@@deriving sexp, yojson]

type mutez = Int64.t [@@deriving yojson]
let sexp_of_mutez x =
  [%sexp_of: string list]
    ["mutez"; Int64.to_string x]
let mutez_of_sexp sexp =
  match [%of_sexp: string list] sexp with
  | ["mutez"; x] -> Int64.of_string x
  | _ -> invalid_arg Format.(
      asprintf "unparsable as mutez: %a"
        Sexplib.Sexp.pp_hum sexp)

type hyperlink = {
    title : string;
    url : string;
    synopsis : string option;
  }
                   [@@deriving sexp, yojson]

type tezos_network = {
    netident : string; (** e.g. "carthagenet" *)
    chain_id : string; (** e.g. "NetXyJVJ3mkBox6" *)
  }
                       [@@deriving sexp, yojson]

type advertized_book = {
    bookident : string;
    bookhash : string;
    title : string;
    synopsis : string;
  }
                         [@@deriving sexp, yojson]

type book_charges = {
    agency_charge : mutez;
    provider_charge : mutez;
  }
                      [@@deriving sexp, yojson]

type book_review_results = {
    contract_complexity : string;
    certification_status : string;
  }
                             [@@deriving sexp, yojson]

type provider_info = {
    providerident : string;
    display_name : string;
    introduction : string;
    website : string;
    contact : string;
  }
                       [@@deriving sexp, yojson]

type book_status = {
    bookident : string;
    bookhash : string;
    charges : book_charges;
    review_results : book_review_results;
  }
                     [@@deriving sexp, yojson]

type book_entry = {
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
                    [@@deriving sexp, yojson]

type spirit_info = {
    sprthash : string;
    tmplhash : string;
    network : tezos_network;
    broker : tzaddr;
    requester : tzaddr;
    ensemble : (string*tzaddr) list;
  }
                     [@@deriving sexp, yojson]

module Sexp_enc = struct
  let list (xs,sx) = sexp_of_list xs, list_of_sexp sx
  let tuple (xs1,sx1) (xs2,sx2) =
    let lenc, ldec = list (identity, identity) in
    (fun (o1, o2) -> lenc [xs1 o1; xs2 o2]),
    ((function
      | [s1; s2] -> (sx1 s1, sx2 s2)
      | ss -> invalid_arg Format.(
          asprintf "unparsable as tuple: %a"
            Sexplib.Sexp.pp_hum (lenc ss)) )
     % ldec)

  let mutez = sexp_of_mutez, mutez_of_sexp
  let hyperlink = sexp_of_hyperlink, hyperlink_of_sexp
  let string = sexp_of_string, string_of_sexp
  let tezos_network =
    sexp_of_tezos_network,
    tezos_network_of_sexp
  let advertized_book =
    sexp_of_advertized_book,
    advertized_book_of_sexp
  let provider_info =
    sexp_of_provider_info,
    provider_info_of_sexp
  let book_status =
    sexp_of_book_status,
    book_status_of_sexp
  let book_entry =
    sexp_of_book_entry,
    book_entry_of_sexp
  let spirit_info =
    sexp_of_spirit_info,
    spirit_info_of_sexp
  let string_list = list string
end
