open Sexplib.Std

type bil_info = {
    sprthash : string;
    network : Aii_dir.AgencyTypes.tezos_network;
    agency_base : string;
  }
[@@deriving sexp, yojson]

let json_of_bil_info info =
  let open Yojson.Safe in
  info |> bil_info_to_yojson
  |> to_string
