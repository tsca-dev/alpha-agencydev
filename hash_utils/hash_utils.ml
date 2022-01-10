open Tezos_crypto

module Arbitrarity = struct

  let sprthash_rawprefix_testnet = "\007\024\174\234\138"
  let sprthash_rawprefix_mainnet = "\007\024\174\236\106"
  let sprthash_rawlen = 12, 7
  let sprthash_prefix_testnet = "sprt1"
  let sprthash_prefix_mainnet = "sprtA"

  let tmplhash_rawprefix = "\128\170\157\004\236"
  let tmplhash_rawlen = 21, 16
  let tmplhash_prefix = "tmpL1Q"

  let tmplversion_rawprefix = "\086\224\136\154"
  let tmplversion_rawlen = 24, 20
  let tmplversion_prefix = "tpv1G"
end

external str_set : string -> int -> char -> unit = "%string_safe_set"

let make_table prefix varlen totlen ppf =
  let open Tezos_crypto in
  let plen = String.length prefix in
  let buf = String.(prefix^(make varlen 'x')^(make (totlen-varlen-plen) '0')) in
  let set vi c = str_set buf (vi+plen) (char_of_int c) in
  let write() =
    let encoded = Base58.safe_encode buf in
    Format.printf "%s < %s@."
      encoded (String.escaped buf) in
  try
    match varlen with
    | 0 -> write()
    | 1 ->
       for v1 = 0 to 255 do
         set 0 v1;
         write()
       done
    | 2 ->
       for v1 = 0 to 255 do
         for v2 = 0 to 255 do
           set 0 v1; set 1 v2;
           write()
         done; done
    | _ -> Stdlib.failwith "noimpl"
  with exn ->
    Format.pp_print_flush ppf ();
    raise exn

let make_sample prefix totlen =
  let open Tezos_crypto in
  let plen = String.length prefix in
  let sample c =
    let buf = String.(prefix^(make (totlen-plen) c)) in
    let encoded = Base58.safe_encode buf in
    Format.printf "%s < %s@."
      encoded (String.escaped buf) in
  sample '0';
  sample '\000';
  sample '\255'

let sha256 body =
  body |> Bigstring.of_bytes
  |> Hacl.Hash.SHA256.digest
  |> Bigstring.to_string

let () =
  let open Kxclib.ArgOptions in
  if has_flag "-sample-rawprefix" then (
    let prefix = get_option_exn (StringOption "-rawprefix")
      |> Scanf.unescaped in
    let totlen = get_option_exn (IntOption "-totlen") in
    make_sample prefix totlen
  ) else if has_flag "-table-rawprefix" then (
    let prefix = get_option_exn (StringOption "-rawprefix")
      |> Scanf.unescaped in
    let totlen = get_option_exn (IntOption "-totlen") in
    make_table prefix 2 totlen (Format.std_formatter)
  ) else if has_flag "-calc-tmplversion" then (
    let idhex = `Hex (get_option_exn (StringOption "-idhex")) in
    let rawident = Hex.to_string idhex in
    let prefix, (_, exlen) = Arbitrarity.(
        tmplversion_rawprefix, tmplversion_rawlen) in
    (if String.length rawident <> exlen
     then invalid_arg "rawident length mismatch");
    let hash = prefix ^ rawident in
    Base58.safe_encode hash |> print_endline
  ) else if has_flag "-calc-tmplhash" then (
    let prefix, (_, exlen) = Arbitrarity.(
        tmplhash_rawprefix, tmplhash_rawlen) in
    if has_flag "-idhex" then (
      let idhex = `Hex (get_option_exn (StringOption "-idhex")) |> Hex.to_string in
      (if String.length idhex <> (256/8)
       then invalid_arg "idhex length mismatch");
      let rawident = String.sub idhex 0 exlen in
      let hash = prefix ^ rawident in
      Base58.safe_encode hash |> print_endline
    ) else if has_flag "-bodyhex" then (
      let body = `Hex (get_option_exn (StringOption "-bodyhex"))
                 |> Hex.to_bytes in
      let bodyhash = sha256 body in
      let rawident = String.sub bodyhash 0 exlen in
      let hash = prefix ^ rawident in
      Base58.safe_encode hash |> print_endline
    )
  ) else if has_flag "-calc-sprthash" then (
    let prefix, (_, exlen) = Arbitrarity.(
        (if has_flag "-testnet" then
           sprthash_rawprefix_testnet
         else if has_flag "-mainnet" then
           sprthash_rawprefix_mainnet
         else invalid_arg "you must specify -testnet|mainnet"),
        sprthash_rawlen) in
    let bodyhash =
      if has_flag "-idhex" then (
        let idhex = `Hex (get_option_exn (StringOption "-idhex")) |> Hex.to_string in
        (if String.length idhex <> (256/8)
         then invalid_arg "idhex length mismatch");
        idhex
      ) else if has_flag "-idbody" then (
        let body = get_option_exn (StringOption "-idbody")
                   |> Scanf.unescaped
                   |> Bytes.of_string in
        let bodyhash = sha256 body in
        bodyhash
      ) else if has_flag "-readbody" then (
        let body = slurp_stdin()
                   |> Bytes.of_string in
        let bodyhash = sha256 body in
        bodyhash
      ) else invalid_arg "-calc-sprthash: illegal arguments" in
    Format.eprintf "sha256sum = %s@."
      (Hex.of_string bodyhash |> Hex.show);
    let rawident = String.sub bodyhash 0 exlen in
    let hash = prefix ^ rawident in
    Base58.safe_encode hash |> print_endline
  ) else (
    print_endline "illegal arguments";
    exit 2
  )
