open Tezos_crypto

module Arbitrarity = struct
  type base58check_encoding_scheme = {
      bhes_dataprefix : string;
      (** prefix of the decoded data *)
      bhes_datalen : int;
      (** length of the decoded data *)
      bhes_rawlen : int; (* must be [bhes_datalen - String.length bhes_dataprefix] *)
      (** length of the un-prefixed decoded data *)
      bhes_encprefix : string;
      (** prefix of the encoded data *)
      bhes_enclen : int;
      (** length of the encoded data *)
    }

  let base58check_encoding_scheme
        ~rawlen:bhes_rawlen
        ~dataprefix:bhes_dataprefix
        ~datalen:bhes_datalen
        ~encprefix:bhes_encprefix
        ~enclen:bhes_enclen
    = { bhes_dataprefix;
        bhes_datalen;
        bhes_rawlen;
        bhes_encprefix;
        bhes_enclen; }

  let sprthash_testnet_scheme =
    base58check_encoding_scheme
      ~rawlen:7 ~dataprefix:"\007\024\174\234\138"
      ~datalen:12 ~encprefix:"sprt1" ~enclen:21

  let sprthash_mainnet_scheme =
    base58check_encoding_scheme
      ~rawlen:7 ~dataprefix:"\007\024\174\236\106"
      ~datalen:12 ~encprefix:"sprtA" ~enclen:21

  let tmplhash_scheme =
    base58check_encoding_scheme
      ~rawlen:16 ~dataprefix:"\128\170\157\004\236"
      ~datalen:21 ~encprefix:"tmpL1Q" ~enclen:34

  let tmplversion_scheme =
    base58check_encoding_scheme
      ~rawlen:20 ~dataprefix:"\086\224\136\154"
      ~datalen:24 ~encprefix:"tpv1G" ~enclen:38

  module Helpers = struct

    let calc_base58check_encoding_scheme
          ~dataprefix ~rawlen ~encprefix =
      let low, high = '\000', '\255' in
      let len = String.length in
      let datalen = rawlen + len dataprefix in
      let sample1 = dataprefix^(String.make rawlen low) in
      let sample2 = dataprefix^(String.make rawlen high) in
      let enc data = Base58.safe_encode data in
      let check_prefix data enc =
        if not (String.starts_with encprefix enc)
        then invalid_arg Format.(
          sprintf "bad encprefix : expected %s.. but sample (%s) encodes to %s"
            encprefix data enc) in
      let sample1enc, sample2enc = enc sample1, enc sample2 in
      check_prefix sample1 sample1enc;
      check_prefix sample2 sample2enc;
      {
        bhes_dataprefix = dataprefix;
        bhes_datalen = datalen;
        bhes_rawlen = rawlen;
        bhes_encprefix = encprefix;
        bhes_enclen = len sample1enc;
      }

    let gen_base58check_encoding_scheme
          ~dataprefix ~rawlen ~encprefix =
      let { bhes_dataprefix;
            bhes_datalen;
            bhes_rawlen;
            bhes_encprefix;
            bhes_enclen; } =
        calc_base58check_encoding_scheme
          ~dataprefix ~rawlen ~encprefix in
      Format.sprintf
        "base58check_encoding_scheme \
         ~rawlen:%d \
         ~dataprefix:\"%s\" \
         ~datalen:%d \
         ~encprefix:\"%s\" \
         ~enclen:%d"
        bhes_rawlen
        String.(to_seq bhes_dataprefix |> List.of_seq
                |&> int_of_char
                |&> Format.sprintf "\\%03d"
                |> String.concat "")
        bhes_datalen
        bhes_encprefix
        bhes_enclen

  end

end open Arbitrarity

let b58check_encode scheme raw =
  (if String.length raw <> scheme.bhes_rawlen
   then invalid_arg' "rawdata.len (%d) <> scheme.rawlen (%d)"
          (String.length raw) scheme.bhes_rawlen);
  let data = scheme.bhes_dataprefix^raw in
  Base58.safe_encode data

let b58check_decode scheme str =
  let open MonadOps(Option) in
  Base58.safe_decode str >>= fun data ->
  match String.chop_prefix scheme.bhes_dataprefix data with
  | Some raw when String.length data = scheme.bhes_datalen ->
     Some raw
  | _ -> None

let tmplhash_of_hashbytes ?check_length:(check_len=true) bytes =
  (* [bytes] should be an sha256 hash thus of 256-bits *)
  let bytes = Bytes.to_string bytes in
  (if check_len && String.length bytes <> (256/8)
   then invalid_arg "hash length mismatch");
  let scheme = tmplhash_scheme in
  let rawident = String.sub bytes 0 scheme.bhes_rawlen in
  b58check_encode scheme rawident

let calc_sprthash ~testnet identstr =
  let hash = Digestif.SHA256.(digest_string identstr |> to_raw_string) in
  let scheme =
    if testnet
    then sprthash_testnet_scheme
    else sprthash_mainnet_scheme in
  let raw = String.sub hash 0 scheme.bhes_rawlen in
  b58check_encode scheme raw |> String.lowercase_ascii
