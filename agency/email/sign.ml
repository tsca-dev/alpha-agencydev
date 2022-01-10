let of_option_exn = function
  | Some v -> v
  | None -> failwith "Expected Some v, got None."

let encode_query ps =
  (* NOTE(dbp 2015-03-13): We want just:
     A-Z, a-z, 0-9, hyphen ( - ), underscore ( _ ), period ( . ), and tilde ( ~ ).
            As per the docs:
            http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
            Uri has that as it's fall-through, which at least currently (and hopefully forever)
            ~component:`Authority causes it to use.
  *)
  let encoded =
    List.map
      (fun (k, v) ->
         let key = Uri.pct_encode ~component:`Authority k in
         let value =
           match v with
           | [] -> ""
           | [ x ] -> Uri.pct_encode ~component:`Authority x
           | _ -> failwith "AWS query cannot have multiple values for same key"
         in
         key, value)
      ps
  in
  let sorted = List.sort (fun a b -> compare (fst a) (fst b)) encoded in
  let joined = List.map (fun (k, v) -> k ^ "=" ^ v) sorted in
  String.concat "&" joined

module Hash = struct
  let _sha256 ?key str =
      match key with
        | Some key -> Digestif.SHA256.hmac_string ~key str
        | None -> Digestif.SHA256.digest_string str
                    
  let sha256 ?key str = _sha256 ?key str |> Digestif.SHA256.to_raw_string
                                              
  let sha256_hex ?key str = _sha256 ?key str |> Digestif.SHA256.to_hex
end

module Time = struct
  module C = CalendarLib.Calendar
  module P = CalendarLib.Printer.Calendar

  let date_yymmdd = P.sprint "%Y%m%d"

  let date_time = P.sprint "%Y%m%dT%H%M%SZ"

  let now_utc () = C.(now () |> to_gmt)

  let parse s = P.from_fstring "%Y-%m-%dT%T" (String.sub s 0 (String.length s - 5))

  let format t = P.sprint "%Y-%m-%dT%T.000Z" t
end

module Request = struct
  type meth =
    [ `DELETE
    | `GET
    | `HEAD
    | `OPTIONS
    | `CONNECT
    | `TRACE
    | `Other of string
    | `PATCH
    | `POST
    | `PUT
    ]

  let string_of_meth = function
    | `DELETE -> "DELETE"
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `OPTIONS -> "OPTIONS"
    | `CONNECT -> "CONNECT"
    | `TRACE -> "TRACE"
    | `Other s -> s
    | `PATCH -> "PATCH"
    | `POST -> "POST"
    | `PUT -> "PUT"

  type headers = (string * string) list

  type t = meth * Uri.t * headers
end

let sign_request ~access_key ~secret_key ~service ~region ~host ~body (meth, uri, headers) =
    let params = encode_query (Uri.query uri) in
    let sign key msg = Hash.sha256 ~key msg in
    let get_signature_key key date region service =
      sign (sign (sign (sign ("AWS4" ^ key) date) region) service) "aws4_request"
    in
    let now = Time.now_utc () in
    let amzdate = Time.date_time now in
    let datestamp = Time.date_yymmdd now in
    let canonical_uri = "/v2/email/outbound-emails" in
    let canonical_querystring = params in
    let payload_hash = Hash.sha256_hex body in
    let canonical_headers =
      "host:"
      ^ host
      ^ "\n"
      ^ "x-amz-content-sha256:"
      ^ payload_hash
      ^ "\nx-amz-date:"
      ^ amzdate
      ^ "\n"
    in
    let signed_headers = "host;x-amz-content-sha256;x-amz-date" in
    let canonical_request =
      Request.string_of_meth meth
      ^ "\n"
      ^ canonical_uri
      ^ "\n"
      ^ canonical_querystring
      ^ "\n"
      ^ canonical_headers
      ^ "\n"
      ^ signed_headers
      ^ "\n"
      ^ payload_hash
    in
    let algorithm = "AWS4-HMAC-SHA256" in
    let credential_scope =
      datestamp ^ "/" ^ region ^ "/" ^ service ^ "/" ^ "aws4_request"
    in
    let string_to_sign =
      algorithm
      ^ "\n"
      ^ amzdate
      ^ "\n"
      ^ credential_scope
      ^ "\n"
      ^ Hash.sha256_hex canonical_request
    in
    let signing_key = get_signature_key secret_key datestamp region service in
    let signature = Hash.sha256_hex ~key:signing_key string_to_sign in
    let authorization_header =
      String.concat
        ""
        [ algorithm
        ; " "
        ; "Credential="
        ; access_key
        ; "/"
        ; credential_scope
        ; ", "
        ; "SignedHeaders="
        ; signed_headers
        ; ", "
        ; "Signature="
        ; signature
        ]
    in
    let headers =
      ("x-amz-date", amzdate)
      :: ("x-amz-content-sha256", payload_hash)
      :: ("Authorization", authorization_header)
      :: headers
    in
    meth, uri, headers
