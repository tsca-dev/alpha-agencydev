open Base
open Sign
open Lwt.Infix
open Cohttp_lwt_unix

type aws_region = [
  | `Tokyo
  | `NorthVirginia
  | `Oregon ]

type ses_config = {
    aws_access_key_id : string;
    aws_secret_access_key : string;
    aws_region : aws_region;
    aws_host : string option;
    aws_endpoint : string option;
}

let stringify_region = function
  | `Tokyo -> "ap-northeast-1"
  | `NorthVirginia -> "us-east-1"
  | `Oregon -> "us-west-2"

let host_of config =
  match config.aws_host with
  | None -> "email." ^ stringify_region config.aws_region ^ ".amazonaws.com"
  | Some host -> host

let endpoint_of config =
  match config.aws_endpoint with
  | None -> "https://" ^ (host_of config) ^ "/v2/email/outbound-emails"
  | Some endpoint -> endpoint

let sign_ses_header config header body =
  let host = host_of config in
  let (_, _, signed) = sign_request
    ~access_key:config.aws_access_key_id
    ~secret_key:config.aws_secret_access_key
    ~service:"ses"
    ~region:(stringify_region config.aws_region)
    ~host:(host_of config)
    ~body
    (`POST, Uri.of_string host, header)
  in signed

let make_header config body = sign_ses_header config [
    ("Accept-Encoding", "identity");
    ("Host", host_of config);
    ("Content-Type", "application/x-amz-json-1.0");
    ("User-Agent", "ocaml");
  ] body |> Cohttp.Header.of_list

let string_to_json s = `String s
type body_type = [ `Text | `Html ]

let send_email_ses config from recipients cc bcc subject body_type body =
  let body_type_str =
    match body_type with
    | `Text -> "Text"
    | `Html -> "Html"
  in
  let body =
    `Assoc [
      "FromEmailAddress", `String from;
      "Destination", `Assoc [
        "ToAddresses", `List (List.map string_to_json recipients);
        "CcAddresses", `List (List.map string_to_json cc);
        "BccAddresses", `List (List.map string_to_json bcc);
      ];
      "Content", `Assoc [
        "Simple", `Assoc [
          "Body", `Assoc [
            body_type_str, `Assoc [
              "Charset", `String "UTF-8";
              "Data", `String body
            ]
          ];
          "Subject", `Assoc [
            "Charset", `String "UTF-8";
            "Data", `String subject
          ]
        ];
      ]
    ] |> Yojson.to_string
  in
  let headers = make_header config body in
  Client.post
    ~headers
    ~body:(Cohttp_lwt.Body.of_string body)
    (Uri.of_string (endpoint_of config)) >>= fun (resp, body) ->
  Lwt.return (resp, body)

module SESV2Backend : (EmailBackend with type config = ses_config) = struct
  type config = ses_config

  (* WARNING: AWS recommends that you send only one email at a time. If you
   * send multiple emails at a time, the entire API call will fail even if only
   * one email failed to send! *)
  let send_email ~config ~from ~recipients ?cc:(cc = []) ?bcc:(bcc = [])
        ~subject ~body =
    (match body with
     | `TextBody txt ->
        send_email_ses config from recipients cc bcc subject `Text txt
     | `HtmlBody htm ->
        send_email_ses config from recipients cc bcc subject `Html htm) >>=
      fun (resp, body) ->
      match Response.status resp with
      | `OK ->
         body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string >>=
           fun res ->
           (match res with
            | `Assoc res ->
               (match List.assoc_opt "MessageId" res with
                | Some (`String message_id) -> Err.pure (message_id ^ "@email.amazonses.com")
                | _ -> Lwt.return
                         (Error "Anomaly: return value of SendEmail API call is garbage"))
           | _ -> Lwt.return (Error "Anomaly: impossible!"))
      | _ -> Lwt.return (Error "The API call to Amazon SES returned an erroneous response")
end
