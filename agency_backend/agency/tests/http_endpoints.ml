open Api_directory.Auxtypes.Http_endpoint

let test_parse str parsed =
  Alcotest.test_case ("parse_endpoint") `Quick
  @@ fun () ->
     let res = (parse_endpoint str) = parsed in
     Alcotest.(check bool) str
       true res

let () =
  Alcotest.run __FILE__
    [ ("parse_endpoint"),
      [
test_parse "localhost"
  (http_endpoint ~tls:false "localhost" 80 "");

test_parse "localhost/"
  (http_endpoint ~tls:false "localhost" 80 "");

test_parse "http://localhost"
  (http_endpoint ~tls:false "localhost" 80 "");

test_parse "https://localhost"
  (http_endpoint ~tls:true "localhost" 443 "");

test_parse "http://localhost:8080"
  (http_endpoint ~tls:false "localhost" 8080 "");

test_parse "http://localhost:8080/"
  (http_endpoint ~tls:false "localhost" 8080 "");

test_parse "http://localhost/path:to/where"
  (http_endpoint ~tls:false "localhost" 80 "/path:to/where");

test_parse "http://localhost:8080/path/where"
  (http_endpoint ~tls:false "localhost" 8080 "/path/where");

test_parse "http://localhost:8080/path/where/"
  (http_endpoint ~tls:false "localhost" 8080 "/path/where");

test_parse "http://localhost:8080/path"
  (http_endpoint ~tls:false "localhost" 8080 "/path");

test_parse "http://localhost:8080/path/"
  (http_endpoint ~tls:false "localhost" 8080 "/path");


test_parse "//localhost"
  (relative_endpoint (`SameProtocol ("localhost", None, "")));

test_parse "//localhost/context/to/"
  (relative_endpoint (`SameProtocol ("localhost", None, "/context/to")));

test_parse "//localhost:334/context"
  (relative_endpoint (`SameProtocol ("localhost", Some 334, "/context")));

test_parse "//localhost:334"
  (relative_endpoint (`SameProtocol ("localhost", Some 334, "")));

test_parse "/context"
  (relative_endpoint (`SameAuthority "context"));

test_parse "/"
  (relative_endpoint (`SameAuthority ""));
      ]
    ]

