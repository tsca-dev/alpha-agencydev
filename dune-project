(lang dune 2.6)
(name tsca-agencydev)

(source (github kxcteam/tsca-agencydev))
(license MIT)
(authors "Haochen Xie")
(maintainers "Haochen Xie <hx@kxc.io>")

(generate_opam_files true)
(package
 (name tsca-agencydev)
 (synopsis "TSCA Agency Omnibus Package")
 (version 0.1)
 (depends
  (dune (>= 2.0))
  (lwt (>= 4.0))
  (tls (= 0.11.1))
  (ppxlib (= 0.12.0))
  (js_of_ocaml (= 3.6.0))
  (tezos-client-base-unix (= 7.4))
  (tezos-client-007-PsDELPH1-commands (= 7.4))
  (faraday-lwt-unix (>= 0.7))))
