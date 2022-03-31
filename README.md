# tsca-dev setup

NB: if you are using Apple Silicon, make sure to use a Intel shell
and have an Intel version of Homebrew setup properly

## common setup

```bash
git submodule init
git submodule update
opam update
```

## webish/
firstly, install node v17.4.0 (nvm is recommended for this).
then run `npm install -g yarn` to install yarn.

then, under `webish/`, run `yarn install` and `yarn dev` to start
the dev server, or `yarn build` then `yarn start` to start the
production server.

you may find
`sudo setcap CAP_NET_BIND_SERVICE=+eip $(which node)`
helpful to grant permission to nodejs to serve via port 80/443

## proto_pinned/

make sure that you are under `proto_pinned/`

```bash
opam switch create . ocaml-base-compiler.4.09.1 --no-install
eval $(opam env)
opam pin vendors/ppx_meta_conv
opam pin dune 2.9.3 -n
opam pin ppxlib 0.12.0 -n
opam pin tezos-client-base-unix 7.4 -n
opam pin js_of_ocaml 4.0.0 -n
opam install . --deps-only
opam install -y camlon digestif typerep alcotest-lwt \
  ppx_deriving_yojson ppx_inline_test ppx_typerep_conv \
  tezos-client-006-PsCARTHA tezos-client-006-PsCARTHA-commands \
  tezos-protocol-006-PsCARTHA
dune external-lib-deps --missing @@default # and install listed packages
dune build
```

## agency_backend/

make sure that you are under `agency_backend/`

```bash
opam switch create . ocaml-base-compiler.4.12.1 --no-install
eval $(opam env)
opam pin js_of_ocaml 4.0.0 -n
opam pin tezos-client-base-unix 11.1 -n
opam install . -y --deps-only
opam instal -y ppx_deriving_yojson js_of_ocaml-lwt alcotest-lwt uri-sexp \
  tezos-client-011-PtHangz2-commands js_of_ocaml-ppx_deriving_json typerep \
  nocrypto tezos-test-helpers
dune external-lib-deps --missing @@default # and install listed packages
dune build
```

to start the agency backend server, first spin-up a Tezos node with full RPC capacity. 
assuming the Tezos node's address being `http://localhost:8732`, run something like the following
under `agency_backend/`:

```
./agency/agency_server.exe -port 8090 \
    -endpoint http://localhost:8732 \
    -agency-base https://alpha.tezosign.io \
    -bookapp-domain alpha-bookapps.tezosign.io \
    -serve-agency-spa ../webish/.next/server/pages/index.html \
    -static-assets ../webish/.next/server/pages \
    -admin-http-token 4624fe43fa2d57f096795381 \
    -brokers KT1H2zZQP7B7beit5g2LRn7sh7KWosU2htno \
    -index /tmp/tsca-index \
    -reindex-at-start -reindex-per-nsec 15 \
    -dev-cors -loose-widgeto-csp \
    -bookapp-repository "$(pwd)/../bookapps"
```

you need to set the `-brokers` argument according to the broker contract's address
described in the next section. to work with multiple brokers, separate them using commas.

the webapp assumes the agency backend is accessible at localhost:8090.
you can change this behavior by modifying `webish/next.config.js`.
you need to build the webapp with `yarn build` first to populate `webish/.next`.

the front-end implementation in this repository does not build as separated apps,
so `-bookapp-repository` is not useful for the included front-end, but the argument
is expected. any path that points to a directory will suffice but an empty directory
is recommended.

## uploading and configuring broker / template contracts

once `dune build` succeeds under `proto_pinned`, you should get
`proto_pinned/broker/broker.exe`. we refer to this executable as
`$broker.exe` in the following.

to upload the broker contract, run
`$broker.exe -tzcli-broker-originate -banner [your banner] -admin [your admin account key_hash] -src [originating account key_hash] -kt`
this will write the broker contract to /tmp/kt.tz and output
the command you need to execute in order to originate the broker contract.
you can pass option `-tzprog '$tz'` to customize the path of tezos-client binary.
here is an output sample:
```
INFO: contract code written to /tmp/kt.tz
$tezos-client originate contract tsca_broker transferring 0 from tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb running /tmp/kt.tz --init 'Pair (Pair (Pair "v:tsca_proto03~dev" (Left 0))
           (Pair { "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" } False))
     (Pair (Pair "demo broker" (Pair {} 0)) (Pair (Pair {} 0) (Pair {} 0)))' --burn-cap 18 -q
```

to upload a contract template (i.e. ccg file), you first need to find out its hash using
`$broker.exe -hash-ccgen [path-to-ccg]`. here is an output sample:
```
# $broker.exe -hash-ccgen ../../tsca-formaldev/crowdfunding.ccg
rawhash: dc05767b6a2a9f499f583b31d4987449a7d8fe1e93ea15cbd7601cbd67028926
chopped rawhash: dc05767b6a2a9f499f583b31d4987449
b58checked: tmpL1Q76bCk4nhT2Wf4e2K5rjyubW7gR2v
```
you will be needing the "chopped rawhash" in the template uploading command.
then run
`$broker.exe -tzcli-sudo-upload-template -tmplhash 0x[chopped rawhash] -tmplid [id-from-0] -ccgen [path-to-ccg] -src [tezos account key_hash]`
and execute the output of this command. here is an output sample:
```
# $broker.exe -tzcli-sudo-upload-template -tmplhash 0x52a18bf24ffd0c8031ac7b08cf56ea0a -tmplid 0 -ccgen ../../tsca-formaldev/graveyard.ccg -src alice
$tezos-client transfer 0 from alice to tsca_broker --arg 'Right
  (Right
     (Left (Left (Pair 0
                       (Pair 0x0507070200000119071f0001020000001e053d076507650368075e0765036903690765055f036d036907650369036a0931000000d50765036903690765055f036d036902000000c2020000000f03210316071f000102000000020317050d0369072f020000001707430368010000000c756e7061636b20706172616d03270200000087034c050d0369072f020000001907430368010000000e756e7061636b2073746f726167650327020000005c034c034203210316050d036c072f020000001707430368010000000c756e7061636b20706172616d03270200000004052000010317053d036d0342020000000f03210316071f000102000000020317071f00010200000002030c0342000000000743036801000000046d61696e03420342031b020000002d0520000109310000001f07600368036e055f076503680369020000000c05200001053d07650368036900000000
                             (Some 0x52a18bf24ffd0c8031ac7b08cf56ea0a))))))' --burn-cap 18 -q
```

after originating the broker contract and uploading the contract templates,
you need to enable and configure fee collection for the broker and templates.

to enable and configure fee collection for the broker, run
`$broker.exe -tzcli-sudo-config-broker -src [tezos address key_hash] -enable -fee '[broker charge]>[broker charge collector key_hash]`
to get the command you need to execute. here is an sample output:
```
# $broker.exe -tzcli-sudo-config-broker -src alice -enable -fee '0.5>tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb'
$tezos-client transfer 0 from alice to tsca_broker --arg 'Right
  (Right
     (Right
        (Right
           (Right
              (Pair (Some (Right (Pair 500000 "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb")))
                    (Pair (Some True) None))))))' --burn-cap 18 -q
```

to enable and configure fee collection for a template, run
`$broker.exe -tzcli-sudo-config-template -tmplid [template tmplid] -src [tezos address key_hash] -enable -fee '[template charge]>[template charge collector key_hash]'`
to get the command you need to execute. here is an sample output:
```
# $broker.exe -tzcli-sudo-config-template -tmplid 1 -src alice -enable -fee '0.5>tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb'
$tezos-client transfer 0 from alice to tsca_broker --arg 'Right
  (Right
     (Left (Right
              (Pair (Pair 1 (Some (Right (Pair 500000 "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"))))
                    (Pair (Some True) None)))))' --burn-cap 18 -q
```

