## preamble
echo This file is not intended to be executed directly. Now exiting..
exit 1

#### general notes ####
# we assume the tezos client binary path is saved to $tz

## to import test accounts
# alice : tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
$tz import secret key alice unencrypted:edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq
#   bob : tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6
$tz import secret key   bob unencrypted:edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt


#############################################################
#### create a self-signed certificate for the dev server ####
#############################################################
# create a folder to hold the dev certificates, assuming the directory
# being $SSL_DIR hereafter
cd $SSL_DIR

# ref: https://www.openssl.org/docs/man1.1.1/man1/openssl-req.html
# NB: you may need to upgrade your openssl (OpenSSL>=v1.1, or LibreSSL>=3.1) to use the -addext option
openssl req -nodes -new -x509 -sha256 -newkey rsa:2048 -keyout tscadev.key -out tscadev.cert -days 365 -subj '/C=JP/CN=tsca_devcert' -addext 'subjectAltName = DNS:localhost, DNS:_localtest._dev.tsca.kxc.io, DNS:*._localtest2._dev.tsca.kxc.io'

# trust the newly generated self-signed certificate tscadev.cert
# this certificate covers _localtest._dev.tsca.kxc.io and *._localtest2._dev.tsca.kxc.io,
# which all resolves to 127.0.0.1
# hereafter we assume you have executed `export sslconf=$SSL_DIR/tscadev.cert:$SSL_DIR/tscadev.key`

# test the certificates newly generated using kserve
# assuming under a temporary directory
mkdir -p testroot && echo 'hello, it works ;)' > testroot/index.txt
$kserve -port 8000 -ssl $sslconf -root testroot -welcome testroot/index.txt
# open https://_localtest._dev.tsca.kxc.io:8000 with browser;
# you should see no SSL warnings and a page displays "hello, it works ;)"

# to create a Java Keystore to be used with dev server of webapp, use the following commands
openssl pkcs12 -export -out tscadev.p12 -inkey tscadev.key -in tscadev.cert
# the dev server expects a password of "shadow-cljs"
keytool -importkeystore -destkeystore keystore.jks -srcstoretype PKCS12 -srckeystore tscadev.p12


##########################################
#### build the agency backend project ####
##########################################
## follow instructions in SETUP.txt to build the following binaries
# - agency/agency_server.exe, the omnibus API and web server for the agency
# - broker/broker.exe, a utility to manage the broker contract
# - indexer/indexer.exe, a utility to dump information about TSCA managed smart contracts on-chain

## their usage is currently not well documented. please refer to their corresponding source code for reference:
# - agency/agency_server.exe <== agency/agency_server.ml
# - broker/broker.exe <== broker/broker.ml
# - indexer/indexer.exe <== indexer/indexer.ml

###############################
#### broker administration ####
###############################
## originate the broker contract, assuming 'alice' being the administrator
eval "$(admin="$($tz show address alice 2>/dev/null | grep Hash: | cut -c'7-')"; ./broker/broker.exe -tzprog '$tz' -banner 'demo broker' -tzcli-broker-originate -admin "$admin" -src $admin -kt)"
# this will by default save the originated contract under name tsca_broker
# hereafter we assume the contract name is $broker_kt

# upload a contract template, here the "Book of Frozen" as an example
eval "$(./broker/broker.exe -tzcli-sudo-upload-template -tzprog '$tz' -tmplhash 0xc2251bff8a2dfa2dfbfe47fb1ab080f4 -tmplid 0 -ccgen frozen.ccg -src alice)"
# config (dis/enable, set fees, etc) the broker
eval "$(./broker/broker.exe -tzcli-sudo-config-broker -tzprog '$tz' -src alice -enable -fee '0.5>tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb')"
# config (dis/enable, set fees, etc) a contract template
eval "$(./broker/broker.exe -tzcli-sudo-config-template -tzprog '$tz' -tmplid 0 -src alice -enable -fee '0.5>tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb')"

# to check broker status, you could use
./indexer/indexer.exe -endpoint 'http://localhost:8732' -brokers $broker_kt


###################################
#### running the agency server ####
###################################
# we assume you have a build of the agency webapp, whose asset root is $agency_webapp_root

# you probably need to firstly run this depending on your OS's settings
ulimit -n unlimited

# assume - $broker_kt, the address book name for the broker contract
#        - $SSL_DIR, where SSL key and certificate located
#        - $tezos_rpc_endpoint, the rpc endpoint of a tezos node
#        - $agency_webapp_root, where the agency webapp is located
#        - $agency_webapp_spa, where the agency webapp SPA file is located
# option: sudo setcap CAP_NET_BIND_SERVICE=+eip agency/agency_server.exe
# option: add option "-bind 0.0.0.0" to accept global traffics
(
port=8090
kthash = "$($tz list known contracts | grep -e "^$broker_kt:" | cut -d' ' -f2)"
./agency/agency_server.exe -port $port -admin-http-token 4624fe43fa2d57f096795381 \
  -endpoint $tezos_rpc_endpoint \
  -serve-agency-spa $agency_webapp_spa -static-assets $agency_webapp_root \
  -dev-ssl -sslkey $SSL_DIR/tscadev.key -sslcert $SSL_DIR/tscadev.cert \
  -agency-base 'https://_localtest._dev.tsca.kxc.io:8090' \
  -bookapp-domain _localtest2._dev.tsca.kxc.io \
  -index /tmp/tsca-index -reindex-at-start -reindex-per-nsec 30 \
  -brokers "$kthash" \
  -bookapp-repository $(pwd)/bookapps
)


###########################################
#### agency webapp manual test routine ####
###########################################

# assuming the demo server is running at https://alpha.tezosign.io/

## the goal is to test the following:
## (1) contract ("spirit") origination
## (2) contract info query, via "BookApp"
## (3) contract interaction, from "BookApp"

## sub-goal (1) testing contract origination
# - go to https://alpha.tezosign.io/
# - select "Book of Frozen"
# - check informationed displayed, tick all checkboxes
# - click "Launch"
# - enter contract origination parameters (Fund_amount, Frozen_until, Fund_owners)
#   each of the parameters are explained in the information page
# - click "Proceed" to proceed to the "Chain Clerk" page
# - enter contact information (currently ignored) and originator's address (cannot change later)
# - click "Proceed" to perform on-chain operation simulation
# - if the simulation results looks good, tick "it looks good to me"
# - under the "Use CLI" tab, instructions are given to perform the on-chain operation
# - after finish the on-chain operation, click the "BookApp URL" link.
#   this will open the "BookApp" for the newly created contract,
#   which provides a webui to display information about and provide interaction with the contract.
#   note that before the contract is originated on-chain, the URL will return 404 Not_found

## sub-goal (2) testing BookApp on-chain contract state query
# - go to the "BookApp URL" of the contract originated in sub-goal (1)
# - you will find the current state of the contract displayed on the screen
#   this information is updated on the server-side periodically (configurable).
#   in the case of a contract created from "Book of Frozen", its BookApp will
#   display (a) the remaining fund balance (Fund Blaance),
#           (b) when the fund could be accessed (Frozen Until)
#           (c) who have access to the remaining fund (Owners)
#           (d) the on-chain smart contract hash (Contract)

## sub-goal (3) testing contract interaciton via BookApp
# - go to the "BookApp URL" of the contract originated in sub-goal (1)
# - assuming the contract is originated from "Book of Frozen",
#   its BookApp will have a "Withdraw" button
# - click the "Withdraw" button, a dialog will be displayed
# - enter information for the withdraw: Amount and Beneficiary
# - click "Proceed" to proceed to the "Chain Clerk" page
# - follow the same instruction as sub-goal (1) about Chain Clerk,
# - this time, after the on-chain operation, a withdraw option will be performed on the smart contract
# - you can close the dialog, wait a minutes or so, and refresh to see the changes in balance
#   (you can use `curl -X POST https://$server_host/_tscadev/re-index -H 'Authorization: Bearer admin:$admin_token'`
#    to force a re-index to see the balance changes earlier. $admin_token is set as one of the agency server's options)

###########################
#### running a sandbox ####
###########################
# run a delphibox
docker run -it --rm --name delphibox -p 20000:20000 tqtezos/flextesa:20201214 delphibox start

# run a hangzbox
docker run -it --rm --name delphibox -p 20000:20000 oxheadalpha/flextesa:20211221 hangzbox start

# to keep data, try something like
docker run -it --rm --name delphibox -v $HOME/data/delphi_root:/tmp/mini-box -p 20000:20000 tqtezos/flextesa:20201214 delphibox start --keep-root
