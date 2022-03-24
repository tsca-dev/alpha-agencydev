import {NextPage} from "next";
import {useRouter} from "next/router";
import Script from "next/script";
import {useEffect, useState} from "react";
import {TSCABookappInterface} from "../../../../models/AiiTypes";
import Button from "react-bootstrap/Button";
import dynamic from "next/dynamic";
import {Modal} from "react-bootstrap";

const ReactJson = dynamic(() => import('react-json-view'), { ssr: false })

const bookhash = "bkBookOfGraveyardProto0";

const GraveyardProto0BookappPage: NextPage = () => {

  let router = useRouter();
  let { sprthash } = router.query as { sprthash: string };
  const debugging = router.query.debug !== undefined;
  if (debugging) console.debug("query params: ", {
    bookhash, sprthash, router
  })

  let bilurl = `/_tscalibs/wouldbe-bil-jslib.js?sprthash=${sprthash}`;
  let [bilLoaded, setBilLoaded] = useState(false);

  // @ts-ignore
  if (!bilLoaded && (typeof global.window?.TSCABookappInterface !== "undefined")) {
    setBilLoaded(true);
  }

  let [wizardOpeningStatus, setWizardOpeningStatus] = useState<[boolean, number]>([false, 0]);
  let wizardOpened = wizardOpeningStatus[0];
  let closeWizard = () => setWizardOpeningStatus(([opened, cnt]: typeof wizardOpeningStatus) => opened ? [false, cnt] : [opened, cnt]);
  let openWizard = () => setWizardOpeningStatus(([opened, cnt]: typeof wizardOpeningStatus) => opened ? [opened, cnt] : [true, cnt+1]);

  let bil: TSCABookappInterface|null
  if (bilLoaded) {
    // @ts-ignore
    bil = (window.TSCABookappInterface as TSCABookappInterface);
    if (debugging) {
      console.debug("bil: ", bil, "; spirit status: ", bil.spiritStatus());
    }
  }

  let [interp0, setInterp] = useState<any|null>(null);
  let [salabel, setSalabel] = useState<string|null>(null);
  let [refreshCounter, setRefreshConuter] = useState(0);

  let stepRefreshCounter = () => setRefreshConuter((x: number) => x+1);

  let interp: {
    contract: string;
    balance: string;
  } | null = null;

  if (interp0 && interp0[0] === "InterpretationResult") {
    interp = JSON.parse(interp0[1]);
  }

  let wizardUrl = salabel && sprthash && `/widgets/o/sa+cc/ofbook/${bookhash}/${salabel}?spellfor=invocation:${sprthash}`;

  useEffect(() => {
    if (bilLoaded && bil) {
      bil.interpretSpiritStatus("basic.json").then(setInterp);
    }
  }, [bilLoaded, refreshCounter])

  return (<main>
    {sprthash && <Script
        src={bilurl}
        strategy="afterInteractive"
        onLoad={e => {
          setBilLoaded(true);
          console.log("on script loaded, ", e);
        }}/>}
    <h2>Bookapp for Graveyard Contract</h2>
    <dl>
      <dt>Spirit</dt>
      <dd>{sprthash}</dd>
      <dt>On-chain Contract</dt>
      <dd>{interp?.contract}</dd>
      <dt>Current Balance</dt>
      <dd>{interp?.balance}ꜩ <a href="javascript:void(0)" onClick={stepRefreshCounter}>reload</a></dd>
    </dl>
    <Button onClick={() => {
      setSalabel("dump.basic01");
      openWizard()
    }} >Dump my tezzes here</Button>
    <Modal show={wizardOpened}
           onHide={() => {
             closeWizard();
             stepRefreshCounter();
           }}
           fullscreen={true}
           backdrop="static">
      <Modal.Header closeButton>
        <h3>Dump Tezzes</h3>
      </Modal.Header>
      <Modal.Body>
        <iframe style={{width: "100%", height: "100%"}} src={wizardUrl || undefined} />
      </Modal.Body>
      <Modal.Footer>
        <Button onClick={() => {
          openWizard();
          stepRefreshCounter();
        }} variant="outline-secondary">Cancel</Button>
      </Modal.Footer>
    </Modal>
    {debugging && <p>interp: <ReactJson src={interp0 || {}}/></p>}
  </main>);
}

export default GraveyardProto0BookappPage
