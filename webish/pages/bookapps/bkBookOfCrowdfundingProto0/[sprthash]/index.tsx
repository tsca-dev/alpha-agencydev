import {NextPage} from "next";
import {useRouter} from "next/router";
import Script from "next/script";
import {useEffect, useState} from "react";
import {TSCABookappInterface} from "../../../../models/AiiTypes";
import Button from "react-bootstrap/Button";
import dynamic from "next/dynamic";
import {Modal} from "react-bootstrap";

const ReactJson = dynamic(() => import('react-json-view'), { ssr: false })

const bookhash = "bkBookOfCrowdfundingProto0";

const formatTimestamp = (rfc3339: string) => new Date(rfc3339).toLocaleString();

const NowClock = () => {
  let [now, setNow] = useState(new Date().toLocaleString());
  useEffect(() => {
    let handle = setInterval(() => {
      setNow(new Date().toLocaleString());
    }, 1000);
  }, []);
  return <>{now}</>
}

const BookOfCrowdfundingProto0: NextPage = () => {

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
    raisers: string[];
    status: "before_funding"|"funding_period"|"funding_finished_fund_available"|"funding_finished_fund_withdrawn"|"refunding"|"refunded";
    funding_start: string;
    funding_end: string;
    unconditional_refund_start: string;
    refund_table: [string /* contributor */ , string /* amount */][];
  } | null = null;

  if (interp0 && interp0[0] === "InterpretationResult") {
    interp = JSON.parse(interp0[1]);
  }

  let wizardUrl = salabel && sprthash && `/widgets/o/sa+cc/ofbook/${bookhash}/${salabel}?spellfor=invocation:${sprthash}`;

  useEffect(() => {
    if (bilLoaded && bil) {
      bil.interpretSpiritStatus("basic.json").then(setInterp);
    }
  }, [bilLoaded, refreshCounter]);

  let renderReload = () => <a href="javascript:void(0)" onClick={stepRefreshCounter}>reload</a>;

  let InvocationButton = (props: { expectedStatus?: string|string[], children: React.ReactNode, salabel: string }) => {
    let enabled = debugging || !props.expectedStatus ? true : (interp && (interp.status === props.expectedStatus || (typeof props.expectedStatus === "object" && props.expectedStatus.includes(interp.status))));
    return (<Button disabled={!enabled} style={{marginInlineEnd: "0.4rem"}} onClick={() => {
          setSalabel(props.salabel);
          openWizard();
        }}>{props.children}</Button>);
  };

  return (<main>
    {sprthash && <Script
        src={bilurl}
        strategy="afterInteractive"
        onLoad={e => {
          setBilLoaded(true);
          console.log("on script loaded, ", e);
        }}/>}
    <h2>Bookapp for Crowdfunding Contract</h2>
    <dl>
      <dt>Spirit</dt>
      <dd>{sprthash}</dd>
      <dt>On-chain Contract</dt>
      <dd>{interp?.contract}</dd>
      <dt>Crowdfunding Status {renderReload()}</dt>
      <dd>{interp?.status}<br/>(now = <NowClock />)</dd>
      <dt>Funding Period</dt>
      <dd>{interp && formatTimestamp(interp.funding_start)} -- {interp && formatTimestamp(interp.funding_end)}</dd>
      <dt>Unconditional Refunding</dt>
      <dd>Starting from {interp && formatTimestamp(interp.unconditional_refund_start)}</dd>
      <dt>Current Balance {renderReload()}</dt>
      <dd>{interp?.balance}ꜩ</dd>
      <dt>Fund Raisers</dt>
      <dd><ul>{ interp && (interp.raisers.map((addr, idx) => <li key={idx}>{addr}</li>)) }</ul></dd>
      {(() => {
        if (interp) {
          let status = interp.status;
          if (status === "refunded" || status === "before_funding") {
            return undefined;
          } else {
            return (<>
              <dt>{ status === "refunding" ? "Eligible Refundees" : "Funders"}</dt>
              <dd>{
                  interp.refund_table.length > 0
                      ? (<ul>{interp.refund_table.map(([contributor, mutez], idx) =>
                          <li key={idx}>{contributor} : {parseFloat(mutez) / 1000000}ꜩ</li>)}</ul>)
                      : "(empty)"}</dd>
            </>);
          }
        }
      })()}
    </dl>
    <InvocationButton
        expectedStatus={"funding_period"}
        salabel={"contribute.basic01"}>Contribute</InvocationButton>
    <InvocationButton
        expectedStatus={"funding_finished_fund_available"}
        salabel={"withdraw.basic01"}>Collect Raised Fund</InvocationButton>
    <InvocationButton
        expectedStatus={"refunding"}
        salabel={"refund.basic01"}>Refund</InvocationButton>
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
          closeWizard();
          stepRefreshCounter();
        }} variant="outline-secondary">Cancel</Button>
      </Modal.Footer>
    </Modal>
    {debugging && <p>interp: <ReactJson src={interp || {}}/></p>}
  </main>);
}

export default BookOfCrowdfundingProto0
