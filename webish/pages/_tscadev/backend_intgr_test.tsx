/* eslint-disable */
import type { NextPage } from 'next'
import Head from 'next/head'
import Script from 'next/script'
import {useContext, useEffect, useRef, useState} from "react";
import * as React from "react";
import {
  genesis_processed,
  genesis_request,
  hyperlink, invocation_processed, invocation_request, spirit_interpretation_result, spirit_status,
  tezos_network, TSCABookappInterface,
  TSCAInternalInterface
} from '../../models/AiiTypes';
import dynamic from "next/dynamic";
import AgencyGlobalContext from "../../models/AgencyGlobalContext";

const ReactJson = dynamic(() => import('react-json-view'), { ssr: false })

const incr = (x: number) => x+1;

type RenderableObject = string|boolean|object|undefined|null;
type ObjectRenderingOptions = {
  collapsed?: boolean;
  collapsing?: string[];
}

const renderObj = (x: RenderableObject, opts?: ObjectRenderingOptions) => {
  let content;
  let useReactJson = false;
  if (!x) {
    content = "processing.."
  } else {
    switch (typeof x) {
      case "string": content = x; break;
      case "object": useReactJson = true; break;
      case "boolean": content = Boolean.prototype.toString.call(x); break;
    }
  }

  if (useReactJson) {
    return <ReactJson src={x as object}
                      name={false} displayDataTypes={false}
                      collapsed={opts?.collapsed}
                      shouldCollapse={p => {
                        if (!opts) return false;
                        else if (!opts.collapsing || !p.name) return false;
                        else return opts.collapsing.includes(p.name);
                      }} />
  } else {
    content = content || Object.prototype.toString.call(x);
    return <span style={{color: "#333"}}>{content}</span>;
  }
}
const renderLines = (lines: string[]|null) =>
    <span style={{display: "inline-block", color: "#333"}}>{lines ? lines.join("\n") : "processing.."}</span>;

const AsyncObj = (props: {
  obj: () => Promise<RenderableObject>,
  opts?: ObjectRenderingOptions
}) => {
  const [v, setv] = useState<[true, RenderableObject]|false>(false);
  const func = props.obj;
  useEffect(() => {
    func().then(v => setv([true, v]));
  }, [func, setv]);

  return (v ? renderObj(v[1], props.opts) : <span>processing...</span>)
}

const Label = (props: {children: React.ReactNode }) =>
    <span style={{fontWeight: "bold"}}>{props.children}</span>

const TestCase = (props: {
  children: React.ReactNode,
  title: string,
  onReload?: () => any;
}) => {
  let [collapsed, setCollapsed] = useState(false);
  return (
      <div style={{padding: "0.6rem", border: "1px solid olive", margin: "0.6rem 0", overflow: "scroll"}}>
        <h3 style={{margin: "0", marginBottom: "0.6rem"}}>{props.title}{
            props.onReload && (
                <button style={{margin: "0 0.8rem"}} onClick={props.onReload}>reload</button>
            )}
          <label>
            <input checked={collapsed} type={"checkbox"} onChange={() => setCollapsed(!collapsed)} />
            <span style={{fontWeight: "normal", fontSize: "1rem"}}>expand</span>
          </label>
        </h3>
        <div style={{ display: collapsed ? "unset" : "none"}}>
          {props.children}
        </div>
      </div>
  );
};

class TextField extends React.Component {
  constructor(props: { value?: string; onChange: (v: string|undefined) => any }) {
    super(props);
    this.state = { value: props.value };
  }
  handleChange(event: any) {
    let v: string|undefined = event.target?.value;
    v = v ? v.trim() : v;
    v = v === "" ? undefined : (!v ? undefined : v);
    this.setState({ value: v });
    // @ts-ignore
    this.props.onChange && this.props.onChange(v);
  }
  render() {
    // @ts-ignore
    return <input value={this.state.value} type={"text"} onChange={this.handleChange.bind(this)} />
  }
}

const MakeGenesisTest = (opts : {
  title: string,
  originator: string,
  template: string,
  spell: string
}) => ({ tsca }: { tsca: TSCAInternalInterface }) => {
  let resultEmpty: {
    network: tezos_network|null;
    request: genesis_request|null;
    processedRequest: genesis_processed|null;
    spiritUrl: hyperlink|null;
    simulationResult: any|null;
    cliInstructions: string[]|null;
  };
  const [result, resultUpdater] = useState<typeof resultEmpty>((resultEmpty = {
    simulationResult: null, network: null, request: null, processedRequest: null, cliInstructions: null, spiritUrl: null}));
  const [rc, rcu] = useState(0); // rc = reloading-counter
  useEffect(() => {
    resultUpdater(resultEmpty);

    tsca && (async () => {
      let defaultNetwork = await
          tsca.RefMaster.defaultTezosNetwork();

      const genesisRequest: genesis_request = {
        email: "b.sakai@example.org",
        name: "Bob Sakai",
        network: defaultNetwork,
        requester: opts.originator,
        template: opts.template,
        spell: opts.spell
      }

      resultUpdater(s => ({...s,
        network : defaultNetwork,
        request: genesisRequest}));

      let genesisProcessed = await
          tsca.Proto0.processGenesisRequest(genesisRequest);
      resultUpdater(s => ({...s,
        processedRequest: genesisProcessed,
        cliInstructions: tsca.Helpers.formatCliInstructionIntoLines(genesisProcessed.cli_instructions),
      }));

      let [spiritUrl, simulationResult] = await
          Promise.all([
            tsca.Proto0.bookAppUrlForSpirit(genesisProcessed.sprthash),
            tsca.Proto0.simulateGenesis(genesisRequest, genesisProcessed)]);
      resultUpdater(s => ({...s,
        spiritUrl,
        simulationResult }));
    })();
  }, [tsca, rc]);

  return (<TestCase title={opts.title} onReload={() => rcu(incr)}>
    <pre><Label>network</Label>: {renderObj(result.network)}</pre>
    <pre><Label>request</Label>: {renderObj(result.request)}</pre>
    <pre><Label>processedRequest</Label>: {renderObj(result.processedRequest, {
      collapsing: ["cli_instructions"]
    })}</pre>
    <pre><Label>spiritUrl</Label>: <br/>{renderObj(result.spiritUrl)}</pre>
    <pre><Label>instructions</Label>: <br/>{renderLines(result.cliInstructions)}</pre>
    <pre><Label>simulationResult</Label>: {result.simulationResult && result.simulationResult[0]} {renderObj(result.simulationResult, {
      collapsed: true
    })}</pre>
  </TestCase>);
}

const MakeSpiritTest = (opts : {
  title: string,
  expectingTemplate: string,
  spiritInterpLabel: string,
  invoker?: string,
  spell?: string
}) => ({ tsca }: { tsca: TSCAInternalInterface }) => {
  const [rc, rcu] = useState(0); // rc = reloading-counter

  const [sprthash, setSprthash] = useState<string|undefined>();

  const expectingTmplhash = opts.expectingTemplate;

  let bilurl = `/_tscalibs/wouldbe-bil-jslib.js?sprthash=${sprthash}`;
  const [bil, bilUpdater] = useState<TSCABookappInterface|null>(null);
  const [shouldLoad, setShouldLoad] = useState(false);

  let resultEmpty: {
    network: tezos_network|null;
    spiritStatus: spirit_status|null;
    tmplhashExpected: boolean|null;
    interpreted: spirit_interpretation_result|null;
    interpretationResult: any|null;
    invokeRequest: invocation_request|null;
    invokeProcessed: invocation_processed|null;
    invokeCliInstructions: string[]|null;
    simulationResult: any|null;
  };
  const [result, resultUpdater] = useState<typeof resultEmpty>((resultEmpty = {
    network: null, spiritStatus: null, tmplhashExpected: null, interpreted: null, interpretationResult: null,
    invokeRequest: null, invokeProcessed: null, invokeCliInstructions: null, simulationResult: null}));

  useEffect(() => {
    if (!bil) return;

    (async () => {
      let spiritStatus = await
          bil.spiritStatus();
      resultUpdater(s => ({...s,
        spiritStatus,
        tmplhashExpected: spiritStatus.spirit.tmplhash === expectingTmplhash,
      }));

      let interpreted = await
          bil.interpretSpiritStatus(opts.spiritInterpLabel);
      resultUpdater(s => ({...s,
        interpreted
      }));

      if (interpreted[0] === "InterpretationResult") {
        resultUpdater(s => ({...s,
          interpretationResult: JSON.parse(interpreted[1] as string)
        }))
      }
    })();
  }, [bil, rc, shouldLoad]);

  const performWithdraw = () => {
    if (!opts.invoker || !opts.spell) return;
    if (bil && tsca && sprthash) {
      const invokeRequest: invocation_request = {
        email: "b.sakai@example.org",
        name: "Bob Sakai",
        network: bil.network,
        requester: opts.invoker,
        spell: opts.spell,
        spirit: sprthash,
      };
      resultUpdater(s => ({...s,
        invokeRequest
      }));

      (async () => {
        let invokeProcessed = await tsca.Proto0.processInvocationRequest(invokeRequest);
        let invokeCliInstructions = tsca.Helpers.formatCliInstructionIntoLines(invokeProcessed.cli_instructions);
        resultUpdater(s => ({...s,
          invokeProcessed, invokeCliInstructions
        }));

        let simulationResult = await tsca.Proto0.simulateInvocation(invokeRequest, invokeProcessed);
        resultUpdater(s => ({...s,
          simulationResult
        }));
      })();
    }
  }

  return (<TestCase title={opts.title} onReload={() => rcu(incr)}>
    <label>
      sprthash: <TextField
        // @ts-ignore
        value={sprthash} onChange={v => {
      setShouldLoad(false); setSprthash(v);
    }} /></label>
    <label>
      <input checked={shouldLoad} type={"checkbox"} onChange={() => setShouldLoad(!shouldLoad)} />
      should load?
    </label>

    <pre><Label>Spirit: </Label>{renderObj(sprthash || "not-specified")}</pre>
    {(sprthash && <>

      <pre><Label>{`Spirit BIL URL (loaded? ${!!bil}): `}</Label>{renderObj(bilurl || "not-specified")}</pre>

      {shouldLoad ? <Script
          key={rc}
          src={bilurl}
          strategy="afterInteractive"
          onLoad={() => {
            let loaded;
            if (typeof window !== "undefined" &&
                // @ts-ignore
                (loaded = window.TSCABookappInterface)) {
              // setTscaAii(loaded);
              bilUpdater(loaded);
              console.info("bil-lib loaded: ", loaded);
            }
          }}/> : "not loading bil"}

      <pre><Label>spiritStatus</Label>:{renderObj(result.spiritStatus)}</pre>
      <pre><Label>{`tmplhashExpected (expecting = ${expectingTmplhash}) ? `}</Label>{renderObj(result.tmplhashExpected)}</pre>

      {result.tmplhashExpected && (<>
        <pre><Label>{`interpreted(${opts.spiritInterpLabel})`}</Label>{renderObj(result.interpreted)}</pre>
        <pre><Label>interpretationResult</Label>{renderObj(result.interpretationResult)}</pre>
        {(opts.spell&&opts.invoker) && <button onClick={performWithdraw}>{opts.spell}</button> }
      </>)}

      {result.invokeRequest && (<>
        <pre><Label>invokeRequest</Label>{renderObj(result.invokeRequest)}</pre>
        <pre><Label>invokeProcessed</Label>{renderObj(result.invokeProcessed, {
          collapsing: ["cli_instructions"]
        })}</pre>
        <pre><Label>invokeInstructions</Label>: <br/>{renderLines(result.invokeCliInstructions)}</pre>
        <pre><Label>simulationResult</Label>: {result.simulationResult && result.simulationResult[0]} {renderObj(result.simulationResult, {
          collapsed: true
        })}</pre>
      </>)}
    </>)}
  </TestCase>);
}

const BackendIntegrationTestPage: NextPage = () => {

  // const [tscaAii, setTscaAii] = useState<TSCAInternalInterface|null>(
  //     (typeof window !== "undefined")
  //         // @ts-ignore
  //         ? window.TSCAInternalInterface
  //         : null
  // );

  let [tscaAii, setTscaAii] = useState<TSCAInternalInterface|null>();
  let agencyGlobalContext = useContext(AgencyGlobalContext);
  useEffect(() => {
    return agencyGlobalContext.onAgencyInternalInterfaceAvailable(aii => {
      setTscaAii(aii);
    });
  }, [agencyGlobalContext]);

  const pageTitle = "TSCA Agency Backend Integration Test";

  const TestListingBooks = ({ tsca }: { tsca : TSCAInternalInterface }) => {
    return (<TestCase title={"TestListingBooks"}>
      <AsyncObj obj={async () => {
        let advertizedBooks = await tsca.RefMaster.listAdvertizedBooks();
        let bookEntries = await Promise.all(advertizedBooks.map(b => tsca.InfoBank.getBookEntry(b.bookhash)));
        let bookStatus = await Promise.all(advertizedBooks.map(b => tsca.RefMaster.getBookStatus(b.bookhash)));
        return {advertizedBooks, bookStatus, bookEntries};
      }} opts={{collapsing: ["specifications", "contract_parameters_en", "contract_terms_en", "contract_caveats_en"]}} />
    </TestCase>);
  };

  const DumpIndex = ({ tsca }: { tsca : TSCAInternalInterface }) => {
    const [rc, rcu] = useState(0);

    return (<TestCase title={"DumpIndex"} onReload={() => rcu(incr)}>
      <pre><AsyncObj obj={async () => {
        return (await fetch("/_tscadev/tmp/indexed")).text()
      }} /></pre>
    </TestCase>);
  };

  const aliceAddress = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb";
  const aliceKeyhash = "edpkvGfYw3LyB1UcCahKQk4rF2tvbMUk8GFiTuMjL75uGXrpvKXhjn";

  const bobAddress = "tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6";
  const bobKeyhash = "edpkurPsQ8eUApnLUJ9ZPDvu98E8VNj4KtJa1aZr16Cr5ow5VHKnz4";

  const frozenTmplhash = "tmpL1Q6jgUaZeMvDpuw5axKCwYXGUSvX2P";
  const graveyardTmplhash = "tmpL1Q5BaFCgLb9ouyioFT2rP93FRnsreP";
  const crowdfundingTmplhash = "tmpL1Q6pfrWTjT9t35efx8LDy6wU2rRbKn";

  const timestamp = (x: any) => {
    let date: Date = new Date(x);
    return date.toISOString();
  }

  const GenesisTestFrozen01 = MakeGenesisTest({
    title: "GenesisTestFrozen01",
    originator: aliceAddress,
    template: "tmpL1Q6jgUaZeMvDpuw5axKCwYXGUSvX2P",
    spell: "(frozen0.gen 12312tz (tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb) 2022-03-13T15:00:00.000Z)",
  });

  const SpiritTestFrozen01 = MakeSpiritTest({
    title: "SpiritTestFrozen01",
    expectingTemplate: frozenTmplhash,
    spiritInterpLabel: "basic.json",
    invoker: "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb",
    spell: "(frozen0.withdraw 1tz tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb)",
  });

  const now = Date.now();
  const one_min = 1000*60;

  const GenesisTestCrowdfunding01 = MakeGenesisTest({
    title: "GenesisTestCrowdfunding01",
    originator: aliceAddress,
    template: crowdfundingTmplhash,
    spell: `(crowdfunding0.gen (${aliceAddress}) ${timestamp(now+1*one_min)} ${timestamp(now+3*one_min)} ${timestamp(now+5*one_min)})`
  });

  const SpiritTestCrowdfunding00 = MakeSpiritTest({
    title: "SpiritTestCrowdfunding00",
    expectingTemplate: crowdfundingTmplhash,
    spiritInterpLabel: "storage.json",
  });

  const SpiritTestCrowdfunding01 = MakeSpiritTest({
    title: "SpiritTestCrowdfunding01-contribute",
    expectingTemplate: crowdfundingTmplhash,
    spiritInterpLabel: "basic.json",
    invoker: bobAddress,
    spell: `(crowdfunding0.contribute 5tz ${bobAddress})`,
  });

  const SpiritTestCrowdfunding02 = MakeSpiritTest({
    title: "SpiritTestCrowdfunding02-withdraw",
    expectingTemplate: crowdfundingTmplhash,
    spiritInterpLabel: "basic.json",
    invoker: aliceAddress,
    spell: `(crowdfunding0.withdraw ${aliceAddress})`,
  });

  const SpiritTestCrowdfunding03 = MakeSpiritTest({
    title: "SpiritTestCrowdfunding02-refund",
    expectingTemplate: crowdfundingTmplhash,
    spiritInterpLabel: "basic.json",
    invoker: bobAddress,
    spell: `(crowdfunding0.refund ${bobAddress})`,
  });

  const GenesisTestGraveyard01 = MakeGenesisTest({
    title: "GenesisTestGraveyard01",
    originator: aliceAddress,
    template: graveyardTmplhash,
    spell: "(graveyard0.gen)"
  });

  const SpiritTestGraveyard01 = MakeSpiritTest({
    title: "SpiritTestGraveyard01",
    expectingTemplate: graveyardTmplhash,
    spiritInterpLabel: "basic.json",
    invoker: aliceAddress,
    spell: "(graveyard0.dump 13tz)",
  });

  const SpiritTestGraveyard02 = MakeSpiritTest({
    title: "SpiritTestGraveyard02",
    expectingTemplate: graveyardTmplhash,
    spiritInterpLabel: "basic.json",
    invoker: aliceAddress,
    spell: "(graveyard0.dump 0.3tz)",
  });

  const SpiritTestGraveyard03 = MakeSpiritTest({
    title: "SpiritTestGraveyard03",
    expectingTemplate: graveyardTmplhash,
    spiritInterpLabel: "basic.json",
    invoker: aliceAddress,
    spell: "(graveyard0.dump 0tz)",
  });

  return (
      <div style={{margin: "0.8rem 1.2rem"}}>
        <Head>
          <title>{pageTitle}</title>
        </Head>

        <h2>{pageTitle}</h2>

        {!tscaAii && <span>loading aii....</span>}

        {tscaAii && <TestListingBooks tsca={tscaAii}/>}
        {tscaAii && <DumpIndex tsca={tscaAii}/>}
        {tscaAii && <GenesisTestFrozen01 tsca={tscaAii}/>}
        {tscaAii && <SpiritTestFrozen01 tsca={tscaAii}/>}

        {tscaAii && <GenesisTestCrowdfunding01 tsca={tscaAii}/>}
        {tscaAii && <SpiritTestCrowdfunding00 tsca={tscaAii}/>}
        {tscaAii && <SpiritTestCrowdfunding01 tsca={tscaAii}/>}
        {tscaAii && <SpiritTestCrowdfunding02 tsca={tscaAii}/>}
        {tscaAii && <SpiritTestCrowdfunding03 tsca={tscaAii}/>}

        {tscaAii && <GenesisTestGraveyard01 tsca={tscaAii}/>}
        {tscaAii && <SpiritTestGraveyard01 tsca={tscaAii}/>}
        {tscaAii && <SpiritTestGraveyard02 tsca={tscaAii}/>}
        {tscaAii && <SpiritTestGraveyard03 tsca={tscaAii}/>}
      </div>
  )
}

export default BackendIntegrationTestPage
