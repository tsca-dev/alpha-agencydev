import {NextPage} from "next";
import {useRouter} from "next/router";
import {useContext, useEffect, useState} from "react";
import {
  book_entry, genesis_processed, hyperlink, invocation_processed, invocation_request, simulation_result,
  spell_assistant_desc,
  spell_assistant_field_desc, spell_assistant_input, spell_assistant_interpretation,
  tezos_network, TSCAInternalInterface
} from "../../../../../../models/AiiTypes";
import AgencyGlobalContext from "../../../../../../models/AgencyGlobalContext";
import {BeatLoader, BounceLoader, RotateLoader} from "react-spinners";
import React from "react";
import {Form, FormControl, InputGroup, ListGroup, NavLink} from "react-bootstrap";
import Button from "react-bootstrap/Button";
import {Stepper} from "react-form-stepper";
import dynamic from "next/dynamic";
const ReactJson = dynamic(() => import('react-json-view'), { ssr: false })
import Link from "next/link";
import assert from "assert";

type strmap = {[key: string]: string};
type setter<t> = (v: t) => void;
type stateTuple<t> = [t, setter<t>]

const renderField = (desc: spell_assistant_field_desc,
                     stateTuple: stateTuple<string>|undefined,
                     paramFixHint: strmap) => {
  if (!stateTuple || (typeof stateTuple[0] === "undefined")) return <BounceLoader />;
  const [state, setState] = stateTuple;
  const empty = state === '';

  if (desc.typ[0] === "atom") {
    if (desc.typ[1][0] === "amount") {
      return <InputGroup hasValidation>
        <FormControl
            onChange={v => setState(v.target.value)}
            defaultValue={empty ? "0" : state}
            aria-label="Amount" isInvalid={!!paramFixHint[desc.name]} />
        <Form.Control.Feedback type={"invalid"} tooltip>Hint: {paramFixHint[desc.name]}</Form.Control.Feedback>
        <InputGroup.Text>ꜩ</InputGroup.Text>
      </InputGroup>;
    } else if (desc.typ[1][0] === "timestamp") {
      return <InputGroup hasValidation><Form.Control
          onChange={v =>
              setState(new Date(v.target.value).toISOString())
          }
          isInvalid={!!paramFixHint[desc.name]}
          type="datetime-local"/>
        <Form.Control.Feedback type={"invalid"} tooltip>Hint: {paramFixHint[desc.name]}</Form.Control.Feedback>
      </InputGroup>;
    } else if (desc.typ[1][0] === "tzaddr") {
      return <InputGroup hasValidation><FormControl
          onChange={v => setState(v.target.value)}
          isInvalid={!!paramFixHint[desc.name]}
          type="text"
          placeholder="tz1.."  />
        <Form.Control.Feedback type={"invalid"} tooltip>Hint: {paramFixHint[desc.name]}</Form.Control.Feedback>
      </InputGroup>;
    }
  } else if (desc.typ[0] === "list") {
    const sep: string = desc.typ[2];
    let tmp = "";
    let control: any = null;
    if (desc.typ[1][0] === "tzaddr") {
      const curr = state === '' ? [] : state.split(sep);
      return <>
        <ListGroup as="ul">
          {empty ? (
              <ListGroup.Item disabled>(empty list)</ListGroup.Item>
          ) : (
              curr.map((v, idx) =>
                  <React.Fragment key={idx}>
                    <ListGroup.Item
                        className="d-flex justify-content-between align-items-start">
                      <span>{v}</span>
                      <a style={{cursor: "pointer", color: "darkred"}}
                         onClick={() => {
                           setState(curr.filter((_v, i) => i !== idx).join(sep));
                         }}
                          // variant="outline-secondary"
                      >remove</a>
                    </ListGroup.Item>
                  </React.Fragment>)
          )}
        </ListGroup>
        <InputGroup hasValidation>
          <FormControl
              onChange={v => {
                control = v.target; // TODO - hacky!!
                tmp = v.target.value
              }}
              isInvalid={!!paramFixHint[desc.name]}
              type="text"
              placeholder="tz1.."  />
          <Form.Control.Feedback type={"invalid"} tooltip>Hint: {paramFixHint[desc.name]}</Form.Control.Feedback>
          <Button
              onClick={() => {
                setState(empty ? tmp : (state + sep + tmp));
                if (control) control.value = "";
              }}
              variant="outline-primary">add</Button>
        </InputGroup>
      </>;
    }
  }
  return <Form.Control
      type="text"
      placeholder={"unsupported field type: "+JSON.stringify(desc.typ)}
      readOnly />;
};


const renderLines = (lines: string[]|null) =>
    <span style={{display: "inline-block", color: "#333"}}>{lines ? lines.join("\n") : "processing.."}</span>;

const SaCcOfBook: NextPage = () => {

  let router = useRouter();
  let { bookhash, salabel } = router.query as { bookhash: string, salabel: string };
  const debugging = router.query.debug !== undefined;
  let spellfor: ["genesis"]|["invocation", string /* sprthash */]|undefined;
  try {
    spellfor = router.query.spellfor && (router.query.spellfor as string).split(":") as any;
    // @ts-ignore
    assert(spellfor[0] === "genesis" || spellfor[0] === "invocation");
  } catch (e) {
    spellfor = undefined;
    console.error(`failed to parse spellfor ('${router.query.spellfor}'): `, e)
  }
  if (debugging) console.debug("query params: ", {
    bookhash, salabel, spellfor, router
  })

  let emptyLoadState: {
    // convention: undefined = not_loaded, null = not_found

    aii: undefined|null|TSCAInternalInterface;
    tezosNetwork: undefined|null|tezos_network;
    bookEntry: undefined|null|book_entry;
    spellAssistantDesc: undefined|null|spell_assistant_desc&{
      interpret: (req: spell_assistant_input) => Promise<spell_assistant_interpretation>
    };
  };

  let [loadState, setLoadState] = useState<typeof emptyLoadState>(emptyLoadState = {
    aii: undefined,
    tezosNetwork: undefined,
    bookEntry: undefined,
    spellAssistantDesc: undefined,
  });
  let updateLoadState = <l extends keyof (typeof emptyLoadState)>(key: l, value: (typeof emptyLoadState)[l]) => {
    setLoadState((prev: typeof loadState) => ({...prev, [key]: value}));
  };

  let loadStatus: "BadQueryParam"|"Loading"|"BookNotFound"|"Loaded"|"SpellAssistantNotFound" =
      !spellfor ? "BadQueryParam" :
          (loadState.tezosNetwork === undefined || loadState.bookEntry === undefined) ? "Loading"
              : (loadState.bookEntry === null ? "BookNotFound"
                  : (loadState.spellAssistantDesc === null ? "SpellAssistantNotFound"
                      : "Loaded"));
  if (debugging) {
    console.debug("loading status: ", loadStatus, "; loading state: ", loadState);
  } else {
    console.debug("loading status: ", loadStatus);
  }

  let emptyWizardState: {
    step: 0|1|2;
    spell: string|undefined;
    newOrTargetSpirit: [string /* sprthash */, hyperlink]|undefined;
    processResult: genesis_processed|invocation_processed|undefined;
    processOrSimulationError: string|undefined;
    showSimulationResult: boolean;
    simulationResult: simulation_result|undefined;
    userName: string|undefined;
    userEmail: string|undefined;
    userTezosAccount: string|undefined;
  };
  let [wizardState, setWizardState] = useState<typeof emptyWizardState>(emptyWizardState = {
    step: 0,
    spell: undefined,
    newOrTargetSpirit: undefined,
    processResult: undefined,
    processOrSimulationError: undefined,
    showSimulationResult: false,
    simulationResult: undefined,
    userName: undefined,
    userEmail: undefined,
    userTezosAccount: undefined,
  });
  let updateWizardState = <l extends keyof (typeof emptyWizardState)>(
      ...kvs: [l, (typeof emptyWizardState)[l]][]
  ) => setWizardState((prev: typeof wizardState) => {
    kvs.forEach(([k,v]) => prev[k] = v);
    return {...prev};
  });
  if (debugging) console.debug("wizard state: ", wizardState);

  let paramInputState = useState<strmap>({});
  let paramFixHintsState = useState<strmap>({});

  // load network, book entry, and spell assistant
  let agencyGlobalContext = useContext(AgencyGlobalContext);
  useEffect(() => {
    return agencyGlobalContext.onAgencyInternalInterfaceAvailable(aii => {
      updateLoadState("aii", aii);
      // load book info
      (async () => {
        updateLoadState("tezosNetwork", await aii.RefMaster.defaultTezosNetwork());

        if (spellfor && spellfor[0] === "invocation") {
          let sprthash = spellfor[1];
          aii.Proto0.bookAppUrlForSpirit(sprthash).then(link =>
              updateWizardState(["newOrTargetSpirit",
                [sprthash, link]]));
        }

        let entry;
        try {
          entry = await aii.InfoBank.getBookEntry(bookhash);
        } catch (e) {
          console.error("failed to load book_entry: ", e);
          entry = null;
        }
        updateLoadState("bookEntry", entry);

        let sa: (typeof emptyLoadState)["spellAssistantDesc"];
        if (entry) {
          try {
            sa = {...(await aii.Proto0.describeSpellAssistant(entry.tmplhash, salabel)),
              interpret : (req) =>
                  aii.Proto0.interpretSpellAsisstant(req)
            };
          } catch (e) {
            console.error("failed to load spell_assistant_desc: ", e);
            sa = null;
          }
          updateLoadState("spellAssistantDesc", sa);
        }
      })();
    });
  }, [agencyGlobalContext, bookhash, salabel]);

  const renderSpellAssistant = (
      [paramInput, setParamInput]: stateTuple<strmap>,
      [paramFixHints, setParamFixHint]: stateTuple<strmap>) => {
    if (loadState.spellAssistantDesc) {
      const {form_title, form_major_button, form_fields, tmplversion, salabel} = loadState.spellAssistantDesc;
      const [major_button_title, major_button_desc] = form_major_button;
      loadState.spellAssistantDesc.form_fields.forEach(fd => paramInput[fd.name] = paramInput[fd.name] || "");
      return (<>
        <h4>{form_title}</h4>
        <dl style={{marginBlockEnd: "2.6em"}}>{
          form_fields.map((fd, idx) =>
              <React.Fragment key={idx}>
                <dt>{fd.name}{fd.mandated && <span style={{color: "darkred"}}> *required</span>}</dt>
                <dd style={{marginInlineStart: "0.6rem"}}>
                  ({fd.doc})
                  <div>{renderField(fd, [paramInput[fd.name], (v => {
                        if (debugging) console.debug(`paramInput['${fd.name}'] changed to '${v}'`);
                        setParamInput({...paramInput, [fd.name]: v});
                      }
                  )], paramFixHints)}</div>
                </dd>
              </React.Fragment>)
        }</dl>
        { form_fields.length === 0 && <div style={{marginBlockEnd: "2.4rem"}}>(there is no parameter that you need to enter)</div> }
        <Button
            onClick={() => {
              if (debugging) console.debug("major button clicked; paramInput = ", paramInput);
              const req: spell_assistant_input = {
                tmplversion, salabel,
                filled_form: [],
              };
              form_fields.forEach(fd => req.filled_form.push([fd.name, paramInput[fd.name]]));
              if (debugging) console.debug("spell_assistant_input = ", req);
              setParamFixHint({});
              loadState.spellAssistantDesc?.interpret(req).then(res => {
                console.log(res);
                if (res[0] === "UnsuccessfulSpellAssistantInterpretation") {
                  const hints: typeof paramFixHints = {};
                  res[1].hints.forEach(([fn, hint]) => hints[fn] = hint);
                  setParamFixHint(hints);
                } else if (res[0] === "SuccessfulSpellAssistantInterpretation") {
                  let duser = loadState.aii!.Proto0.defaultClerkUserInfo();
                  updateWizardState(["spell", res[1]], ["step", 1],
                      ["userName", duser.name], ["userEmail", duser.email], ["userTezosAccount", duser.tzaddr]);
                }
              });
            }}
            title={major_button_desc || undefined}>{major_button_title}</Button>
      </>)
    }
  };

  let renderStartOver = () =>
      <Button onClick={() => setWizardState(emptyWizardState)}
              style={{marginInlineStart: "0.4rem"}}
              variant="outline-warning">Start Over</Button>

  let renderSpiritLink = (sprtspec: Exclude<(typeof wizardState)["newOrTargetSpirit"], null|undefined>) =>
      <><Link href={`/bookapps/${bookhash}/${sprtspec[0]}${debugging?"?debug":""}`}><a title={sprtspec[1].synopsis}>{sprtspec[0]}</a></Link></>;

  if (debugging) console.debug("rerender now");
  return (
      <main style={{ maxWidth: "unset" }}>
        {(() => {
          switch (loadStatus) {
            case "BadQueryParam": return <h3>Error: bad URL</h3>
            case "BookNotFound": return <h3>Error: no such book whose bookhash={bookhash}</h3>
            case "SpellAssistantNotFound": return <h3>Error: no such spell_assistant whose salabel={salabel} where bookhash={bookhash}</h3>
            case "Loading": return <BeatLoader />
            case "Loaded":
              return (<div>
              <Stepper activeStep={wizardState.step} steps={[
                {label: "Enter Contract Parameters"},
                {label: "Enter Account Info"},
                {label: "Execute on Blockchain"},
              ]} />
              { (wizardState.step === 0) && (<>
                    {renderSpellAssistant(paramInputState, paramFixHintsState)}
                    {renderStartOver()}
                  </>
              ) }
              { (wizardState.step === 1 || wizardState.step === 2) && (
                  <>
                    <div style={{display: "flex", flexDirection: "row", width: "100%", flexWrap: "wrap"}}>
                      <div style={{border: "solid 0.3pt darkgray", padding: "1.2rem", minWidth: "24rem", maxWidth: "42rem", flex: "2"}}>
                        <h6>Chain Action Description</h6>
                        <dl>
                          { spellfor && spellfor[0] === "genesis" && <>
                            <div>
                              <dt style={{display: "inline"}}>Action Kind: </dt>
                              <dd style={{display: "inline"}}>Smart Contract Genesis</dd>
                            </div>
                            <div>
                              <dt style={{display: "inline"}}>Book: </dt>
                              <dd style={{display: "inline"}}><Link href={`/books/${loadState.bookEntry?.bookident}`}>
                                <a>{loadState.bookEntry?.title}</a>
                              </Link></dd>
                            </div>
                            <div>
                              <dt style={{display: "inline"}}>New Spirit: </dt>
                              <dd style={{display: "inline"}}>{
                                wizardState.newOrTargetSpirit
                                    ? renderSpiritLink(wizardState.newOrTargetSpirit)
                                    : ("(to be assigned)")
                              }</dd>
                            </div>
                          </> }
                          { spellfor && spellfor[0] === "invocation" && <>
                            <div>
                              <dt style={{display: "inline"}}>Action Kind: </dt>
                              <dd style={{display: "inline"}}>Contract Interaction</dd>
                            </div>
                            <div>
                              <dt style={{display: "inline"}}>Regarding: </dt>
                              <dd style={{display: "inline"}}>{
                                wizardState.newOrTargetSpirit
                                    ? renderSpiritLink(wizardState.newOrTargetSpirit)
                                    : ("(no-info-available)")
                              }</dd>
                            </div>
                            <div>
                              <dt style={{display: "inline"}}>Contract Book: </dt>
                              <dd style={{display: "inline"}}><Link href={`/books/${loadState.bookEntry?.bookident}`}>
                                <a>{loadState.bookEntry?.title}</a>
                              </Link></dd>
                            </div>
                          </> }
                          <dt style={{display: "inline"}}>Spell: </dt>
                          <dd style={{display: "inline", fontFamily: "monospace", overflow: "scroll"}}>{wizardState.spell}</dd>
                        </dl>
                      </div>
                      <div style={{border: "solid 0.3pt darkgray", padding: "1.2rem", minWidth: "180pt", flex: "5"}}>
                        <h6>User and Account Information</h6>
                        <dl>
                          <dt>Name</dt>
                          <dd><Form.Control
                              defaultValue={wizardState.userName}
                              onChange={v => updateWizardState(["userName", v.target.value])}
                              type="text" placeholder="Enter your name" /></dd>
                          <dt>Email</dt>
                          <dd><Form.Control
                              defaultValue={wizardState.userEmail}
                              onChange={v => updateWizardState(["userEmail", v.target.value])}
                              type="email" placeholder="Enter email" /></dd>
                          <dt>Tezos Account</dt>
                          <dd><Form.Control
                              defaultValue={wizardState.userTezosAccount}
                              onChange={v => updateWizardState(["userTezosAccount", v.target.value])}
                              type="text" placeholder="Enter your Tezos wallet address" /></dd>
                        </dl>
                        <Button variant="primary"
                                onClick={async () => {
                                  let aii = loadState.aii!;

                                  if (spellfor && spellfor[0] === "genesis") {
                                    let req = {
                                      email: wizardState.userEmail!,
                                      name: wizardState.userName!,
                                      network: loadState.tezosNetwork!,
                                      requester: wizardState.userTezosAccount!,
                                      spell: wizardState.spell!,
                                      template: loadState.bookEntry?.tmplhash!
                                    };
                                    try {
                                      let proc = await aii.Proto0.processGenesisRequest(req);
                                      updateWizardState(["processResult", proc]);
                                      aii.Proto0.bookAppUrlForSpirit(proc.sprthash).then(link =>
                                          updateWizardState(["newOrTargetSpirit", [proc.sprthash, link]]));
                                      let sim = await aii.Proto0.simulateGenesis(req, proc)
                                      updateWizardState(["simulationResult", sim]);

                                      if (sim[0] === "SimulationSucceeded") {
                                        updateWizardState(["step", 2], ["processOrSimulationError", undefined]);
                                      } else {
                                        updateWizardState(["step", 1]);
                                      }
                                    } catch (e) {
                                      console.error("processSpell error", e);
                                      updateWizardState(
                                          ["processOrSimulationError", `${(e as XMLHttpRequest).responseText}`],
                                          ["simulationResult", undefined],
                                          ["step", 1]);
                                      return;
                                    }
                                  } else if (spellfor && spellfor[0] === "invocation") {
                                    let sprthash = spellfor[1];
                                    let req: invocation_request = {
                                      spirit: sprthash,
                                      email: wizardState.userEmail!,
                                      name: wizardState.userName!,
                                      network: loadState.tezosNetwork!,
                                      requester: wizardState.userTezosAccount!,
                                      spell: wizardState.spell!
                                    };
                                    try {
                                      let proc = await aii.Proto0.processInvocationRequest(req);
                                      updateWizardState(["processResult", proc]);
                                      aii.Proto0.bookAppUrlForSpirit(proc.sprthash).then(link =>
                                          updateWizardState(["newOrTargetSpirit", [proc.sprthash, link]]));
                                      let sim = await aii.Proto0.simulateInvocation(req, proc)
                                      updateWizardState(["simulationResult", sim]);

                                      if (sim[0] === "SimulationSucceeded") {
                                        updateWizardState(["step", 2], ["processOrSimulationError", undefined]);
                                      } else {
                                        updateWizardState(["step", 1]);
                                      }
                                    } catch (e) {
                                      console.error("processSpell error", e);
                                      updateWizardState(
                                          ["processOrSimulationError", `${(e as XMLHttpRequest).responseText}`],
                                          ["simulationResult", undefined],
                                          ["step", 1]);
                                      return;
                                    }
                                  }
                                }}
                                disabled={ !wizardState.userName
                                    || !wizardState.userEmail
                                    || !wizardState.userTezosAccount }
                        >{ (wizardState.simulationResult)
                            ? (wizardState.simulationResult[0]==="SimulationSucceeded" ? "Re-process" : "Retry")
                            : (!!wizardState.processOrSimulationError ? "Retry" : "Process")}</Button>
                        {renderStartOver()}
                      </div>
                    </div>
                    { (wizardState.simulationResult) && (<>
                          <div style={{border: "solid 0.3pt darkgray", padding: "1.2rem", maxHeight: "40rem", overflow: "scroll"}}>
                            {wizardState.simulationResult[0] === "SimulationSucceeded" && <>
                              <h4 style={{color: "darkgreen"}}>Simulation Succeeded {
                                <span style={{color: "darkgray", fontSize: "1rem", fontWeight: "unset", cursor: "pointer"}}
                                >({!wizardState.showSimulationResult
                                    ? (<a onClick={() => updateWizardState(["showSimulationResult", true])}>show details</a>)
                                    : (<a onClick={() => updateWizardState(["showSimulationResult", false])}>hide details</a>)
                                })</span>
                              }</h4>
                              <pre style={{display: wizardState.showSimulationResult ? "unset" : "none"}}
                              >{wizardState.simulationResult[1].simulation_output}</pre>
                              <div style={{width: "100%", height: "0.6pt", background: "lightgray", margin: "0.4rem auto"}}/>
                              <p>
                                To execute the action on blockchain, please copy and execute the following command.
                                {spellfor && spellfor[0] === "genesis" && <>
                                  <br/>After executing the action, you could access your contract at {renderSpiritLink(wizardState.newOrTargetSpirit!)}.
                                </>}
                              </p>
                              <pre>{
                              loadState.aii!.Helpers.formatCliInstructionIntoString(
                              wizardState.processResult!.cli_instructions)}</pre>
                            </>}
                            {wizardState.simulationResult[0] === "SimulationFailed" && <>
                              <h4 style={{color: "darkred"}}>Simulation Failed</h4>
                              <pre>{wizardState.simulationResult[1].error_message}</pre>
                            </>}
                          </div>
                        </>
                    )}
                    { (wizardState.processOrSimulationError) && (<>
                          <div style={{border: "solid 0.3pt darkgray", padding: "1.2rem", maxHeight: "40rem", overflow: "scroll"}}>
                            <h4 style={{color: "darkred"}}>Process / Simulation Error</h4>
                            <pre>{wizardState.processOrSimulationError}</pre>
                          </div>
                        </>
                    )}
                  </>
              )}
              {debugging && <>
                <ReactJson src={{...wizardState, paramInput: paramInputState[0], paramFixHints: paramFixHintsState[0]}}
                           collapsed={false} name={false} displayDataTypes={false} />
                <ReactJson src={loadState} collapsed={true}
                           name={false} displayDataTypes={false} />
              </>}
            </div>)
            default: return <h3>something is wrong. contact support.</h3>
          }
        })()}
      </main>
      );
}

export default SaCcOfBook;
