export type mutez = number;
export type tzaddr = string;
export type int = number;
export type hexbytes = string;
export type rclabel = string;
export type sprthash = string;

export type tezos_network = {
  netident : string;
  chain_id : string;
}

export type hyperlink = {
  title : string;
  url : string;
  synopsis? : string;
}

export type advertized_book = {
  bookident : string;
  bookhash : string;
  title : string;
  synopsis : string;
}

export type book_review_results = {
  contract_complexity : string;
  certification_status : string;
}

export type provider_info = {
  providerident : string;
  display_name : string;
  introduction : string;
  website : string;
  contact : string;
}

export type book_charges = {
  agency_charge : mutez;
  provider_charge : mutez;
}

export type book_status = {
  bookident : string;
  bookhash : string;
  charges : book_charges;
  review_results : book_review_results;
}
export type book_entry = {
  bookident : string;
  bookhash : string;
  title : string;
  synopsis : string;
  tmplhash : string;
  provider : string;
  contract_parameters_en : [string, string][];
  contract_terms_en : string[];
  contract_caveats_en : string[];
  specifications : hyperlink[];
}

export type cli_instruction_line =
  ["InstructionComment", string]
| ["InstructionParameter", string, string]
| ["TezosClientCommand", string]

export type genesis_request = {
  network: tezos_network;
  template: string;
  requester: tzaddr;
  name: string;
  email: string;
  spell: string;
};

export type genesis_processed = {
  sprthash : string;
  initbal  : mutez;
  agency_charge : mutez;
  provider_charge : mutez;
  cli_instructions : cli_instruction_line[];
  broker : tzaddr;
  tmplid : int;
  genparam : hexbytes;

  txn_argument : string;
  txn_amount : mutez;
}

export type invocation_request = {
  network : tezos_network;
  spirit : sprthash;
  requester : tzaddr;
  name  : string;
  email : string;
  spell : string;
}

export type invocation_processed = {
  sprthash : string;
  invparam : string;
  invparam_unpacked? : string;
  amount : mutez;
  dest_rclabel : string;
  dest_ktaddr : tzaddr;
  cli_instructions : cli_instruction_line[];

  txn_argument : string;
  txn_amount : mutez;
}

export type simulation_result =
| ["SimulationSucceeded", {
  unsigned_transaction : hexbytes;
  watermark : hexbytes;
  simulation_output : string;
}]
| ["SimulationFailed", {
  error_message : string;
}]

export type injection_request = {
  network : tezos_network;
  unsigned_transaction : hexbytes;
  signature : hexbytes;
}

export type injection_result =
| ["InjectionSucceeded", {
  ophash : string;
  originated_contracts : tzaddr[];
}]
| ["InjectionFailed", {
  error_message : string;
}]

export type spirit_info = {
  sprthash : string;
  tmplhash : string;
  network : tezos_network;
  broker : tzaddr;
  requester : tzaddr;
  ensemble : [string, tzaddr][]
}

export type spirit_status = {
  spirit : spirit_info;
  avatars : [rclabel, /*avatar_status*/ any][];
}

export type spirit_interpretation_result =
    ["InterpretationResult", string]
    | ["InterpretationError", { message: string }]
    | ["NoSuchSpiritInterpreter"]
    | ["TmplversionNotAvailable"]

export type spell_assistant_atom_field =
    // | ["line", "proportional" | "monospaced"] // TODO : unsupported yet
    // | ["text", "proportional" | "monospaced"] // TODO : unsupported yet
    // | ["date"] // TODO : unsupported yet
    // | ["toggle"] // TODO : unsupported yet
    | ["timestamp"]
    | ["amount"]
    | ["tzaddr"]
    // | ["integer", string|undefined /* optional unit_desc */] // TODO : unsupported yet

export type spell_assistant_field_type =
  | ["atom", spell_assistant_atom_field]
  // | ["list", spell_assistant_atom_field, string /* delim */] // TODO : unsupported yet
  | ["list", ["tzaddr"], string /* delim */]

export type spell_assistant_field_desc = {
  name : string;
  desc : string;
  typ : spell_assistant_field_type;
  mandated : boolean;

  placeholder? : string;
  doc? : string;
  requirement_desc? : string;
}

export type spell_assistant_desc = {
  salabel : string;
  tmplversion : string;
  form_title : string;
  form_desc : string;
  form_major_button : [string] | [string, string]; // (* [title, desc] *)
  form_fields : spell_assistant_field_desc[];
}

export type spell_assistant_input = {
  salabel : string;
  tmplversion : string;
  filled_form: [string /* field_name */, string /* user_input */][];
}

export type spell_assistant_interpretation =
    | ["UnsuccessfulSpellAssistantInterpretation", {
         error_message : string;
         hints : [string, string][]; /* (field_name |-> fix_hint) */
       }]
    | ["InvalidSpellAssistantInput", {
         error_message? : string;
       }]
    | ["SuccessfulSpellAssistantInterpretation", string /* interpreted spell */]

export type TSCAInternalInterface = {
  RefMaster: {
    defaultTezosNetwork: () => Promise<tezos_network>;
    listAdvertizedBooks: () => Promise<advertized_book[]>;
    getBookStatus: (bookhash: string) => Promise<book_status>;
    getProviderInfo: (providerident: string) => Promise<provider_info>;
  };
  InfoBank: {
    getBookEntry: (bookhash: string) => Promise<book_entry>;
  };
  Proto0: {
    defaultClerkUserInfo: () => { name: string; email: string; tzaddr: tzaddr; }
    processGenesisRequest: (req: genesis_request) => Promise<genesis_processed>;
    simulateGenesis: (req: genesis_request, proc: genesis_processed) => Promise<simulation_result>;
    bookAppUrlForSpirit: (sprthash: string) => Promise<hyperlink>;
    processInvocationRequest: (req: invocation_request) => Promise<invocation_processed>;
    simulateInvocation: (req: invocation_request, proc: invocation_processed) => Promise<simulation_result>;
    injectOperation: (req: injection_request) => Promise<injection_result>;
    describeSpellAssistant: (tmplhash: string, salabel: string) => Promise<spell_assistant_desc>;
    interpretSpellAsisstant: (req: spell_assistant_input) => Promise<spell_assistant_interpretation>
  };
  TezosUtils: {
    calculateaddressfromledgerpublickey: (ledgerPubkey: hexbytes) => Promise<tzaddr>;
  };
  Helpers: {
    formatCliInstructionIntoLines: (lines: cli_instruction_line[]) => string[];
    formatCliInstructionIntoString: (lines: cli_instruction_line[]) => string;
  }
};

export type TSCABookappInterface = {
  network: tezos_network;
  sprthash: string;
  agencyUrl: any;
  spiritStatus: () => Promise<spirit_status>;
  interpretSpiritStatus: (silabel: string, tmplversion?: string) => Promise<spirit_interpretation_result>;
}
