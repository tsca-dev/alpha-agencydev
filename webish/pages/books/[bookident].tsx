import type {NextPage} from 'next'
import Link from 'next/link';
import {useRouter} from "next/router";
import React, {useContext, useEffect, useState} from "react";
import {RotateLoader} from "react-spinners";
import AgencyGlobalContext from "../../models/AgencyGlobalContext";
import {
  book_entry,
  book_status,
  provider_info,
  spell_assistant_desc,
  spell_assistant_input,
  spell_assistant_interpretation
} from "../../models/AiiTypes";
import dynamic from "next/dynamic";
import Button from "react-bootstrap/Button";
import Modal from "react-bootstrap/Modal";

const ReactJson = dynamic(() => import('react-json-view'), { ssr: false })

const BookTopPage: NextPage = () => {

  const router = useRouter();
  const { bookident } = router.query as {
    bookident: string;
  };
  const debugging = router.query.debug !== undefined;

  const [bookInfo, setBookInfo] = useState<{
    book: book_entry,
    provider: provider_info,
    status: book_status,
  }|null>(null);
  const [bookNotFound, setBookNotFound] = useState(false);

  const [genesisSpellAssistant, setGenesisSpellAssistant] =
      useState<spell_assistant_desc&{
        interpret: (req: spell_assistant_input) => Promise<spell_assistant_interpretation>
      }|null>(null);
  const [modalOpen, setModalOpen] = useState(false);

  let agencyGlobalContext = useContext(AgencyGlobalContext);
  useEffect(() => {
    setBookNotFound(false);
    return agencyGlobalContext.onAgencyInternalInterfaceAvailable(aii => {
      // load book info
      aii.RefMaster.listAdvertizedBooks().then(
          advertized => advertized.filter(e => e.bookident === bookident)
      ).then(
          async advertized => {
            if (advertized.length === 0) {
              setBookNotFound(true);
            } else {
              let bookhash = advertized[0].bookhash;
              const book = await aii.InfoBank.getBookEntry(bookhash);
              const provider = await aii.RefMaster.getProviderInfo(book.provider);
              const status = await aii.RefMaster.getBookStatus(bookhash);
              setBookInfo({
                book, provider, status
              });

            }
          }
      );
    });
  }, [agencyGlobalContext, bookident]);

  const renderBookInfo = (info: typeof bookInfo) => {
    if (info) {
      const { book, provider, status } = info;
      return (<>
        <Link href="/books"><a>Back to the list of books</a></Link>
        <h2>{book.title}</h2>
        <section>
          <dl>
            <dt>Synopsis</dt><dd>{book.synopsis}</dd>
            <dt>Tmplhash</dt><dd>{book.tmplhash}</dd>
            <dt>Provider</dt><dd>
              <Link href={provider.website}><a title={provider.introduction}>{provider.display_name}</a></Link>
            </dd>
            <dt>Charges</dt><dd>{
              `${(status.charges.agency_charge + status.charges.provider_charge) / 1000000}tz `
              + `(${status.charges.agency_charge/1000000}tz by agency and ${status.charges.provider_charge/1000000}tz by provider)`
            }</dd>
          </dl>
        </section>
        <section>
          <dl>
            <dt>Contract Parameters</dt>
            <dd>
              <dl style={{marginInlineStart: "1.2rem"}}>
                { book.contract_parameters_en.map(([param, desc], idx) =>
                    <React.Fragment key={idx}>
                      <dt><pre>{param}</pre></dt>
                      <dd>{desc}</dd></React.Fragment>
                )}
              </dl>
            </dd>
            <dt>Contract Terms</dt>
            <dd><ul>{ book.contract_terms_en.map((term, idx) => <li key={idx}>{term}</li>) }</ul></dd>
            <dt>Contract Caveats</dt>
            <dd><ul>{ book.contract_caveats_en.map((caveat, idx) => <li key={idx}>{caveat}</li>) }</ul></dd>
            <dt>Contract Specifications</dt>
            <dd><ul>{ book.specifications.map((link, idx) =>
                <li key={idx}><Link href={link.url}><a>{link.title}</a></Link> : {link.synopsis}</li>) }
            </ul></dd>
          </dl>
        </section>
        <Button disabled={!bookInfo}
                onClick={() => setModalOpen(true)}>Launch this Contract</Button>
        {debugging && <>
          <ReactJson
            src={info} collapsed={true}
            name={false}
            displayDataTypes={false}/>
          <ReactJson
            src={genesisSpellAssistant as object} collapsed={true}
            name={false}
            displayDataTypes={false}/>
        </>}

        <Modal show={modalOpen}
               onHide={() => setModalOpen(false)}
               fullscreen={true}
               backdrop="static">
          <Modal.Header closeButton>
            <h3>Launch {book.title}</h3>
          </Modal.Header>
          <Modal.Body>
            {/* TODO - we blindly assume the genesis spell assistant to have salabel=genesis.basic01 */}
            <iframe height="100%" width="100%" src={`/widgets/sa+cc/ofbook/${book.bookhash}/genesis.basic01?spellfor=genesis`+(debugging?"&debug":"")} />
            {debugging && <>
              <ReactJson
                src={info} collapsed={true}
                name={false}
                displayDataTypes={false}/>
            </>}
          </Modal.Body>
          <Modal.Footer>
            <Button onClick={() => setModalOpen(false)} variant="outline-secondary">Cancel</Button>
          </Modal.Footer>
        </Modal>
      </>);
    }
  };

  return (
      <><main>
        {bookInfo ? (renderBookInfo(bookInfo)
        ) : (<>
          {bookNotFound ? (<>
                <h2>Book not found</h2>
                <Link href="/books">
                <a>return to the list of available books</a>
                </Link>
              </>
          ) : (<>
            <h2>Loading book where bookident = &ldquo;{bookident}&rdquo;</h2>
            <RotateLoader/>
          </>)}
        </>)}
      </main>
      </>
  );
}

export default BookTopPage
