import type {NextPage} from 'next'
import AgencyGlobalContext from "../../models/AgencyGlobalContext";
import {useContext, useEffect, useState} from "react";
import {RotateLoader} from "react-spinners";
import {advertized_book} from "../../models/AiiTypes";
import Link from 'next/link';

const BooksListing: NextPage = () => {

  let [advertized, setAdvertized] = useState<advertized_book[]|null>(null);

  let agencyGlobalContext = useContext(AgencyGlobalContext);
  useEffect(() => {
    return agencyGlobalContext.onAgencyInternalInterfaceAvailable(aii => {
      aii.RefMaster.listAdvertizedBooks().then(setAdvertized);
    })
  }, [agencyGlobalContext]);

  return (
    <>
      <main>
        <h2>TSCA Available Books of Contract</h2>
        <div style={{margin: "0 auto"}}>
          { advertized ? (
              <ul>
                {advertized.map((x) => (
                    <li key={x.bookhash}>
                      <Link href={`/books/${x.bookident}`}>
                        <a>{x.title}</a>
                      </Link>&nbsp; {x.synopsis}
                    </li>
                ))}
              </ul>
          ) : <RotateLoader />
          }
        </div>
      </main>
    </>
  );
}

export default BooksListing
