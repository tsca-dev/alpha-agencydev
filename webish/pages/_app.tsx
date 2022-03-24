import '../styles/globals.css'
import 'bootstrap/dist/css/bootstrap.min.css'
import type { AppProps } from 'next/app'
import Script from 'next/script';
import AgencyGlobalContext, {AgencyGlobalContextImpl, AgencyGlobalContextType} from "../models/AgencyGlobalContext";
import React, {useEffect, useState} from "react";
import {useRouter} from "next/router";

function MyApp({ Component, pageProps }: AppProps) {
  const routerEvents = useRouter().events;
  const [agencyGlobalContext, setAgencyGlobalContext] =
      useState<AgencyGlobalContextType>(new AgencyGlobalContextImpl());

  useEffect(() => {
        routerEvents.on("routeChangeComplete", () => {
          let aii = agencyGlobalContext.getAgencyInternalInterface();
          aii && agencyGlobalContext.setAgencyInternalInterface(aii);
        });
  }, [agencyGlobalContext, routerEvents]);

  return <AgencyGlobalContext.Provider value={agencyGlobalContext}>
    <Script
        src="/_tscalibs/aii-jslib.js"
        strategy="afterInteractive"
        onLoad={() => {
          let loaded;
          if (typeof window !== "undefined" &&
              // @ts-ignore
              (loaded = window.TSCAInternalInterface)) {

            agencyGlobalContext.setAgencyInternalInterface(loaded);
            console.debug("aii-lib loaded: ", loaded);
          }
        }}/>
    <Component {...pageProps} />
  </AgencyGlobalContext.Provider>;
}

export default MyApp
