import {TSCAInternalInterface} from "./AiiTypes";
import React, {Context, useContext, useEffect, useState} from "react";

export type selfDeregisterer = () => void;

export interface AgencyGlobalContextType {
  getAgencyInternalInterface() : TSCAInternalInterface|null;
  setAgencyInternalInterface(aii: TSCAInternalInterface) : void;
  onAgencyInternalInterfaceAvailable(
      callback: ((aii: TSCAInternalInterface) => any)
  ) : selfDeregisterer;
}

export class AgencyGlobalContextImpl implements AgencyGlobalContextType {
  private counter: number;
  private aii: TSCAInternalInterface|null;
  private aiiAvailListeners: { [key: number] : ((aii: TSCAInternalInterface) => any) };

  constructor() {
    this.counter = 0;
    this.aii = null;
    this.aiiAvailListeners = [];
  }

  getAgencyInternalInterface(): TSCAInternalInterface | null {
    return this.aii;
  }

  onAgencyInternalInterfaceAvailable(callback: (aii: TSCAInternalInterface) => any): selfDeregisterer {
    const key = this.counter++;
    this.aiiAvailListeners[key] = callback;
    if (this.aii) callback(this.aii);
    return () => {
      delete this.aiiAvailListeners[key];
    };
  }

  setAgencyInternalInterface(aii: TSCAInternalInterface): void {
    this.aii = aii;
    if (!!aii) {
      Object.values(this.aiiAvailListeners).forEach(cb => cb(aii));
    }
  }
}

const AgencyGlobalContext: Context<AgencyGlobalContextType> =
    React.createContext<AgencyGlobalContextType>(new AgencyGlobalContextImpl());

export default AgencyGlobalContext;
