export enum OsanoConsentType {
  ESSENTIAL = "ESSENTIAL",
  STORAGE = "STORAGE",
  MARKETING = "MARKETING",
  PERSONALIZATION = "PERSONALIZATION",
  ANALYTICS = "ANALYTICS",
  OPT_OUT = "OPT_OUT"
}

export enum OsanoConsentDecision {
  ACCEPT = "ACCEPT",
  DENY = "DENY"
}

export type OsanoConsentMap = Record<OsanoConsentType, OsanoConsentDecision>;

export enum OsanoEvent {
  CONSENT_SAVED = "osano-cm-consent-saved"
}

export type OsanoEventCallback = {
  [OsanoEvent.CONSENT_SAVED]: (changed: Partial<OsanoConsentMap>) => void;
};

declare global {
  interface Osano {
    cm?: {
      getConsent(): OsanoConsentMap;
      addEventListener: <T extends OsanoEvent>(
        ev: T,
        callback: OsanoEventCallback[T]
      ) => void;
      removeEventListener: <T extends OsanoEvent>(
        ev: T,
        callback: OsanoEventCallback[T]
      ) => void;
    };
  }

  var Osano: Osano | undefined;
}
