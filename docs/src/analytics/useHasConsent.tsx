import { useEffect, useState } from "react";
import {
  OsanoConsentDecision,
  OsanoConsentType,
  OsanoEvent,
  OsanoEventCallback
} from "./osano";

export { OsanoConsentType as ConsentType };

export default function useHasConsent(
  type: OsanoConsentType
): boolean | undefined {
  const initialConsent =
    typeof Osano !== "undefined"
      ? Osano.cm?.getConsent()[type] === OsanoConsentDecision.ACCEPT
      : undefined;

  const [state, setState] = useState<boolean | undefined>(initialConsent);

  useEffect(() => {
    const cm = typeof Osano !== "undefined" ? Osano.cm : undefined;

    if (!cm) {
      return;
    }

    setState(cm.getConsent()[type] === OsanoConsentDecision.ACCEPT);

    const handler: OsanoEventCallback[OsanoEvent.CONSENT_SAVED] = (changed) => {
      if (type in changed) {
        setState(changed[type] === OsanoConsentDecision.ACCEPT);
      }
    };

    cm.addEventListener(OsanoEvent.CONSENT_SAVED, handler);
    return () => {
      cm.removeEventListener(OsanoEvent.CONSENT_SAVED, handler);
    };
  }, [type]);

  return state;
}
