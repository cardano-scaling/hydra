import React, {
  PropsWithChildren,
  createContext,
  useCallback,
  useEffect,
} from "react";
import dayjs from "dayjs";
import TrackRoute from "./TrackRoute";
import useHasConsent, { ConsentType } from "./useHasConsent";

export const AnalyticsContext = createContext<
  (eventName: string, eventProps?: Record<string, unknown>) => void
>(() => {});

export function AnalyticsProvider({ children }: PropsWithChildren) {
  const analyticsAccepted = useHasConsent(ConsentType.ANALYTICS);

  const gtag = (...args: any[]) => {
    if (typeof window !== "undefined" && (window as any).gtag) {
      (window as any).gtag(...args);
    }
  };

  const capture = useCallback(
    (eventName: string, eventProperties: Record<string, unknown> = {}) => {
      if (analyticsAccepted) {
        gtag("event", eventName, {
          sent_at_local: dayjs().format(),
          ...eventProperties,
        });
      }
    },
    [analyticsAccepted]
  );

  useEffect(() => {
    const gaMeasurementId = "G-Z847J5GYDW"; // Replace with env variagble
    if (analyticsAccepted) {
      if (!(window as any).gtag) {
        const script = document.createElement("script");
        script.async = true;
        script.src = `https://www.googletagmanager.com/gtag/js?id=${gaMeasurementId}`;
        document.head.appendChild(script);

        // Initialize dataLayer and gtag
        (window as any).dataLayer = (window as any).dataLayer || [];
        (window as any).gtag = function gtag(...args: any[]) {
          (window as any).dataLayer.push(args);
        };
        gtag("js", new Date());
        gtag("config", gaMeasurementId);
      }
    } else {
      // Disable Google Analytics if consent is revoked
      gtag("config", gaMeasurementId, { send_page_view: false });
    }
  }, [analyticsAccepted]);

  return (
    <AnalyticsContext.Provider value={capture}>
      <TrackRoute />
      {children}
    </AnalyticsContext.Provider>
  );
}
