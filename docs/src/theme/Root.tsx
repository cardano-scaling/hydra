import React, { PropsWithChildren } from "react";
import { AnalyticsProvider } from "../analytics/AnalyticsContext";

export default function Root({ children }: PropsWithChildren) {
  return <AnalyticsProvider>{children}</AnalyticsProvider>;
}
