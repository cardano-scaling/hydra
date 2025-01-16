import { useLocation } from "@docusaurus/router";
import React, { useContext, useEffect } from "react";
import { AnalyticsContext } from "./AnalyticsContext";

export default function TrackRoute() {
  const location = useLocation();
  const capture = useContext(AnalyticsContext);

  useEffect(() => {
    capture("page_view", {
      page_path: location.pathname,
      page_search: location.search,
    });
  }, [capture, location.pathname, location.search]);

  return null;
}
