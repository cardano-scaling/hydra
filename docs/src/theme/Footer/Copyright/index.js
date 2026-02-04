import React from "react";
import { useIsLandingPage } from "../../../hooks/useIsLandingPage";
import HydraLogo from "../../../components/icons/HydraLogo";
import clsx from "clsx";

export default function FooterCopyright({ copyright }) {
  const isLandingPage = useIsLandingPage();
  return (
    <div
      className={clsx(
        " inline-flex gap-4 text-center",
        isLandingPage ? "text-primary-alternate-light" : "text-primary"
      )}
      // Developer provided the HTML, so assume it's safe.
      // eslint-disable-next-line react/no-danger
    >
      <HydraLogo />
      <span className="mt-0.5">{copyright}</span>
    </div>
  );
}
