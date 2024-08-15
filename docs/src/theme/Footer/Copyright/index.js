import React from "react";
import HydraLogo from "../../../components/icons/HydraLogo";
export default function FooterCopyright({ copyright }) {
  return (
    <div
      className="text-primary-light inline-flex gap-4 text-center"
      // Developer provided the HTML, so assume it's safe.
      // eslint-disable-next-line react/no-danger
    >
      <HydraLogo />
      <span className="mt-0.5">{copyright}</span>
    </div>
  );
}
