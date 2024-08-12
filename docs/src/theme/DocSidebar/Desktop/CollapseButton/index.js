import React from "react";
import clsx from "clsx";
import { translate } from "@docusaurus/Translate";
import ToggleMenu from "../../../../components/icons/ToggleMenu";
export default function CollapseButton({ onClick }) {
  return (
    <button
      type="button"
      title={translate({
        id: "theme.docs.sidebar.collapseButtonTitle",
        message: "Collapse sidebar",
        description: "The title attribute for collapse button of doc sidebar",
      })}
      aria-label={translate({
        id: "theme.docs.sidebar.collapseButtonAriaLabel",
        message: "Collapse sidebar",
        description: "The title attribute for collapse button of doc sidebar",
      })}
      className={clsx("inline-flex p-4 mx-6 justify-between")}
      onClick={onClick}
    >
      Collapse side panel <ToggleMenu />
    </button>
  );
}
