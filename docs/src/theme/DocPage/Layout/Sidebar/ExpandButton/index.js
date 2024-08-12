import React from "react";
import { translate } from "@docusaurus/Translate";
import styles from "./styles.module.css";
import ToggleMenu from "../../../../../components/icons/ToggleMenu";
export default function DocPageLayoutSidebarExpandButton({ toggleSidebar }) {
  return (
    <div
      className={styles.expandButton}
      title={translate({
        id: "theme.docs.sidebar.expandButtonTitle",
        message: "Expand sidebar",
        description:
          "The ARIA label and title attribute for expand button of doc sidebar",
      })}
      aria-label={translate({
        id: "theme.docs.sidebar.expandButtonAriaLabel",
        message: "Expand sidebar",
        description:
          "The ARIA label and title attribute for expand button of doc sidebar",
      })}
      tabIndex={0}
      role="button"
      onKeyDown={toggleSidebar}
      onClick={toggleSidebar}
    >
      <ToggleMenu className="rotate-180 self-start mt-[120px] " />
    </div>
  );
}
