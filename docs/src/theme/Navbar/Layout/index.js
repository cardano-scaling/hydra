import React from "react";
import clsx from "clsx";
import { useThemeConfig } from "@docusaurus/theme-common";
import {
  useHideableNavbar,
  useNavbarMobileSidebar,
} from "@docusaurus/theme-common/internal";
import { translate } from "@docusaurus/Translate";
import NavbarMobileSidebar from "@theme/Navbar/MobileSidebar";
import styles from "./styles.module.css";

import { motion } from "framer-motion";
import { useIsLandingPage } from "../../../hooks/useIsLandingPage";

function NavbarBackdrop(props) {
  return (
    <div
      role="presentation"
      {...props}
      className={clsx("navbar-sidebar__backdrop", props.className)}
    />
  );
}
export default function NavbarLayout({ children }) {
  const isLandingPage = useIsLandingPage();
  const {
    navbar: { hideOnScroll },
  } = useThemeConfig();
  const mobileSidebar = useNavbarMobileSidebar();
  const { navbarRef, isNavbarVisible } = useHideableNavbar(hideOnScroll);
  return (
    <motion.header
      style={
        isLandingPage && {
          backgroundColor: "var(--pure-black)",
        }
      }
      ref={navbarRef}
      aria-label={translate({
        id: "theme.NavBar.navAriaLabel",
        message: "Main",
        description: "The ARIA label for the main navigation",
      })}
      className={clsx(
        "flex navbar !px-0 shadow-none z-50",
        isLandingPage
          ? "border-none py-4 h-[88px] tablet:py-[21px] tablet:h-[100px]"
          : "border-b border-[var(--ifm-toc-border-color)] pt-3 pb-4 tablet:px-2 tablet:py-[30px]",
        "navbar--fixed-top",
        hideOnScroll && [
          // styles.navbarHideable,
          !isNavbarVisible && styles.navbarHidden,
        ],
        {
          "navbar-sidebar--show": mobileSidebar.shown,
        }
      )}
    >
      <div className={clsx(isLandingPage ? "pageContainer" : "w-full px-6")}>
        {children}
      </div>
      <NavbarBackdrop onClick={mobileSidebar.toggle} />
      <NavbarMobileSidebar />
    </motion.header>
  );
}
