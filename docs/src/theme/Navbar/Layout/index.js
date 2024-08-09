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

import { useScroll, motion, useTransform } from "framer-motion";
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
  const { scrollY } = useScroll();
  const y = useTransform(
    scrollY,
    [0, 70],
    ["rgba(255, 255, 255, 0)", "rgba(255, 255, 255, 1)"]
  );
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
          backgroundColor: y,
        }
      }
      ref={navbarRef}
      aria-label={translate({
        id: "theme.NavBar.navAriaLabel",
        message: "Main",
        description: "The ARIA label for the main navigation",
      })}
      className={clsx(
        "flex navbar tablet:py-[30px] !px-0 shadow-none z-50",
        isLandingPage
          ? "border-none pt-3"
          : "border-b border-[#EAEAEB] pt-3 pb-4 tablet:px-2",
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
