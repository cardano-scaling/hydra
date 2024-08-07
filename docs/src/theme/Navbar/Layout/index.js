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

import { useLocation } from "@docusaurus/router";
import { useScroll, motion, useTransform } from "framer-motion";

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
  const location = useLocation();
  const {
    navbar: { hideOnScroll },
  } = useThemeConfig();
  const mobileSidebar = useNavbarMobileSidebar();
  const { navbarRef, isNavbarVisible } = useHideableNavbar(hideOnScroll);
  return (
    <motion.header
      style={
        (location.pathname === "/" ||
          location.pathname === "/head-protocol/") && {
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
        "flex navbar tablet:py-[30px] px-0 shadow-none z-50",
        location.pathname === "/" || location.pathname === "/head-protocol/"
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
      <div
        className={clsx(
          location.pathname === "/" || location.pathname === "/head-protocol/"
            ? "pageContainer"
            : "w-full px-6"
        )}
      >
        {children}
      </div>
      <NavbarBackdrop onClick={mobileSidebar.toggle} />
      <NavbarMobileSidebar />
    </motion.header>
  );
}
