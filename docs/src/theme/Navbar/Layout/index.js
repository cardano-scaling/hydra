import React, { useEffect, useState } from "react";
import clsx from "clsx";
import { useThemeConfig } from "@docusaurus/theme-common";
import {
  useHideableNavbar,
  useNavbarMobileSidebar,
} from "@docusaurus/theme-common/internal";
import { translate } from "@docusaurus/Translate";
import NavbarMobileSidebar from "@theme/Navbar/MobileSidebar";
import styles from "./styles.module.css";

import { motion, useMotionValueEvent, useScroll } from "framer-motion";
import { useIsLandingPage } from "../../../hooks/useIsLandingPage";

const homepageNavbarVariants = {
  visible: { opacity: 1, y: 0 },
  hidden: { opacity: 0, y: -64 },
};

function useHomepageNavbarScroll(enabled) {
  const { scrollY } = useScroll();
  const [hidden, setHidden] = useState(false);
  const [atTop, setAtTop] = useState(true);

  const update = (latest, previous) => {
    if (!enabled) {
      return;
    }
    setAtTop(latest <= 55);
    if (latest < previous) {
      setHidden(false);
    } else if (latest > 50 && latest > previous) {
      setHidden(true);
    }
  };

  useEffect(() => {
    update(scrollY.get(), scrollY.get());
  }, [enabled]);

  useMotionValueEvent(scrollY, "change", (latest) => {
    update(latest, scrollY.getPrevious() ?? 0);
  });

  return { hidden, atTop };
}

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
  const { hidden, atTop } = useHomepageNavbarScroll(isLandingPage);
  const isHomepageNavbarHidden = hidden && !mobileSidebar.shown;
  return (
    <motion.header
      {...(isLandingPage && {
        variants: homepageNavbarVariants,
        initial: false,
        animate: isHomepageNavbarHidden ? "hidden" : "visible",
        transition: { ease: [0.1, 0.25, 0.3, 1], duration: 0.6 },
        style: { pointerEvents: isHomepageNavbarHidden ? "none" : "auto" },
      })}
      ref={navbarRef}
      aria-label={translate({
        id: "theme.NavBar.navAriaLabel",
        message: "Main",
        description: "The ARIA label for the main navigation",
      })}
      className={clsx(
        "flex navbar !px-0 shadow-none z-50",
        isLandingPage
          ? "navbar--homepage border-none py-4 h-[88px] tablet:py-[21px] tablet:h-[100px]"
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
      {isLandingPage && (
        <div
          aria-hidden
          className={clsx(
            "navbar-homepage-background",
            !atTop && "navbar-homepage-background--scrolled"
          )}
        />
      )}
      <div className={clsx(isLandingPage ? "pageContainer" : "w-full px-6")}>
        {children}
      </div>
      <NavbarBackdrop onClick={mobileSidebar.toggle} />
      <NavbarMobileSidebar />
    </motion.header>
  );
}
