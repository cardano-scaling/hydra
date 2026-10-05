import React from "react";
import Link from "@docusaurus/Link";
import { useNavbarMobileSidebar } from "@docusaurus/theme-common/internal";
import { translate } from "@docusaurus/Translate";
import NavbarColorModeToggle from "@theme/Navbar/ColorModeToggle";
import { useIsLandingPage } from "../../../../hooks/useIsLandingPage";
import { HydraLogoWordmark } from "../../../../components/icons/HydraLogo";
import { BurgerMenuClose } from "../../../../components/icons/BurgerMenu";
function CloseButton() {
  const mobileSidebar = useNavbarMobileSidebar();
  return (
    <button
      type="button"
      aria-label={translate({
        id: "theme.docs.sidebar.closeSidebarButtonAriaLabel",
        message: "Close navigation bar",
        description: "The ARIA label for close button of mobile sidebar",
      })}
      className="navbar-sidebar__close"
      onClick={() => mobileSidebar.toggle()}
    >
      <BurgerMenuClose />
    </button>
  );
}
export default function NavbarMobileSidebarHeader() {
  const isLandingPage = useIsLandingPage();
  const mobileSidebar = useNavbarMobileSidebar();
  return (
    <div className="navbar-sidebar__brand">
      <Link
        to="/"
        className="navbar__brand"
        aria-label="Hydra home"
        onClick={() => mobileSidebar.toggle()}
      >
        <HydraLogoWordmark width={114} height={32} className="text-white" />
      </Link>
      {!isLandingPage && <NavbarColorModeToggle />}
      <CloseButton />
    </div>
  );
}
