import React from "react";
import Link from "@docusaurus/Link";
import { useThemeConfig } from "@docusaurus/theme-common";
import { useNavbarMobileSidebar } from "@docusaurus/theme-common/internal";
import NavbarItem from "@theme/NavbarItem";
import { useIsLandingPage } from "../../../../hooks/useIsLandingPage";
import Code from "../../../../components/icons/Code";
import { ExternalArrow, RightArrow } from "../../../../components/icons/Arrow";
function useNavbarItems() {
  const { navbar, homepageNavbarItems } = useThemeConfig();
  const isLandingPage = useIsLandingPage();
  return isLandingPage
    ? homepageNavbarItems.filter((item) => item.position === "center")
    : navbar.items.filter((item) => item.type !== "html");
}
// The primary menu displays the navbar items
export default function NavbarMobilePrimaryMenu() {
  const mobileSidebar = useNavbarMobileSidebar();
  const { mobileSidebarLinks } = useThemeConfig();
  const items = useNavbarItems();
  const closeSidebar = () => mobileSidebar.toggle();
  return (
    <div className="mobile-side-menu">
      <div className="mobile-side-menu__main">
        <ul className="mobile-side-menu__items menu__list">
          {items.map((item, i) => (
            <NavbarItem mobile {...item} onClick={closeSidebar} key={i} />
          ))}
        </ul>
        <div className="mobile-side-menu__buttons">
          <Link
            to="/docs/getting-started"
            className="link-button"
            onClick={closeSidebar}
          >
            Open your first head <RightArrow />
          </Link>
          <Link
            to="/docs"
            className="link-button link-button-transparent"
            onClick={closeSidebar}
          >
            Read the Docs <Code />
          </Link>
        </div>
      </div>
      <ul className="mobile-side-menu__links clean-list">
        {mobileSidebarLinks.map((item, i) => (
          <li key={i}>
            <Link
              className="mobile-side-menu__link"
              {...(item.href ? { href: item.href } : { to: item.to })}
              onClick={closeSidebar}
            >
              {item.label}
              <ExternalArrow width={12} height={12} aria-hidden="true" />
            </Link>
          </li>
        ))}
      </ul>
    </div>
  );
}
