import React from "react";
import clsx from "clsx";
import { useThemeConfig, ErrorCauseBoundary } from "@docusaurus/theme-common";
import { useLocation } from "@docusaurus/router";
import {
  splitNavbarItems,
  useNavbarMobileSidebar,
} from "@docusaurus/theme-common/internal";
import NavbarItem from "@theme/NavbarItem";
import NavbarColorModeToggle from "@theme/Navbar/ColorModeToggle";
import SearchBar from "@theme/SearchBar";
import NavbarMobileSidebarToggle from "@theme/Navbar/MobileSidebar/Toggle";
import NavbarLogo from "@theme/Navbar/Logo";
import NavbarSearch from "@theme/Navbar/Search";
import styles from "./styles.module.css";
import { GithubSmall } from "../../../components/icons/Github";
import Discord from "../../../components/icons/Discord";
import Link from "@docusaurus/Link";
import Code from "../../../components/icons/Code";
import { HydraLogoWordmark } from "../../../components/icons/HydraLogo";

function useNavbarItems() {
  // TODO temporary casting until ThemeConfig type is improved
  return useThemeConfig().navbar.items;
}

function useNavbarHomepageItems() {
  return useThemeConfig().homepageNavbarItems;
}

function NavbarItems({ items }) {
  return (
    <>
      {items.map((item, i) => (
        <ErrorCauseBoundary
          key={i}
          onError={(error) =>
            new Error(
              `A theme navbar item failed to render.
Please double-check the following navbar item (themeConfig.navbar.items) of your Docusaurus config:
${JSON.stringify(item, null, 2)}`,
              { cause: error }
            )
          }
        >
          <NavbarItem {...item} />
        </ErrorCauseBoundary>
      ))}
    </>
  );
}
function NavbarContentLayout({ className, left, center, right }) {
  return (
    <div className={clsx("navbar__inner", className)}>
      <div className="navbar__items">{left}</div>
      {center && (
        <div className="navbar__items navbar__items--center">{center}</div>
      )}
      <div className="navbar__items navbar__items--right">{right}</div>
    </div>
  );
}
export default function NavbarContent() {
  const mobileSidebar = useNavbarMobileSidebar();
  const location = useLocation();
  const items = useNavbarItems();
  const homepageItems = useNavbarHomepageItems();
  const [leftItems, rightItems] = splitNavbarItems(items);

  const [leftHomepageitems, rightHomepageItems] =
    splitNavbarItems(homepageItems);
  const centerHomepageItems = homepageItems.filter(
    (item) => item.position === "center"
  );
  const searchBarItem = items.find((item) => item.type === "search");
  if (location.pathname === "/head-protocol/") {
    return (
      <NavbarContentLayout
        className="navbar-homepage"
        left={
          <>
            <Link to="/" className="navbar__brand" aria-label="Hydra home">
              <HydraLogoWordmark className="text-white" />
            </Link>
            <NavbarItems items={leftHomepageitems} />
          </>
        }
        center={<NavbarItems items={centerHomepageItems} />}
        right={
          <>
            <a
              href="https://github.com/cardano-scaling/hydra"
              target="_blank"
              rel="noopener noreferrer"
              className="flex text-white hover:text-[var(--hydra-red)]"
              aria-label="Github link"
            >
              <GithubSmall />
            </a>
            <Link
              href="/docs"
              className="link-button link-button-transparent"
              aria-label="Docs link"
            >
              Docs <Code />
            </Link>
            {!mobileSidebar.disabled && <NavbarMobileSidebarToggle />}
          </>
        }
      />
    );
  } else {
    return (
      <NavbarContentLayout
        left={
          <>
            <NavbarLogo />
            <NavbarItems items={leftItems} />
          </>
        }
        right={
          <>
            <NavbarItems items={rightItems} />
            <div class="navColorModeToggle">
              <NavbarColorModeToggle className={styles.colorModeToggle} />
            </div>
            {!searchBarItem && (
              <NavbarSearch>
                <SearchBar />
              </NavbarSearch>
            )}
            <a
              href="https://github.com/cardano-scaling/hydra"
              target="_blank"
              rel="noopener noreferrer"
              className="hover:text-primary-light mx-3 py-1"
              aria-label="Github link"
            >
              <GithubSmall />
            </a>
            <a
              href="https://discord.com/invite/Qq5vNTg9PT"
              target="_blank"
              rel="noopener noreferrer"
              className="hover:text-primary-light mx-3 py-1"
              aria-label="Discord link"
            >
              <Discord />
            </a>
            {!mobileSidebar.disabled && <NavbarMobileSidebarToggle />}
          </>
        }
      />
    );
  }
}
