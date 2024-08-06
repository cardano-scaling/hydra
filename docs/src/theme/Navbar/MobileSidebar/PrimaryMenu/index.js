import React from "react";
import { useThemeConfig } from "@docusaurus/theme-common";
import { useNavbarMobileSidebar } from "@docusaurus/theme-common/internal";
import NavbarItem from "@theme/NavbarItem";
import { GithubSmall } from "../../../../components/icons/Github";
import Discord from "../../../../components/icons/Discord";
import NavbarSearch from "@theme/Navbar/Search";
import SearchBar from "@theme/SearchBar";
function useNavbarItems() {
  // TODO temporary casting until ThemeConfig type is improved
  return useThemeConfig().navbar.items;
}
// The primary menu displays the navbar items
export default function NavbarMobilePrimaryMenu() {
  const mobileSidebar = useNavbarMobileSidebar();
  // TODO how can the order be defined for mobile?
  // Should we allow providing a different list of items?
  const items = useNavbarItems();
  const searchBarItem = items.find((item) => item.type === "search");
  return (
    <ul className="mobile-side-menu flex flex-col gap-6 px-[13px] tablet:px-[36px] pt-[76px] text-xl leading-[27px]">
      {items.map((item, i) => (
        <NavbarItem
          mobile
          {...item}
          onClick={() => mobileSidebar.toggle()}
          key={i}
        />
      ))}
      {!searchBarItem && (
        <NavbarSearch className="inline-flex gap-3">
          <SearchBar /> Search
        </NavbarSearch>
      )}
      <a
        href="https://github.com/cardano-scaling/hydra"
        target="_blank"
        rel="noopener noreferrer"
        class="hover:text-teal-light mx-3 py-1 inline-flex gap-3"
      >
        <GithubSmall /> Github
      </a>
      <a
        href="https://github.com/cardano-scaling/hydra"
        target="_blank"
        rel="noopener noreferrer"
        class="hover:text-teal-light mx-3 py-1 inline-flex gap-3"
      >
        <Discord className="mt-1" /> Discord
      </a>
    </ul>
  );
}
