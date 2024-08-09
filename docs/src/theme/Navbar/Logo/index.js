import React from "react";
import Logo from "@theme/Logo";
export default function NavbarLogo() {
  return (
    <Logo
      className="navbar__brand hover:text-[var(--ifm-navbar-link-color)]"
      imageClassName="navbar__logo"
      titleClassName="navbar__title text--truncate"
    />
  );
}
