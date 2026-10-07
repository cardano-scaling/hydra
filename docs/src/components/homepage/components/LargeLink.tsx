import React, { FC, ReactNode } from "react";
import clsx from "clsx";
import Link from "@docusaurus/Link";
import { ExternalArrow } from "../../icons/Arrow";

type Props = {
  to: string;
  children: ReactNode;
  mode?: "light" | "dark";
};

const LargeLink: FC<Props> = ({ to, children, mode = "dark" }) => {
  return (
    <Link
      to={to}
      className={clsx("large-link", mode === "light" && "large-link--light")}
    >
      {children}
      <ExternalArrow aria-hidden="true" />
    </Link>
  );
};

export default LargeLink;
