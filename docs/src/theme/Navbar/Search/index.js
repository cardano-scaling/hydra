import React from "react";
import clsx from "clsx";
export default function NavbarSearch({ children, className }) {
  return <div className={clsx(className, "py-1 mx-3")}>{children}</div>;
}
