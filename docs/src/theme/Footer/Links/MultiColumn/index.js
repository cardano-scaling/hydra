import React from "react";
import LinkItem from "@theme/Footer/LinkItem";
import { useLocation } from "@docusaurus/router";
import clsx from "clsx";
function ColumnLinkItem({ item }) {
  return item.html ? (
    <li
      className="footer__item"
      // Developer provided the HTML, so assume it's safe.
      // eslint-disable-next-line react/no-danger
      dangerouslySetInnerHTML={{ __html: item.html }}
    />
  ) : (
    <li
      key={item.href ?? item.to}
      className="footer__item whitespace-nowrap text-sm"
    >
      <LinkItem item={item} />
    </li>
  );
}
function Column({ column }) {
  const location = useLocation();
  const isLandingPage =
    location.pathname === "/" || location.pathname === "/head-protocol/";
  return (
    <div
      className={
        isLandingPage ? "w-full max-w-[175px]" : "tablet:col basis-1/4"
      }
    >
      <div
        className={clsx(
          "footer__title text-sm pb-1 laptop:pb-5",
          isLandingPage ? "text-[#F3F4F4]" : "text-black"
        )}
      >
        {column.title}
      </div>
      <ul className="footer__items clean-list space-y-1">
        {column.items.map((item, i) => (
          <ColumnLinkItem key={i} item={item} />
        ))}
      </ul>
    </div>
  );
}
export default function FooterLinksMultiColumn({ columns }) {
  const isLandingPage =
    location.pathname === "/" || location.pathname === "/head-protocol/";
  return (
    <div
      className={clsx(
        "grid gap-10 pb-10 max-w-md",
        "tablet:pb-14 tablet:grid-cols-2 tablet:gap-y-16 tablet:gap-x-0 tablet:max-w-[350px]",
        "laptop:flex laptop:pb-0 laptop:gap-7",
        isLandingPage
          ? "w-full laptop:max-w-[750px]"
          : "basis-5/6 laptop:max-w-full"
      )}
    >
      {columns.map((column, i) => (
        <Column key={i} column={column} />
      ))}
    </div>
  );
}
