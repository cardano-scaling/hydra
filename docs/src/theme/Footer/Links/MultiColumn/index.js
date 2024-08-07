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
  return (
    <div className="tablet:col basis-1/4">
      <div
        className={clsx(
          "footer__title laptop:pb-8 pb-2",
          location.pathname === "/" || location.pathname === "/head-protocol/"
            ? "text-[#F3F4F4]"
            : "text-black"
        )}
      >
        {column.title}
      </div>
      <ul className="footer__items clean-list">
        {column.items.map((item, i) => (
          <ColumnLinkItem key={i} item={item} />
        ))}
      </ul>
    </div>
  );
}
export default function FooterLinksMultiColumn({ columns }) {
  return (
    <div className="laptop:flex grid tablet:grid-cols-2 tablet:gap-y-14 tablet:gap-x-0 gap-10 pb-14 laptop:pb-0 max-w-md tablet:max-w-[350px] laptop:max-w-full basis-5/6">
      {columns.map((column, i) => (
        <Column key={i} column={column} />
      ))}
    </div>
  );
}
