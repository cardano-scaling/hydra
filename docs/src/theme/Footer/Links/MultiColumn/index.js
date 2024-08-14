import React from "react";
import LinkItem from "@theme/Footer/LinkItem";
import clsx from "clsx";
import { useIsLandingPage } from "../../../../hooks/useIsLandingPage";
function ColumnLinkItem({ item }) {
  return item.html ? (
    <li
      className="footer__item"
      // Developer provided the HTML, so assume it's safe.
      // eslint-disable-next-line react/no-danger
      dangerouslySetInnerHTML={{ __html: item.html }}
    />
  ) : (
    <li key={item.href ?? item.to} className="footer__item">
      <LinkItem item={item} />
    </li>
  );
}
function Column({ column }) {
  const isLandingPage = useIsLandingPage();
  return (
    <div
      className={
        isLandingPage ? "w-full tablet:max-w-[175px]" : "tablet:col basis-1/4"
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
      <ul className="footer__items clean-list space-y-2">
        {column.items.map((item, i) => (
          <ColumnLinkItem key={i} item={item} />
        ))}
      </ul>
    </div>
  );
}
export default function FooterLinksMultiColumn({ columns }) {
  const isLandingPage = useIsLandingPage();
  return (
    <div
      className={clsx(
        "grid gap-10 pb-10 max-w-md",
        "tablet:flex tablet:max-w-full laptop:pb-0 laptop:gap-7",
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
