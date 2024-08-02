import React from "react";
import LinkItem from "@theme/Footer/LinkItem";
function ColumnLinkItem({ item }) {
  return item.html ? (
    <li
      className="footer__item"
      // Developer provided the HTML, so assume it's safe.
      // eslint-disable-next-line react/no-danger
      dangerouslySetInnerHTML={{ __html: item.html }}
    />
  ) : (
    <li key={item.href ?? item.to} className="footer__item whitespace-nowrap">
      <LinkItem item={item} />
    </li>
  );
}
function Column({ column }) {
  return (
    <div className="tablet:col">
      <div className="footer__title laptop:pb-8 pb-2">{column.title}</div>
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
    <div className="laptop:flex laptop:gap-8 grid tablet:grid-cols-2 tablet:gap-y-14 gap-10 pb-14 laptop:pb-0 max-w-md">
      {columns.map((column, i) => (
        <Column key={i} column={column} />
      ))}
    </div>
  );
}
