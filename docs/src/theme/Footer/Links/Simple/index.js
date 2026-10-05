import React from "react";
import LinkItem from "@theme/Footer/LinkItem";
function SimpleLinkItem({ item }) {
  return item.html ? (
    <li
      className="footer__item"
      // Developer provided the HTML, so assume it's safe.
      // eslint-disable-next-line react/no-danger
      dangerouslySetInnerHTML={{ __html: item.html }}
    />
  ) : (
    <li className="footer__item">
      <LinkItem item={item} />
    </li>
  );
}
export default function FooterLinksSimple({ links }) {
  return (
    <ul className="footer__links clean-list">
      {links.map((item, i) => (
        <SimpleLinkItem key={i} item={item} />
      ))}
    </ul>
  );
}
