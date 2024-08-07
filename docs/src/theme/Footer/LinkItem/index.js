import React from "react";
import Link from "@docusaurus/Link";
import useBaseUrl from "@docusaurus/useBaseUrl";
import { useLocation } from "@docusaurus/router";
import clsx from "clsx";
export default function FooterLinkItem({ item }) {
  const { to, href, label, prependBaseUrlToHref, ...props } = item;
  const toUrl = useBaseUrl(to);
  const normalizedHref = useBaseUrl(href, { forcePrependBaseUrl: true });
  const location = useLocation();
  return (
    <Link
      className={clsx(
        "footer__link-item inline-flex hover:text-teal-light",
        location.pathname === "/" || location.pathname === "/head-protocol/"
          ? "text-[#F3F4F4]"
          : "text-black"
      )}
      {...(href
        ? {
            href: prependBaseUrlToHref ? normalizedHref : href,
          }
        : {
            to: toUrl,
          })}
      {...props}
    >
      {label}
      {/* {href && !isInternalUrl(href) && (
        <IconExternalLink className="self-center" />
      )} */}
    </Link>
  );
}
