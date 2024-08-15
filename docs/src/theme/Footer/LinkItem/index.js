import React from "react";
import Link from "@docusaurus/Link";
import useBaseUrl from "@docusaurus/useBaseUrl";
import clsx from "clsx";
import { useIsLandingPage } from "../../../hooks/useIsLandingPage";
export default function FooterLinkItem({ item }) {
  const { to, href, label, prependBaseUrlToHref, ...props } = item;
  const toUrl = useBaseUrl(to);
  const normalizedHref = useBaseUrl(href, { forcePrependBaseUrl: true });
  const isLandingPage = useIsLandingPage();
  return (
    <Link
      className={clsx(
        "footer__link-item inline-flex hover:text-primary-light",
        isLandingPage ? "text-[#F3F4F4]" : "text-black"
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
