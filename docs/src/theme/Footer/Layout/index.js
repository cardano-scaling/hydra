import React from "react";
import clsx from "clsx";
import Link from "@docusaurus/Link";
import { useIsLandingPage } from "../../../hooks/useIsLandingPage";
import HydraLogo from "../../../components/icons/HydraLogo";

export default function FooterLayout({ style, links, logo, copyright }) {
  const isLandingPage = useIsLandingPage();
  return (
    <footer
      className={clsx(
        "laptop:footer",
        isLandingPage
          ? "bg-primary-darker component laptop:component-xs"
          : "bg-white py-[56px]"
      )}
    >
      <div
        className={clsx(
          "flex justify-between flex-col-reverse laptop:flex-row",
          isLandingPage
            ? "pageContainer laptop:gap-12"
            : "w-full px-6 tablet:px-12 laptop:justify-normal laptop:gap-20"
        )}
      >
        <div
          className={isLandingPage ? "shrink-0 phablet:w-[275px]" : "basis-1/6"}
        >
          <div
            className={clsx(
              "flex flex-col gap-4 border border-solid  p-6 rounded-lg mb-14 laptop:mb-[70px]",
              !isLandingPage
                ? "w-[250px] border-primary"
                : "border-primary-alternate-light"
            )}
          >
            <div
              className={clsx(
                "inline-flex items-center ",
                isLandingPage ? "text-primary-alternate-light" : "text-primary"
              )}
            >
              <HydraLogo className="shrink-0" width={24} height={24} />
              <span className="border-l pl-4 ml-4 text-xl leading-[27px]">
                Join the family
              </span>
            </div>
            <Link
              className="px-4 py-3 justify-center text-center border text-sm border-solid hover:bg-primary-alternate-light border-primary font-bold text-primary rounded-lg no-underline bg-white hover:no-underline hover:text-primary"
              to={"/docs/get-involved"}
            >
              Get involved
            </Link>
          </div>
          {copyright}
        </div>
        {links}
      </div>
    </footer>
  );
}
