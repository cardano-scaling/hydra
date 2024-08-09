import React from "react";
import clsx from "clsx";
import Github from "../../../components/icons/Github";
import Link from "@docusaurus/Link";
import { useIsLandingPage } from "../../../hooks/useIsLandingPage";

export default function FooterLayout({ style, links, logo, copyright }) {
  const isLandingPage = useIsLandingPage();
  return (
    <footer
      className={clsx(
        "laptop:footer",
        isLandingPage
          ? "bg-teal-dark component laptop:component-xs"
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
              "flex flex-col gap-4 border border-solid border-teal-light p-6 rounded-lg mb-14 laptop:mb-[70px]",
              !isLandingPage && "laptop:max-w-[227px] tablet:max-w-[277px]"
            )}
          >
            <div className="inline-flex items-center text-teal-light">
              <Github className="shrink-0" width={24} height={24} />
              <span className="border-l pl-4 ml-4 text-xl leading-[27px]">
                Join the conversation
              </span>
            </div>
            <Link
              className="px-4 py-3 justify-center text-center border text-sm border-solid bg-teal-lightest border-teal font-bold text-teal rounded-lg no-underline hover:bg-white hover:no-underline hover:text-teal"
              target="_blank"
              to={"https://github.com/cardano-scaling/hydra"}
            >
              Go Github
            </Link>
          </div>
          {copyright}
        </div>
        {links}
      </div>
    </footer>
  );
}
