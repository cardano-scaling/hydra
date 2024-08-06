import React from "react";
import clsx from "clsx";
import Github from "../../../components/icons/Github";
import Link from "@docusaurus/Link";

export default function FooterLayout({ style, links, logo, copyright }) {
  return (
    <footer
      className={clsx("laptop:footer component bg-teal-dark", {
        "footer--dark": style === "dark",
      })}
    >
      <div className="flex justify-between pageContainer flex-col-reverse laptop:flex-row laptop:justify-normal laptop:gap-20">
        <div className="basis-1/6">
          <div className="flex flex-col gap-4 border border-solid border-teal-light p-6 rounded-lg laptop:max-w-[227px] tablet:max-w-80 mb-[70px]">
            <div className="inline-flex text-teal-light">
              <Github className="self-center" />
              <span className="border-l pl-4 ml-4 text-xl leading-[27px]">
                Join the conversation
              </span>
            </div>
            <span className="text-sm text-white">
              Lorem Ipsum is simply dummy text of the printing{" "}
            </span>
            <Link
              className="px-4 py-3 justify-center text-center border text-sm border-solid bg-teal-lightest border-teal font-bold text-teal rounded-lg no-underline hover:bg-teal-light hover:no-underline hover:text-teal
      "
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
      {/* {(logo || copyright) && (
        <div className="footer__bottom text--center">
          {logo && <div className="margin-bottom--sm">{logo}</div>}
        </div>
      )} */}
    </footer>
  );
}
