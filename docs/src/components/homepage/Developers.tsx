import { FC } from "react";
import Link from "@docusaurus/Link";
import SectionHeading from "./components/SectionHeading";
import Terminal from "./components/Terminal";
import Code from "../icons/Code";
import { RightArrow } from "../icons/Arrow";
import { DevelopersContent } from "../../../docs/homepage/developers";

const Developers: FC = () => {
  const { label, title, paragraphs, buttons, terminal } = DevelopersContent;
  return (
    <section id="developers" className="developers bg-glow">
      <div className="pageContainer">
        <div className="padding-section developers__inner">
          <div className="developers__intro">
            <SectionHeading
              label={label}
              title={
                <>
                  {title.lineOne} <br className="developers__title-break" />
                  {title.beforeHighlight}{" "}
                  <span className="text-[var(--hydra-red)]">
                    {title.highlight}
                  </span>{" "}
                  {title.afterHighlight}
                </>
              }
              description={
                <>
                  {paragraphs[0]}
                  <br />
                  {paragraphs[1]}
                </>
              }
            />
            <div className="flex flex-col items-start gap-4 tablet:flex-wrap tablet:flex-row">
              <Link
                to={buttons[0].url}
                className="link-button link-button-light"
              >
                {buttons[0].label} <RightArrow className="size-[1.125rem]" />
              </Link>
              <Link
                to={buttons[1].url}
                className="link-button link-button-transparent"
              >
                {buttons[1].label} <Code className="w-[1.125rem] h-4" />
              </Link>
            </div>
          </div>
          <div className="developers__terminal">
            <Terminal title={terminal.title} lines={terminal.lines} />
          </div>
        </div>
      </div>
    </section>
  );
};

export default Developers;
