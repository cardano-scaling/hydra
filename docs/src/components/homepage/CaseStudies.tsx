import React, { FC } from "react";
import { translate } from "@docusaurus/Translate";
import Link from "@docusaurus/Link";
import Arrow from "../icons/Arrow";
import useMediaQuery from "../../hooks/useMediaQuery";
import { forTablet } from "../../../helpers/media-queries";

const FeaturedCaseStudy = {
  content: translate({
    id: "homepage.caseStudy.content",
    message:
      "From enabling micropayments with near-zero feesand instant processing to boosting the performanceof decentralized finance (DeFi) applications and enhancing real-time gaming experiences, Hydra can enhance user engagement across various sectors. Discover how Hydra’s cost-efficient, scalable, andlow-latency transactions can address real-world problems and support innovative solutions on the Cardano platform. ",
  }),
  src: require("@site/static/img/case-studies.png").default,
  mobileSrc: require("@site/static/img/case-studies-mobile.png").default,
};

const CaseStudies: FC = () => {
  const isTabletUp = useMediaQuery(forTablet);
  return (
    <section className="bg-[#E7EEF0]">
      <div className="component pageContainer">
        <h4 className="text-base text-teal pb-14">/ CASE STUDIES</h4>
        <div className="bg-white flex tablet:flex-row flex-col rounded-2xl laptop:mb-10 tablet:p-8justify-between gap-8">
          <div className="flex flex-col laptop:pt-24 gap-8 laptop:max-w-md px-4 py-8 tablet:p-0 tablet:basis-2/5">
            <h4 className="text-2xl text-teal font-medium">Case Studies</h4>
            <p>{FeaturedCaseStudy.content}</p>
            <Link
              className="bg-none text-teal self-start inline-flex gap-3 group hover:no-underline hover:text-teal"
              href="/"
            >
              {"View case studies "}
              <Arrow className="mt-1 rounded-full group-hover:bg-teal/15" />
            </Link>
          </div>
          <div className="tablet:basis-3/5 self-center w-full">
            <img
              width={"100%"}
              src={
                isTabletUp ? FeaturedCaseStudy.src : FeaturedCaseStudy.mobileSrc
              }
            />
          </div>
        </div>
      </div>
    </section>
  );
};

export default CaseStudies;
