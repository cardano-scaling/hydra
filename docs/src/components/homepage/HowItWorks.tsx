import React, { FC, useState } from "react";
import { translate } from "@docusaurus/Translate";
import clsx from "clsx";
import Arrow from "../icons/Arrow";
import { forTablet } from "../../../helpers/media-queries";
import useMediaQuery from "../../hooks/useMediaQuery";

const HowItWorksContent = [
  {
    title: translate({
      id: "homepage.featureList.lowLatency.title",
      message: "Low Latency",
    }),
    src: require("@site/static/img/knight.png").default,
    description: translate({
      id: "homepage.featureList.lowLatency.description",
      message:
        "Transaction finality is only bounded by Head network latency, resulting in near-instant settlement",
    }),
    tagLine: translate({
      id: "homepage.featureList.lowLatency.perk",
      message: "Fast and cheap transactions",
    }),
  },
];

const HowItWorks: FC = () => {
  const [expanded, setExpanded] = useState(false);
  const isTabletUp = useMediaQuery(forTablet);
  return (
    <section className="component">
      <h4 className="text-base text-teal pb-14">/ HOW IT WORKS</h4>
      <div className="laptop:flex laptop:flex-row grid">
        <div
          className={clsx(
            "flex flex-col gap-4 laptop:w-[472px] basis-[32%] laptop:pt-0 pt-4 order-2 laptop:-order-1",
            expanded
              ? "h-full overflow-visible"
              : "laptop:h-[320px] overflow-hidden"
          )}
        >
          <h4 className="text-2xl color-darkRed font-medium text-darkRed">
            Why Hydra Head?
          </h4>
          <p>
            Traditional blockchain systems face scalability limitationsdue to
            the trade-off between decentralization, security, and scalability
            (the blockchain trilemma). Cardano's consensus algorithm, while
            efficient, still requires massive global replication of state
            changes, potentially increasing transaction settlement times during
            peak hours.
          </p>
          {expanded && (
            <>
              <p className="z-10">
                The Hydra Head protocol aims to enhance the flexibility of
                decentralization levels on a case-by-case basis. Not every
                transaction requires global consensus to be considered final.
                For instance, buying a croissant or lending money to a friend
                doesn’t need the involvement of a central bank. Many
                transactions and arrangements can occur within a single Hydra
                head, with only the final outcome recorded on the main chain.
              </p>
              <p className="z-10">
                The protocol facilitates frictionless state transfer between the
                main chain and individual heads, bypassing the 20-second block
                time limit. This enables state evolution at a pace approved by
                involved parties, independent of blockchain constraints.
              </p>
            </>
          )}
          <div className="w-full z-20">
            <button
              className="bg-none text-teal self-start inline-flex gap-3 group"
              onClick={() => setExpanded(!expanded)}
            >
              {`Read ${expanded ? "less" : "more"}`}{" "}
              <Arrow
                className={clsx(
                  "mt-1 rounded-full group-hover:bg-teal/15",
                  expanded ? "-rotate-90" : "rotate-90"
                )}
              />
            </button>
          </div>
        </div>
        {isTabletUp && (
          <div
            className={clsx(
              "basis-[60%] border-b border-solid border-teal inline-block -order-1 laptop:order-2",
              expanded
                ? "h-full overflow-visible"
                : "laptop:h-[320px] overflow-hidden w-full h-[300px]"
            )}
          >
            {/* <div className="basis-[60%] border-b border-solid border-teal overflow-hidden"> */}
            <img src="hydra-docs-landing-graphic.png" className="-z-10" />
          </div>
        )}
      </div>
    </section>
  );
};

export default HowItWorks;
