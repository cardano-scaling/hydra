import React, { FC } from "react";
import { translate } from "@docusaurus/Translate";
import clsx from "clsx";
import LowLatency from "../icons/LowLatency";
import HighThroughput from "../icons/HighThroughput";
import LowFees from "../icons/LowFees";
import IsomorphicStateChannels from "../icons/IsomorphicStateChannels";
import CensorshipResistance from "../icons/CensorshipResistance";
import BasedOnThoroughResearch from "../icons/BasedOnThoroughResearch";

const FeatureList = [
  {
    title: translate({
      id: "homepage.featureList.lowLatency.title",
      message: "Low Latency",
    }),
    icon: <LowLatency />,
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
  {
    title: translate({
      id: "homepage.featureList.highThroughput.title",
      message: "High throughput",
    }),
    icon: <HighThroughput />,
    description: translate({
      id: "homepage.featureList.highThroughput.description",
      message:
        "Transactions are replicated only among protocol participants, reducing data processing and increasing throughput",
    }),
    tagLine: translate({
      id: "homepage.featureList.highThroughput.perk",
      message: "Fast and cheap transactions",
    }),
  },
  {
    title: translate({
      id: "homepage.featureList.lowFees.title",
      message: "Low fees",
    }),
    icon: <LowFees />,
    description: translate({
      id: "homepage.featureList.lowFees.description",
      message:
        "Low processing needs by protocol participants and configurable protocol parameters enable even zero-fee use cases",
    }),
    tagLine: translate({
      id: "homepage.featureList.lowFees.perk",
      message: "Fast and cheap transactions",
    }),
  },
  {
    title: translate({
      id: "homepage.featureList.isomorphicStateChannels.title",
      message: "Isomorphic state channels",
    }),
    icon: <IsomorphicStateChannels />,
    description: translate({
      id: "homepage.featureList.isomorphicStateChannels.description",
      message:
        "Cardano transactions are processed off-chain using the exact same battle-tested Cardano ledger",
    }),
    tagLine: translate({
      id: "homepage.featureList.isomorphicStateChannels.perk",
      message: "Native security",
    }),
  },
  {
    title: translate({
      id: "homepage.featureList.censorshipResistance.title",
      message: "Censorship resistance",
    }),
    icon: <CensorshipResistance />,
    description: translate({
      id: "homepage.featureList.censorshipResistance.description",
      message:
        "Participants of a Hydra head cannot lose any funds that they have not explicitly authorized",
    }),
    tagLine: translate({
      id: "homepage.featureList.censorshipResistance.perk",
      message: "Native security",
    }),
  },
  {
    title: translate({
      id: "homepage.featureList.basedOnThoroughResearch.title",
      message: "Based on thorough research",
    }),
    icon: <BasedOnThoroughResearch />,
    description: translate({
      id: "homepage.featureList.basedOnThoroughResearch.description",
      message:
        "The Hydra protocols have been peer-reviewed and implementations are heavily tested resulting in high-security standards",
    }),
    tagLine: translate({
      id: "homepage.featureList.basedOnThoroughResearch.perk",
      message: "Native security",
    }),
  },
];

type Props = {
  icon: React.JSX.Element;
  title: string;
  description: string;
  tagLine: string;
};

const Feature: FC<Props> = ({ icon, title, description, tagLine }) => {
  return (
    <div className="flex flex-col gap-4">
      <div className="inline-flex gap-3 border-b pb-4 border-gray [&>*:first-child]:mt-1">
        {icon}
        <h3 className="text-2xl">{title}</h3>
      </div>
      <p className="max-w-80">{description}</p>
      <span
        className={clsx(
          "p-2 w-fit",
          tagLine === "Fast and cheap transactions"
            ? "text-teal bg-[#F3F6F8]"
            : "text-darkRed bg-[#F8F4F6]"
        )}
      >
        {tagLine}
      </span>
    </div>
  );
};

const Features: FC = () => {
  return (
    <section className="component bg-white">
      <div className="pageContainer">
        <h4 className="text-base text-teal pb-[72px]">/ FEATURES</h4>
        <div className="grid laptop:grid-cols-3 laptop:grid-flow-row tablet:grid-rows-3 tablet:grid-flow-col gap-x-[52px] tablet:gap-y-6 gap-y-14">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
};

export default Features;
