import { translate } from "@docusaurus/Translate";
import BasedOnThoroughResearch from "../../src/components/icons/BasedOnThoroughResearch";
import CensorshipResistance from "../../src/components/icons/CensorshipResistance";
import HighThroughput from "../../src/components/icons/HighThroughput";
import IsomorphicStateChannels from "../../src/components/icons/IsomorphicStateChannels";
import LowFees from "../../src/components/icons/LowFees";
import React from "react";
import LowLatency from "../../src/components/icons/LowLatency";

export const FeatureList = [
  {
    title: translate({
      id: "homepage.featureList.lowLatency.title",
      message: "Low latency",
    }),
    icon: <LowLatency />,
    description: translate({
      id: "homepage.featureList.lowLatency.description",
      message:
        "Transaction finality is only bounded by head network latency, resulting in near-instant settlement",
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
  },
];
