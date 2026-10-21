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
        "L2 confirmation depends on validation, networking, and signatures from all parties; returning funds to L1 also takes chain time",
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
        "Cardano transaction formats and ledger validation are reused off-chain, with head-specific parameters and limitations",
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
        "An honest party can use signed snapshots to contest an outdated close, provided it monitors Cardano within the contestation period",
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
        "Hydra builds on peer-reviewed research, an executable specification, and implementation tests",
    }),
  },
];
