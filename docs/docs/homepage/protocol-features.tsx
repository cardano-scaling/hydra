import { translate } from "@docusaurus/Translate";
import OneS from "../../src/components/icons/OneS";
import Layers from "../../src/components/icons/Layers";
import Cash from "../../src/components/icons/Cash";
import DotBox from "../../src/components/icons/DotBox";
import Shield from "../../src/components/icons/Shield";
import Book from "../../src/components/icons/Book";

export const ProtocolFeaturesContent = {
  label: translate({
    id: "homepage.protocolFeatures.label",
    message: "Protocol features",
  }),
  title: translate({
    id: "homepage.protocolFeatures.title",
    message: "Built into the protocol.",
  }),
  text: translate({
    id: "homepage.protocolFeatures.text",
    message:
      "The properties behind every Hydra head, from confirmation speed to who can move your funds.",
  }),
  features: [
    {
      icon: <OneS />,
      title: translate({
        id: "homepage.protocolFeatures.feat.lowLatency.title",
        message: "Low latency",
      }),
      text: translate({
        id: "homepage.protocolFeatures.feat.lowLatency.text",
        message:
          "Confirmed the moment every participant signs. There's no block to wait for.",
      }),
    },
    {
      icon: <Layers />,
      title: translate({
        id: "homepage.protocolFeatures.feat.highThroughput.title",
        message: "High throughput",
      }),
      text: translate({
        id: "homepage.protocolFeatures.feat.highThroughput.text",
        message:
          "Only head participants process each transaction. Capacity scales with their hardware.",
      }),
    },
    {
      icon: <Cash />,
      title: translate({
        id: "homepage.protocolFeatures.feat.lowFees.title",
        message: "Low fees",
      }),
      text: translate({
        id: "homepage.protocolFeatures.feat.lowFees.text",
        message:
          "Each head sets its own fees, down to zero, with no Cardano fee per transaction.",
      }),
    },
    {
      icon: <DotBox />,
      title: translate({
        id: "homepage.protocolFeatures.feat.isomorphic.title",
        message: "Isomorphic state channels",
      }),
      text: translate({
        id: "homepage.protocolFeatures.feat.isomorphic.text",
        message:
          "Runs the same ledger rules and smart contracts as Cardano. Your code works unchanged.",
      }),
    },
    {
      icon: <Shield />,
      title: translate({
        id: "homepage.protocolFeatures.feat.funds.title",
        message: "Your funds, your signature",
      }),
      text: translate({
        id: "homepage.protocolFeatures.feat.funds.text",
        message:
          "No one can move your funds without your signature, and any participant can close the head.",
      }),
    },
    {
      icon: <Book />,
      title: translate({
        id: "homepage.protocolFeatures.feat.research.title",
        message: "Peer-reviewed research",
      }),
      text: translate({
        id: "homepage.protocolFeatures.feat.research.text",
        message:
          "Built on published, peer-reviewed research. Open source and tested in the open.",
      }),
    },
  ],
  cta: {
    label: translate({
      id: "homepage.protocolFeatures.cta.label",
      message: "Protocol overview",
    }),
    url: "/docs/protocol-overview",
  },
};
