import { translate } from "@docusaurus/Translate";

export const WhatIsHydraContent = {
  label: translate({
    id: "homepage.whatIsHydra.label",
    message: "What is Hydra",
  }),
  title: translate({
    id: "homepage.whatIsHydra.title",
    message: "Open. Run. Settle.",
  }),
  description: translate({
    id: "homepage.whatIsHydra.description",
    message:
      "A Hydra Head is a live channel. Value moves in and out while it runs.",
  }),
  graphicTitle: translate({
    id: "homepage.whatIsHydra.graphicTitle",
    message: "Value docks and departs like a port that never closes.",
  }),
  steps: [
    {
      title: translate({
        id: "homepage.whatIsHydra.steps.open.title",
        message: "Open",
      }),
      description: translate({
        id: "homepage.whatIsHydra.steps.open.description",
        message:
          "One Cardano transaction locks the funds and starts the Hydra Head.",
      }),
    },
    {
      title: translate({
        id: "homepage.whatIsHydra.steps.run.title",
        message: "Run",
      }),
      description: translate({
        id: "homepage.whatIsHydra.steps.run.description",
        message:
          "Transact UTxOs instantly. Funds move in and out. Hydra Heads keep running.",
      }),
    },
    {
      title: translate({
        id: "homepage.whatIsHydra.steps.settle.title",
        message: "Settle",
      }),
      description: translate({
        id: "homepage.whatIsHydra.steps.settle.description",
        message: "Close when you choose, final balances land on Cardano.",
      }),
    },
  ],
};
