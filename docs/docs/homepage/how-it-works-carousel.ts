import { translate } from "@docusaurus/Translate";

export const HowItWorksCarouselContent = [
  {
    description: translate({
      id: "homepage.carousel.panelOneText",
      message:
        "A fixed group of parties opens an empty Hydra head on Cardano. Each party runs a node and agrees on the head parameters.",
    }),
    alt: "A fixed group of parties initializes an empty head on Cardano.",
    src: "img/how-it-works-panel-one.svg",
    mobileSrc: "img/how-it-works-panel-one.svg",
  },
  {
    description: translate({
      id: "homepage.carousel.panelTwoText",
      message:
        "Funds can enter and leave while the head is open. Deposits add funds after maturation and settlement; decommits return funds to Cardano. The parties stay the same.",
    }),
    alt: "Deposits add funds to an open head; decommits return funds to Cardano.",
    src: "img/how-it-works-panel-two.svg",
    mobileSrc: "img/how-it-works-panel-two.svg",
  },
  {
    description: translate({
      id: "homepage.carousel.panelThreeText",
      message:
        "Inside the head, parties validate Cardano transactions and sign snapshots together. Payments and smart-contract interactions do not need an L1 transaction for every update.",
    }),
    alt: "Parties validate transactions and sign snapshots without an L1 transaction for each payment.",
    src: "img/how-it-works-panel-three.svg",
    mobileSrc: "img/how-it-works-panel-three.svg",
  },
  {
    description: translate({
      id: "homepage.carousel.panelFourText",
      message:
        "Any party can close the head with a signed snapshot. After the contestation period, fanout returns outputs to Cardano in one or more transactions. Independent heads can operate in parallel.",
    }),
    alt: "Close, allow time to contest, then distribute outputs in one or more fanout transactions.",
    src: "img/how-it-works-panel-four.svg",
    mobileSrc: "img/how-it-works-panel-four.svg",
  },
];
