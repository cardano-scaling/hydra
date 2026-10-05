import { translate } from "@docusaurus/Translate";

export const HowItWorksCarouselContent = [
  {
    description: translate({
      id: "homepage.carousel.panelOneText",
      message:
        "Think of a Hydra head as an airplane traveling between two points. Before departure, passengers (participants) and supplies (tokens) are loaded on board. ",
    }),
    src: require("@site/static/img/how-it-works-panel-one.png").default,
    mobileSrc: require("@site/static/img/how-it-works-panel-one-mobile.png")
      .default,
  },
  {
    description: translate({
      id: "homepage.carousel.panelTwoText",
      message:
        "Once in flight, no new passengers or supplies can be added or removed, but the supplies can be rearranged within the plane. This internal movement can include executing agreements (smart contracts), determining which items go where, and ensuring proper validations.",
    }),
    src: require("@site/static/img/how-it-works-panel-two.png").default,
    mobileSrc: require("@site/static/img/how-it-works-panel-two-mobile.png")
      .default,
  },
  {
    description: translate({
      id: "homepage.carousel.panelThreeText",
      message:
        "While the plane is in motion, the direct courier (blockchain) continues to operate independently.",
    }),
    src: require("@site/static/img/how-it-works-panel-three.png").default,
    mobileSrc: require("@site/static/img/how-it-works-panel-three-mobile.png")
      .default,
  },
  {
    description: translate({
      id: "homepage.carousel.panelFourText",
      message:
        "When the plane reaches its destination (head closure), only the final arrangement of supplies is reported back to the courier. It doesn’t matter if you shift things around once or 10 million times, only the end result is communicated to the courier. There is no limit to how many planes can be in the air at any given time; if you need more capacity, you can send up more. Thus, the Hydra head acts as a parallel processing unit, enhancing scalability and efficiency without burdening the main network.",
    }),
    src: require("@site/static/img/how-it-works-panel-four.png").default,
    mobileSrc: require("@site/static/img/how-it-works-panel-four-mobile.png")
      .default,
  },
];
