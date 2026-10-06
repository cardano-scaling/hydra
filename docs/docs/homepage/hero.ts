import { translate } from "@docusaurus/Translate";

export const HeroContent = {
  label: translate({
    id: "homepage.hero.label",
    message: "Hydra 2.3.0 Live on Cardano mainnet",
  }),
  title: [
    translate({
      id: "homepage.hero.titleOne",
      message: "Run your app off-chain.",
    }),
    translate({
      id: "homepage.hero.titleTwo",
      message: "Settle it on Cardano.",
    }),
  ],
  paragraphs: [
    translate({
      id: "homepage.hero.paragraphOne",
      message:
        "Blockchains like Cardano are secure because every transaction is checked by the whole network, but that takes time. A Hydra Head lets a group of people transact directly with each other, instantly, and records only the final result on Cardano.",
    }),
  ],
  buttons: [
    {
      url: "/docs",
      label: translate({
        id: "homepage.hero.buttonOne",
        message: "Open your first head",
      }),
    },
    {
      url: "/docs",
      label: translate({
        id: "homepage.hero.buttonTwo",
        message: "Read the Docs",
      }),
    },
  ],
  stats: [
    {
      title: translate({
        id: "homepage.hero.stat.transaction.title",
        message: "< 1s",
      }),
      text: translate({
        id: "homepage.hero.stat.transaction.text",
        message: "To confirm a transaction",
      }),
    },
    {
      title: translate({
        id: "homepage.hero.stat.fees.title",
        message: "Zero fees",
      }),
      text: translate({
        id: "homepage.hero.stat.fees.text",
        message: "Or whatever you set",
      }),
    },
    {
      title: translate({
        id: "homepage.hero.stat.smart.title",
        message: "1:1",
      }),
      text: translate({
        id: "homepage.hero.stat.smart.text",
        message: "Same smart contracts as Cardano",
      }),
    },
  ],
};
