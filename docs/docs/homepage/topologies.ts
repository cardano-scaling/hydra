import { translate } from "@docusaurus/Translate";

export const TopologiesContent = {
  label: translate({
    id: "homepage.topologies.label",
    message: "Topologies",
  }),
  title: translate({
    id: "homepage.topologies.title",
    message: "Two ways to run a head.",
  }),
  description: translate({
    id: "homepage.topologies.description",
    message:
      "The deciding question is not throughput. It is who runs the infrastructure.",
  }),
  topologies: [
    {
      label: translate({
        id: "homepage.topologies.direct.label",
        message: "Direct",
      }),
      title: translate({
        id: "homepage.topologies.direct.title",
        message: "Your users are known partners.",
      }),
      description: translate({
        id: "homepage.topologies.direct.description",
        message:
          "You run the nodes, no operator trust. Every update needs all participant signatures. Fits trading, settlement and fixed groups.",
      }),
      button: {
        url: "/topologies/basic",
        label: translate({
          id: "homepage.topologies.direct.button",
          message: "Explore direct topology",
        }),
      },
      signers: translate({
        id: "homepage.topologies.direct.signers",
        message: "All operators sign",
      }),
      allSign: true,
      image: "/img/topologies-direct.png",
      imageAlt: translate({
        id: "homepage.topologies.direct.imageAlt",
        message:
          "Three operators, each also a user, connected in one Hydra Head",
      }),
      settlement: translate({
        id: "homepage.topologies.direct.settlement",
        message: "Base chain settlement",
      }),
    },
    {
      label: translate({
        id: "homepage.topologies.delegated.label",
        message: "Delegated",
      }),
      title: translate({
        id: "homepage.topologies.delegated.title",
        message: "Your users are the public.",
      }),
      description: translate({
        id: "homepage.topologies.delegated.description",
        message:
          "Operators run the head; your users keep their own keys. Funds stay safe while at least one operator is honest.",
      }),
      button: {
        url: "/topologies/delegated-head",
        label: translate({
          id: "homepage.topologies.delegated.button",
          message: "Explore delegated topology",
        }),
      },
      signers: translate({
        id: "homepage.topologies.delegated.signers",
        message: "One operator signs",
      }),
      allSign: false,
      image: "/img/topologies-delegated.png",
      imageAlt: translate({
        id: "homepage.topologies.delegated.imageAlt",
        message:
          "Three operators running a Hydra Head, each serving several users",
      }),
      settlement: translate({
        id: "homepage.topologies.delegated.settlement",
        message: "Base chain settlement - One honest operator suffices",
      }),
    },
  ],
  shortButtonLabel: translate({
    id: "homepage.topologies.shortButtonLabel",
    message: "Find out more",
  }),
  link: {
    url: "/topologies",
    label: translate({
      id: "homepage.topologies.link",
      message: "Compare topologies",
    }),
  },
};
