import { translate } from "@docusaurus/Translate";

export const WhyHydraHeadContent = {
  title: translate({
    id: "homepage.whyHydraHead.title",
    message: "Why Hydra Head?",
  }),
  descriptionParagraphOne: translate({
    id: "homepage.featureList.lowLatency.description.paragraphOne",
    message: `Traditional blockchain systems face scalability limitations due to
      the trade-off between decentralization, security, and scalability
      (the blockchain trilemma). Cardano's consensus algorithm, while
      efficient, still requires massive global replication of state
      changes, potentially increasing transaction settlement times during
      peak hours.`,
  }),
  descriptionParagraphTwo: translate({
    id: "homepage.featureList.lowLatency.description.paragraphTwo",
    message: `The Hydra Head protocol aims to enhance the flexibility of
      decentralization levels on a case-by-case basis. Not every
      transaction requires global consensus to be considered final.
      For instance, buying a croissant or lending money to a friend
      doesn’t need the involvement of a central bank. Many
      transactions and arrangements can occur within a single Hydra
      head, with only the final outcome recorded on the main chain.`,
  }),
  descriptionParagraphThree: translate({
    id: "homepage.featureList.lowLatency.description.paragraphThree",
    message: `The protocol facilitates frictionless state transfer between the
      main chain and individual heads, bypassing the 20-second block
      time limit. This enables state evolution at a pace approved by
      involved parties, independent of blockchain constraints.`,
  }),
};
