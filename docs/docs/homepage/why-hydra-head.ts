import { translate } from "@docusaurus/Translate";

export const WhyHydraHeadContent = {
  label: translate({
    id: "homepage.whyHydraHead.label",
    message: "Why Hydra Head",
  }),
  title: translate({
    id: "homepage.whyHydraHead.title",
    message: "Hydra for performance applications.",
  }),
  paragraphs: [
    translate({
      id: "homepage.whyHydraHead.paragraphOne",
      message:
        "Blockchains like Cardano are secure because every transaction is checked by the whole network, but that takes time. A Hydra Head lets a group of people transact directly with each other, instantly, and records only the final result on Cardano.",
    }),
    translate({
      id: "homepage.whyHydraHead.paragraphTwo",
      message:
        "Hydra is isomorphic: a head runs the same ledger rules and smart contracts as Cardano, so your contracts work unchanged.",
    }),
  ],
  table: {
    columns: [
      translate({
        id: "homepage.whyHydraHead.table.property",
        message: "Property",
      }),
      translate({
        id: "homepage.whyHydraHead.table.mainchain",
        message: "Cardano Mainchain",
      }),
      translate({
        id: "homepage.whyHydraHead.table.head",
        message: "Inside a Hydra Head",
      }),
    ],
    rows: [
      {
        property: translate({
          id: "homepage.whyHydraHead.table.finality",
          message: "Finality",
        }),
        mainchain: translate({
          id: "homepage.whyHydraHead.table.finality.mainchain",
          message: "Mins to hours for finality",
        }),
        head: translate({
          id: "homepage.whyHydraHead.table.finality.head",
          message: "Under a second",
        }),
      },
      {
        property: translate({
          id: "homepage.whyHydraHead.table.cost",
          message: "Cost",
        }),
        mainchain: translate({
          id: "homepage.whyHydraHead.table.cost.mainchain",
          message: "A fee on every transaction",
        }),
        head: translate({
          id: "homepage.whyHydraHead.table.cost.head",
          message: "Set per head, down to zero",
        }),
      },
      {
        property: translate({
          id: "homepage.whyHydraHead.table.throughput",
          message: "Throughput",
        }),
        mainchain: translate({
          id: "homepage.whyHydraHead.table.throughput.mainchain",
          message: "Shared with the whole network",
        }),
        head: translate({
          id: "homepage.whyHydraHead.table.throughput.head",
          message: "Your participants' network and hardware",
        }),
      },
      {
        property: translate({
          id: "homepage.whyHydraHead.table.smartContracts",
          message: "Smart contracts",
        }),
        mainchain: translate({
          id: "homepage.whyHydraHead.table.smartContracts.mainchain",
          message: "Plutus and Aiken",
        }),
        head: translate({
          id: "homepage.whyHydraHead.table.smartContracts.head",
          message: "The same contracts, unchanged",
        }),
      },
      {
        property: translate({
          id: "homepage.whyHydraHead.table.security",
          message: "Security",
        }),
        mainchain: translate({
          id: "homepage.whyHydraHead.table.security.mainchain",
          message: "Cardano consensus",
        }),
        head: translate({
          id: "homepage.whyHydraHead.table.security.head",
          message: "Multi-sig (only one-honesty party needed for safety)",
        }),
      },
    ],
  },
};
