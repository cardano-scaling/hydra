import { translate } from "@docusaurus/Translate";
import MasumiLogo from "../../src/components/icons/MasumiLogo";
import EkklesiaLogo from "../../src/components/icons/EkklesiaLogo";
import GlacierDropLogo from "../../src/components/icons/GlacierDropLogo";
import DripDropzLogo from "../../src/components/icons/DripDropzLogo";
import MidgardLogo from "../../src/components/icons/MidgardLogo";
import DeltaDefiLogo from "../../src/components/icons/DeltaDefiLogo";

export const FastAndFrequentActivityContent = {
  label: translate({
    id: "homepage.fastAndFrequentActivity.label",
    message: "Fast and frequent activity",
  }),
  title: translate({
    id: "homepage.fastAndFrequentActivity.title",
    message: "Running on Hydra.",
  }),
  description: translate({
    id: "homepage.fastAndFrequentActivity.description",
    message: "Proven in production on Cardano mainnet.",
  }),
  projects: [
    {
      logo: <MasumiLogo />,
      title: translate({
        id: "homepage.fastAndFrequentActivity.masumi.title",
        message: "Masumi / Sokosumi",
      }),
      description: translate({
        id: "homepage.fastAndFrequentActivity.masumi.description",
        message:
          "Payment rail for AI agents: machine-to-machine micropayments at near-zero fees",
      }),
    },
    {
      logo: <EkklesiaLogo />,
      title: translate({
        id: "homepage.fastAndFrequentActivity.ekklesia.title",
        message: "Ekklesia",
      }),
      description: translate({
        id: "homepage.fastAndFrequentActivity.ekklesia.description",
        message:
          "High-frequency governance voting off-chain with an on-chain verifiable tally",
      }),
    },
    {
      logo: <GlacierDropLogo />,
      title: translate({
        id: "homepage.fastAndFrequentActivity.glacierDrop.title",
        message: "Glacier Drop",
      }),
      description: translate({
        id: "homepage.fastAndFrequentActivity.glacierDrop.description",
        message:
          "Mainnet, multi-operator token distribution at scale, with published benchmarks",
      }),
    },
    {
      logo: <DripDropzLogo />,
      title: translate({
        id: "homepage.fastAndFrequentActivity.dripDropz.title",
        message: "DripDropz",
      }),
      description: translate({
        id: "homepage.fastAndFrequentActivity.dripDropz.description",
        message:
          "Zero-fee vending at live events (Rare Evo, TOKEN2049) through a standard wallet",
      }),
    },
    {
      logo: <MidgardLogo />,
      title: translate({
        id: "homepage.fastAndFrequentActivity.midgard.title",
        message: "Midgard",
      }),
      description: translate({
        id: "homepage.fastAndFrequentActivity.midgard.description",
        message:
          "Fast-withdrawal liquidity rail between another Layer 2 and Cardano",
      }),
    },
    {
      logo: <DeltaDefiLogo />,
      title: translate({
        id: "homepage.fastAndFrequentActivity.deltaDefi.title",
        message: "Delta DeFi",
      }),
      description: translate({
        id: "homepage.fastAndFrequentActivity.deltaDefi.description",
        message:
          "Real-time perpetuals trading; order flow in the head, settlement on L1",
      }),
    },
  ],
};
