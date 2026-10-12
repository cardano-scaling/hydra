import { translate } from "@docusaurus/Translate";
import Thunder from "../../src/components/icons/Thunder";
import AgentsLight from "../../src/components/icons/AgentsLight";
import AgentsDark from "../../src/components/icons/AgentsDark";
import GovLight from "../../src/components/icons/GovLight";
import GovDark from "../../src/components/icons/GovDark";
import PaymentsLight from "../../src/components/icons/PaymentsLight";
import PaymentsDark from "../../src/components/icons/PaymentsDark";
import CandlesLight from "../../src/components/icons/CandlesLight";
import CandlesDark from "../../src/components/icons/CandlesDark";
import PredictionsLight from "../../src/components/icons/PredictionsLight";
import PredictionsDark from "../../src/components/icons/PredictionsDark";
import HouseLight from "../../src/components/icons/HouseLight";
import HouseDark from "../../src/components/icons/HouseDark";
import ControllerLight from "../../src/components/icons/ControllerLight";
import ControllerDark from "../../src/components/icons/ControllerDark";

export const UseCasesContent = {
  label: translate({
    id: "homepage.useCases.label",
    message: "Use cases",
  }),
  title: translate({
    id: "homepage.useCases.title",
    message: "What Hydra unlocks",
  }),
  text: translate({
    id: "homepage.useCases.text",
    message: "For apps held back by slow finality, fees or throughput.",
  }),
  items: [
    {
      icon: <AgentsLight />,
      iconHover: <AgentsDark />,
      title: translate({
        id: "homepage.useCases.item.agent.title",
        message: "Agent-to-agent commerce",
      }),
      label: translate({
        id: "homepage.useCases.item.agent.label",
        message: "Live / Masumi",
      }),
      labelIcon: <Thunder />,
      text: translate({
        id: "homepage.useCases.item.agent.text",
        message:
          "AI agents pay each other per task, instantly and for near-zero fees, with dispute windows and refunds built in.",
      }),
    },
    {
      icon: <GovLight />,
      iconHover: <GovDark />,
      title: translate({
        id: "homepage.useCases.item.governance.title",
        message: "Governance and voting",
      }),
      label: translate({
        id: "homepage.useCases.item.governance.label",
        message: "Live / Intersect",
      }),
      labelIcon: <Thunder />,
      text: translate({
        id: "homepage.useCases.item.governance.text",
        message:
          "High-frequency voting and tallies with a signed audit trail, settled on Cardano when the vote closes.",
      }),
    },
    {
      icon: <PaymentsLight />,
      iconHover: <PaymentsDark />,
      title: translate({
        id: "homepage.useCases.item.payments.title",
        message: "Payments and micropayments",
      }),
      label: translate({
        id: "homepage.useCases.item.payments.label",
        message: "Demo - Vending Machine",
      }),
      labelIcon: null,
      text: translate({
        id: "homepage.useCases.item.payments.text",
        message:
          "Point of sale, pay-per-call APIs and usage metering. Many small payments, one settlement on Cardano.",
      }),
    },
    {
      icon: <CandlesLight />,
      iconHover: <CandlesDark />,
      title: translate({
        id: "homepage.useCases.item.trading.title",
        message: "Trading and DeFi",
      }),
      label: translate({
        id: "homepage.useCases.item.trading.label",
        message: "Pattern",
      }),
      labelIcon: null,
      text: translate({
        id: "homepage.useCases.item.trading.text",
        message:
          "Order matching, arbitrage and liquidations at sub-second speed, with Cardano as the settlement backstop.",
      }),
    },
    {
      icon: <PredictionsLight />,
      iconHover: <PredictionsDark />,
      title: translate({
        id: "homepage.useCases.item.prediction.title",
        message: "Prediction markets",
      }),
      label: translate({
        id: "homepage.useCases.item.prediction.label",
        message: "Pattern",
      }),
      labelIcon: null,
      text: translate({
        id: "homepage.useCases.item.prediction.text",
        message:
          "Bids and trades between known participants confirm instantly. Results are committed to Cardano.",
      }),
    },
    {
      icon: <HouseLight />,
      iconHover: <HouseDark />,
      title: translate({
        id: "homepage.useCases.item.assets.title",
        message: "Real-world assets",
      }),
      label: translate({
        id: "homepage.useCases.item.assets.label",
        message: "Pattern",
      }),
      labelIcon: null,
      text: translate({
        id: "homepage.useCases.item.assets.text",
        message:
          "Regulated assets already depend on known, accountable parties. A head lets them transact quickly and settle on Cardano.",
      }),
    },
    {
      icon: <ControllerLight />,
      iconHover: <ControllerDark />,
      title: translate({
        id: "homepage.useCases.item.gaming.title",
        message: "Gaming",
      }),
      label: translate({
        id: "homepage.useCases.item.gaming.label",
        message: "Demo - Hydra doom",
      }),
      labelIcon: <Thunder />,
      text: translate({
        id: "homepage.useCases.item.gaming.text",
        message:
          "Every move is a confirmed transaction, and in-game economies run without a fee per move.",
      }),
    },
  ],
  cta: {
    label: translate({
      id: "homepage.useCases.cta.label",
      message: "Explore use cases",
    }),
    url: "/use-cases",
  },
};
