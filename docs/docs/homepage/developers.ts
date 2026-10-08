import { translate } from "@docusaurus/Translate";
import type { TerminalLine } from "../../src/components/homepage/components/Terminal";

const quickstartLines: TerminalLine[] = [
  { type: "comment", text: "# Get the demo" },
  {
    type: "command",
    text: "git clone --depth 1 git@github.com:cardano-scaling/hydra.git",
  },
  { type: "command", text: "cd hydra/demo" },
  { type: "command", text: "docker compose pull" },
  { type: "blank" },
  {
    type: "comment",
    text: "# Start a local Cardano devnet and fund the participants",
  },
  { type: "command", text: "./prepare-devnet.sh" },
  { type: "command", text: "docker compose up -d cardano-node" },
  { type: "command", text: "./seed-devnet.sh" },
  { type: "blank" },
  {
    type: "comment",
    text: "# Start three Hydra nodes (Alice, Bob and Carol) and open the TUI",
  },
  { type: "command", text: "docker compose up -d hydra-node-{1,2,3}" },
  { type: "command", text: "docker compose run hydra-tui-1" },
  { type: "blank" },
  { type: "output", text: "[ Hydra Heads are open ]", typewriter: true },
  { type: "blank" },
];

export const DevelopersContent = {
  label: translate({
    id: "homepage.developers.label",
    message: "Developers",
  }),
  title: {
    lineOne: translate({
      id: "homepage.developers.title.lineOne",
      message: "From zero to an open",
    }),
    beforeHighlight: translate({
      id: "homepage.developers.title.beforeHighlight",
      message: "head in",
    }),
    highlight: translate({
      id: "homepage.developers.title.highlight",
      message: "15",
    }),
    afterHighlight: translate({
      id: "homepage.developers.title.afterHighlight",
      message: "minutes.",
    }),
  },
  paragraphs: [
    translate({
      id: "homepage.developers.paragraphOne",
      message:
        "Start with a local devnet on one machine to learn the lifecycle.",
    }),
    translate({
      id: "homepage.developers.paragraphTwo",
      message:
        "Then move to production tooling: MeshJS, hydra-pay, and hydra-mcp.",
    }),
  ],
  buttons: [
    {
      url: "/docs/getting-started",
      label: translate({
        id: "homepage.developers.buttonOne",
        message: "Quickstart",
      }),
    },
    {
      url: "/docs",
      label: translate({
        id: "homepage.developers.buttonTwo",
        message: "Read the Docs",
      }),
    },
  ],
  terminal: {
    title: translate({
      id: "homepage.developers.terminal.title",
      message: "Quickstart",
    }),
    lines: quickstartLines,
  },
};
