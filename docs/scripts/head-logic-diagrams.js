// Regenerate the committed page so topology changes are visible in Git diffs.
// Nix supplies a compiled generator; local builds use the same Cabal executable.
const { spawnSync } = require("node:child_process");
const path = require("node:path");

const destination = path.resolve(__dirname, "../docs/dev/architecture/head-logic-machines.md");
const executable = process.env.HYDRA_HEAD_LOGIC_DIAGRAMS;
const command = executable || "cabal";
const args = executable
  ? [destination]
  : ["run", "hydra-node:head-logic-diagrams", "--", destination];
const result = spawnSync(command, args, {
  cwd: path.resolve(__dirname, "../.."),
  stdio: "inherit",
});
if (result.error) console.error(result.error.message);
process.exit(result.status === null ? 1 : result.status);
