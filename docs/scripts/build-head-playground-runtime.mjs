import { execFileSync } from "node:child_process";
import { copyFileSync, mkdirSync, rmSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";

const docs = fileURLToPath(new URL("..", import.meta.url));
const output = execFileSync(
  "nix",
  [
    "build",
    "--print-build-logs",
    "--impure",
    "--file",
    path.join(docs, "runtime/default.nix"),
    "--no-link",
    "--print-out-paths",
  ],
  { cwd: docs, encoding: "utf8", stdio: ["inherit", "pipe", "inherit"] }
).trim();
const destination = path.join(docs, "static/head-playground-runtime/compiled");
mkdirSync(destination, { recursive: true });
const bundle = path.join(destination, "all.js");
// Nix's read-only outputs need a fresh destination on subsequent builds.
rmSync(bundle, { force: true });
copyFileSync(
  path.join(output, "bin/head-playground-runtime.jsexe/all.js"),
  bundle
);
console.log("Crem runtime ready at /head-playground-runtime/compiled/all.js");
