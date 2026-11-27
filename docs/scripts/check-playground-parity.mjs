import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { loadPlayground } from "./compiled-playground.mjs";
import {
  exercisePlayground,
  exercisePayments,
  exerciseTokenLifecycle,
  rejectionRequests,
} from "./playground-scenarios.mjs";

const root = fileURLToPath(new URL("../..", import.meta.url));
const temporary = mkdtempSync(
  path.join(os.tmpdir(), "hydra-playground-parity-")
);
const runtime = await loadPlayground();
try {
  assert.ok(
    process.env.GHC_ENVIRONMENT,
    "Run this check with cabal exec -- node docs/scripts/check-playground-parity.mjs"
  );
  const packages = new Set([
    "base",
    "bytestring",
    "aeson",
    "containers",
    "crem",
    "text",
    "time",
    "hydra-prelude",
    "hydra-node",
    "hydra-tx",
  ]);
  const environment = readFileSync(process.env.GHC_ENVIRONMENT, "utf8")
    .split("\n")
    .filter((line) => {
      const packageName = line.match(/^package-id (.+?)-[0-9]/)?.[1];
      return !packageName || packages.has(packageName);
    })
    .join("\n");
  const envFile = path.join(temporary, "environment");
  writeFileSync(envFile, environment);
  const executable = path.join(temporary, "native");
  execFileSync(
    process.env.HYDRA_NATIVE_GHC || "ghc",
    [
      "--make",
      "-O1",
      "-package-env",
      envFile,
      ...[
        "GHC2021",
        "NoImplicitPrelude",
        "DataKinds",
        "DefaultSignatures",
        "DeriveAnyClass",
        "DerivingStrategies",
        "DuplicateRecordFields",
        "GADTs",
        "LambdaCase",
        "OverloadedStrings",
        "TypeFamilies",
      ].map((extension) => `-X${extension}`),
      "-idocs/runtime/src",
      "-outputdir",
      path.join(temporary, "build"),
      "-o",
      executable,
      "docs/runtime/src/Main.hs",
    ],
    { cwd: root, stdio: "pipe" }
  );
  const requests = [
    ...exercisePlayground(runtime.evaluate),
    ...exercisePayments(runtime.evaluate),
    ...exerciseTokenLifecycle(runtime.evaluate),
    ...rejectionRequests(),
    { diagnostic: "witnesses" },
  ];
  const native = execFileSync(executable, [], {
    input: requests.map(JSON.stringify).join("\n") + "\n",
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
  })
    .trim()
    .split("\n")
    .map(JSON.parse);
  let executions = 0;
  for (const [i, request] of requests.entries()) {
    const actual = runtime.evaluate(request);
    assert.deepEqual(
      actual,
      native[i],
      `native/JavaScript mismatch in scenario ${i}`
    );
    executions += actual.audit?.length || 0;
  }
  console.log(
    `Compiled JavaScript matches native Hydra for ${requests.length} scenarios and ${executions} intermediate node executions (states, events, effects and decision traces).`
  );
} finally {
  runtime.close();
  rmSync(temporary, { recursive: true, force: true });
}
