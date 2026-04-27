# Hydra Development Guidelines

Hydra is a Nix-based Haskell project. Activate the dev shell with `nix develop` (or set up `direnv` with `direnv allow`) before running any `cabal` command.

## Build verification

Always build with warnings as errors:

```
cabal build all --ghc-options="-Werror"
```

## Formatting

Run `nix fmt` after any code changes, before committing.

## Testing

- **TDD**: write a failing test before implementing. Align on approach (or enter plan mode, if your AI tool supports it) before writing code.
- **Running tests with an AI assistant**: the assistant should ask whether to run the test suite itself or stop at the build step and let the contributor run tests. Different contributors prefer different defaults — confirm before assuming.

## Project orientation

- `hydra-node/src/Hydra/HeadLogic.hs` — off-chain protocol source of truth (pure event-sourced state machine)
- State / events / errors / inputs: `HeadLogic/State.hs`, `HeadLogic/Outcome.hs`, `HeadLogic/Error.hs`, `HeadLogic/Input.hs`
- Head lifecycle: Idle → Initial → Open → Closed → Idle

## Contributing — AI assistant usage

This repo ships project-level Claude Code skills under `.claude/skills/`. They load automatically when their triggers match.

- **`hydra-log-analysis`** — diagnose hydra-node logs ("why did snapshots stop confirming?", multi-node correlation, journalctl-formatted docker logs)
- **`cardano-haskell-developer`** — auto-activates for Cardano/Hydra Haskell work; covers state-machine debugging, log analysis, CBOR/Plutus decoding
- **`grill-me`** — interactive design Q&A; useful for stress-testing a plan before implementing

To inspect a skill, read `.claude/skills/<name>/SKILL.md`. To trigger one explicitly, mention it by name to your AI assistant.

If you add a new skill that benefits the team, put it under `.claude/skills/` so others pick it up automatically.
