# Development workflow

- You can launch `cabal repl <project>` to make quick recompilations in a separate process while working on features

# Testing

- If making same-package changes, use `cabal repl <package>:test:tests` to load the test package and run the tests through `:main -p  "/<test name>/"`
- If making cross-package changes, use `just test <package> "<test name>"`; i.e. `just test hydra-tx "hashing"`
  - :reload won't pick up dep changes (it silently keeps the old compiled dep) so tests can falsely pass. Restart the repl, or use just test.

# Finishing code changes

- Always run `just lint` and iterate on fixes after finishing code changes
- Run `just check` as a final step; it's resource-intensive so prefer to do as few times as possible
  - `just check` runs six of the eight test-suites as nix flake checks, so an
    unchanged package's suite is skipped rather than re-run. hydra-cluster and
    hydra-tui have no flake check, so use `just test hydra-cluster` /
    `just test hydra-tui` for those.
- Make sure any documentation is also updated; you can look in `docs/` to find it
- When you make a change to the HeadLogic, or other sensitive areas, please make sure the spec and the Agda are also updated.
