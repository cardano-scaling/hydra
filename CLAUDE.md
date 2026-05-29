# Development workflow

- You can launch `cabal repl <project>` to make quick recompilations in a separate process while working on features

# Testing

- If making same-package changes, use the `cabal repl <package>:test:tests` to load the test package and run the tests through `:main -p  "/<test name>/"`
- If making cross-package changes, you can use `just test <package> "<test name>"`; i.e. `just test hydra-tx "hashing"`

# Finishing code changes

- Always run `just lint` and iterate on fixes after finishing code changes
- Run `just test && just check` as a final step; it's resource-intensive so prefer to do as few times as possible
- Make sure any documentation is also updated; you can look in `docs/` to find it
