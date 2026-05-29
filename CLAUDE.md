# Development workflow

- You can launch `cabal repl <project>` to make quick recompilations in a separate process while working on features
- Always run `just lint` after finishing code changes
- Run `just check` as a final step; it's resource-intensive so prefer to do as few times as possible
- Whenever a code change is complete, make sure any documentation is also updated; you can look in `docs/` to find it

# Testing

- Run `just test <package> "<test name pattern>"`; i.e. `just test hydra-tx "hashing"`
