[private]
default:
  @just --list

# run 'selfci'
ci:
  selfci check --print-output

