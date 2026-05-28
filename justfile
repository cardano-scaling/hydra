alias c := check
alias t := test
alias l := lint

[private]
default:
  @just --list

# run 'selfci'
ci:
  selfci check --print-output

# run "nix-fast-build" to run the nix checks, then produce HTML test reports
# under ./test-reports/ via the tasty-html ingredient wired into every Hydra
# test-suite. Reports are written per package so they don't overwrite each other.
check:
  nix-fast-build \
    --flake ".#checks.$(nix eval --impure --raw --expr builtins.currentSystem)" \
    --no-link \
    --skip-cached
  # nix-fast-build writes per-suite XML/HTML into each test derivation's nix
  # store path, not ./test-reports/. Only build a local index if some other
  # invocation ('just test') has already populated test-reports/.
  if ls test-reports/*.xml >/dev/null 2>&1; then just test-index; fi

# run cabal tests, optionally with a tasty pattern (see `--pattern`).
# `--keep-going` so failures in one test-suite don't hide failures in others;
# `{suite}` is substituted with the test-suite name by `runHydraTests`, so a
# single `just test` produces one HTML+XML report per package and an
# index.html that links them.
test PKG="all" PATTERN="":
  #!/usr/bin/env bash
  set -euo pipefail
  mkdir -p test-reports
  cabal test {{PKG}} \
    --keep-going \
    --test-options="--html=$(pwd)/test-reports/{suite}.html --xml=$(pwd)/test-reports/{suite}.xml" \
    {{ if PATTERN == "" { "" } else { "--test-options=--pattern=" + PATTERN } }}
  just test-index

# emit ./test-reports/index.html linking to per-suite HTML reports, with
# pass/fail/error counts pulled from the per-suite JUnit XML.
test-index:
  #!/usr/bin/env bash
  set -euo pipefail
  cd test-reports
  shopt -s nullglob
  xmls=(*.xml)
  if [ ${#xmls[@]} -eq 0 ]; then
    echo "no test-reports/*.xml found — run 'just test' first" >&2
    exit 1
  fi
  {
    echo '<!doctype html><meta charset="utf-8"><title>Hydra test reports</title>'
    echo '<style>'
    echo 'body{font-family:system-ui,sans-serif;max-width:50rem;margin:2rem auto;padding:0 1rem;color:#222}'
    echo 'h1{font-size:1.4rem}table{border-collapse:collapse;width:100%}'
    echo 'th,td{padding:.4rem .7rem;text-align:left;border-bottom:1px solid #e3e3e3}'
    echo 'th{font-weight:600;background:#f7f7f7}td.num{text-align:right;font-variant-numeric:tabular-nums}'
    echo 'tr.fail td:first-child::before{content:"✗ ";color:#b00}'
    echo 'tr.ok td:first-child::before{content:"✓ ";color:#080}'
    echo 'a{color:#06c;text-decoration:none}a:hover{text-decoration:underline}'
    echo '</style>'
    echo "<h1>Hydra test reports <small style=\"font-weight:normal;color:#777\">— generated $(date '+%Y-%m-%d %H:%M:%S')</small></h1>"
    echo '<table><thead><tr><th>Suite</th><th class="num">Tests</th><th class="num">Failed</th><th class="num">Errors</th><th>Generated</th></tr></thead><tbody>'
    for xml in "${xmls[@]}"; do
      suite="${xml%.xml}"
      html="${suite}.html"
      attrs=$(head -c 4096 "$xml" || true)
      tests=$(printf %s "$attrs" | grep -oE 'tests="[0-9]+"'    | head -1 | grep -oE '[0-9]+' || echo "?")
      fails=$(printf %s "$attrs" | grep -oE 'failures="[0-9]+"' | head -1 | grep -oE '[0-9]+' || echo "0")
      errs=$(printf %s  "$attrs" | grep -oE 'errors="[0-9]+"'   | head -1 | grep -oE '[0-9]+' || echo "0")
      mtime=$(date -r "$xml" '+%Y-%m-%d %H:%M:%S')
      cls="ok"
      if [ "${fails:-0}" != "0" ] || [ "${errs:-0}" != "0" ]; then cls="fail"; fi
      if [ -f "$html" ]; then link="<a href=\"$html\">$suite</a>"; else link="$suite"; fi
      echo "<tr class=\"$cls\"><td>$link</td><td class=\"num\">$tests</td><td class=\"num\">$fails</td><td class=\"num\">$errs</td><td>$mtime</td></tr>"
    done
    echo '</tbody></table>'
  } > index.html
  echo "wrote test-reports/index.html (${#xmls[@]} suites)"

# build with -Werror and strict linting flags.
lint PKG="all":
  cabal build {{PKG}} \
    --ghc-options="-Werror \
      -Wall \
      -Wcompat \
      -Widentities \
      -Wincomplete-record-updates \
      -Wincomplete-uni-patterns \
      -Wmissing-deriving-strategies \
      -Wredundant-constraints \
      -Wunused-packages"
