#!/usr/bin/env -S bash -e

set -euo pipefail

cd "$(dirname "$0")"

main() {
  local operation=prepare
  if [[ ${1:-} == prepare || ${1:-} == publish ]]; then
    operation=$1
    shift
  fi
  [[ $# == 1 ]] || usage "Expected a version number"
  local version=$1
  check_version_format "$version"
  check_clean_worktree
  local branch
  branch=$(git symbolic-ref --quiet --short HEAD) || exit_err "Check out a branch before releasing"

  # Validate remote state before changing files or creating a commit/tag.
  git fetch origin --tags
  check_fast_forward "refs/remotes/origin/$branch" HEAD
  if [[ $branch == master ]]; then
    check_fast_forward refs/remotes/origin/release HEAD
  fi

  case $operation in
    prepare)
      check_can_release "$version"
      prepare_release "$version"
      err "Prepared the release commit and signed tag. Review them, then publish with:"
      err "nix develop --command ./release.sh publish $version"
      ;;
    publish)
      publish_release "$version" "$branch"
      ;;
  esac
}

# Like 'echo' but on stderr
err() {
  echo >&2 "$1"
}

# Print error and exit with status 1
exit_err() {
  err "$1"
  exit 1
}

usage() {
  local message=$1

  cat >&2 <<EOF
$message

  $0 [prepare] <version>
  $0 publish <version>

Prepare creates a release commit and signed tag locally for review.
Publish atomically pushes the reviewed commit, tag, and (on master) release branch.
<version> must respect [Semantic Versioning](http://semver.org/)
EOF

  exit 1
}

check_can_release() {
  local version="$1"

  check_version_is_valid "$version"
  check_changelog_is_up_to_date "$version"
  check_networks_is_up_to_date "$version"
  jq -e --arg version "$version" \
    '.scriptHashes[$version] | strings | select(length > 0)' \
    hydra-chain-observer/script-hashes.json >/dev/null ||
    exit_err "Missing head script hash for $version"
  jq -e --arg version "$version" \
    '.depositScriptHashes[$version] | strings | select(length > 0)' \
    hydra-chain-observer/script-hashes.json >/dev/null ||
    exit_err "Missing deposit script hash for $version"

  true #avoid error on last instruction of function (see bash -e)
}

prepare_release() {
  local version="$1"

  update_cabal_version "$version"
  update_api_version "$version"
  update_demo_version "$version"
  update_tutorial_version "$version"

  git add -- hydra-*/*.cabal hydra-node/json-schemas/api.yaml \
    docs/docs/tutorial/index.md demo/docker-compose.yaml demo/seed-devnet.sh

  git commit -m "Release $version"

  git tag -as "$version" -F <(changelog "$version")

  # The local release branch may be stale, absent, or checked out in another
  # worktree. Publication updates the remote branch directly from this tag.
}

publish_release() {
  local version="$1"
  local branch="$2"
  local commit
  commit=$(git rev-parse --verify "refs/tags/$version^{commit}") ||
    exit_err "Missing release tag $version; prepare the release first"
  [[ $(git cat-file -t "refs/tags/$version") == tag ]] ||
    exit_err "Release $version must use an annotated tag"
  [[ $(git rev-parse HEAD) == "$commit" ]] ||
    exit_err "HEAD must be the reviewed release commit tagged $version"

  local refs=("$commit:refs/heads/$branch" "refs/tags/$version:refs/tags/$version")
  if [[ $branch == master ]]; then
    refs+=("$commit:refs/heads/release")
  fi
  # Fail as a whole if any ref is rejected; never fall back to separate pushes.
  git push --atomic origin "${refs[@]}"
  err "Pushed the Git refs for $version. Create/review the GitHub release and attach its artifacts."
  err "Publishing the latest stable GitHub release starts docs; watch the Publish Docs workflow."
}

# Checking helper functions

check_clean_worktree() {
  if [[ -n $(git status --porcelain) ]]; then
    exit_err "Commit or stash changes before releasing (including changelog and network updates)"
  fi
}

check_fast_forward() {
  local source=$1 target=$2
  if git show-ref --verify --quiet "$source"; then
    git merge-base --is-ancestor "$source" "$target" ||
      exit_err "$source cannot fast-forward to $target; reconcile the branch before releasing"
  fi
}

check_version_format() {
  local version=$1
  [[ $version =~ ^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$ ]] ||
    exit_err "Invalid format for version: $version"
}

check_version_is_valid() {
  local version=$1
  git show-ref --verify --quiet "refs/tags/$version" &&
    exit_err "A tag for this version already exists"

  true #avoid error on last instruction of function (see bash -e)
}

check_changelog_is_up_to_date() {
  local version="$1"

  local next_release
  next_release=$(sed -n '/^## *\[/{p;q;}' CHANGELOG.md)

  [[ $next_release == "## [$version]"* ]] ||
    exit_err "$version is not the next release in CHANGELOG.md"

  [[ $next_release == *UNRELEASED* ]] &&
    exit_err "$version is not released in CHANGELOG.md. Please replace UNRELEASED with the current date"

  true #avoid error on last instruction of function (see bash -e)
}

# Check whether a transaction id is present for all networks and given version.
check_networks_is_up_to_date() {
  local version="$1"

  local networks=(
    mainnet
    preprod
    preview
  )

  local networks_file=hydra-node/networks.json

  for network in "${networks[@]}"; do
    jq -e --arg network "$network" --arg version "$version" \
      '.[$network][$version] | strings | select(length > 0)' "$networks_file" >/dev/null ||
      exit_err "Missing transaction id for ${version} on ${network} in ${networks_file}"
  done
}

# Prepare helper functions

# Avoid the incompatible GNU/BSD sed -i options and broad backup-file cleanup.
replace_in_file() {
  local replacement_file
  replacement_file=$(mktemp)
  sed "$1" "$2" > "$replacement_file"
  cat "$replacement_file" > "$2"
  rm "$replacement_file"
}

update_cabal_version() {
  local version="$1"
  shift
  local cabal_files=hydra-*/*.cabal

  for file in $cabal_files; do
    replace_in_file "s,\(^version: *\)[^ ]*,\1$version," "$file"
  done
}

update_api_version() {
  local version="$1"
  shift
  local api_file=hydra-node/json-schemas/api.yaml

  replace_in_file "s,\(version: *\)'.*',\1'$version'," "$api_file"
}

update_tutorial_version() {
  local version="$1"
  local tutorial_file=docs/docs/tutorial/index.md
  replace_in_file "s,\(hydra_version=\).*,\1$version," "$tutorial_file"
}

update_demo_version() {
  local version="$1"
  (
    cd demo
    for file in docker-compose.yaml seed-devnet.sh; do
      replace_in_file "s,\(ghcr.io/cardano-scaling/hydra-[^:]*\):[^[:space:]]*,\1:$version," "$file"
    done
  )
}

changelog() {
  local version="$1"
  sed -e '/^## *\['"$version"'\]/ , /^## *\[.*\]/ !d' CHANGELOG.md | sed '$d' | sed -e 's/^\#* //'
}

main "$@"
