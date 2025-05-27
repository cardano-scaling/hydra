#!/usr/bin/env -S bash -e

set -eo pipefail

cd $(dirname $0)

main() {
  local version="$1"

  [[ -z "$version" ]] && usage "Missing version number"

  check_can_release "$version"

  prepare_release "$version"

  publish_release_instructions "$version"
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

  $0 <version>

Prepares a new release of hydra.
<version> must respect [Semantic Versioning](http://semver.org/)
EOF

  exit 1
}

check_can_release() {
  local version="$1"

  confirm_uncommitted_changes
  check_version_is_valid $version
  check_changelog_is_up_to_date $version
  check_networks_is_up_to_date $version

  true #avoid error on last instruction of function (see bash -e)
}

prepare_release() {
  local version="$1"

  update_cabal_version "$version"
  update_api_version "$version"
  update_demo_version "$version"
  update_tutorial_version "$version"

  find . -name '*-e' -exec rm '{}' \; # cleanup BSD sed mess

  git add .

  git commit -m "Release $version"

  git tag -as "$version" -F <(changelog "$version")

  if [ $(git rev-parse --abbrev-ref HEAD) = "master" ]; then
    # Make branch release point to tag so that the website is published
    git checkout release
    git merge "${version}" --ff-only
    git checkout master
  fi
}

publish_release_instructions() {
  local version="$1"
  local branch_name=$(git rev-parse --abbrev-ref HEAD)

  err "Prepared the release commit and tag, review it now and if everything is okay, push using:"
  err ""
  err "git push origin ${branch_name}"
  if [ ${branch_name} = "master" ]; then
    err "git push origin release"
  fi
  err "git push origin ${version}"
}

# Checking helper functions

confirm_uncommitted_changes() {
  if [ ! -z "$(git status --porcelain)" ]; then
    git status >&2
    echo >&2 "WARNING: You have unstaged changes. The release will stage everything and commit it."
    ask_continue
  fi
}

# Ask user whethery they want to continue, exit with error if not
ask_continue() {
  read -p "Do you want to continue? [y/n] " -n 1 -r
  echo >&2 ""
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    exit_err "Aborted release"
  fi
}

check_version_is_valid() {
  local version="$1"

  echo $version | grep -E '^[0-9]*\.[0-9]*\.[0-9]*$' >/dev/null ||
    exit_err "Invalid format for version: $version"

  git tag | grep "^$version$" >/dev/null &&
    exit_err "A tag for this version already exists"

  true #avoid error on last instruction of function (see bash -e)
}

check_changelog_is_up_to_date() {
  local version="$1"

  local next_release="$(sed '/## *\[.*\]/ !d' CHANGELOG.md | head -n1)"

  echo "$next_release" | grep "\[$version\]" >/dev/null ||
    exit_err "$version is not the next release in CHANGELOG.md"

  echo "$next_release" | grep UNRELEASED >/dev/null &&
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
    cat ${networks_file} | jq -e ".\"${network}\".\"${version}\"" >/dev/null ||
      exit_err "Missing transaction id for ${version} on ${network} in ${networks_file}"
  done
}

# Prepare helper functions

update_cabal_version() {
  local version="$1"
  shift
  local cabal_files=hydra-*/*.cabal

  for file in $cabal_files; do
    sed -i"" -e "s,\(^version: *\)[^ ]*,\1$version," $file
  done
}

update_api_version() {
  local version="$1"
  shift
  local api_file=hydra-node/json-schemas/api.yaml

  sed -i"" -e "s,\(version: *\)'.*',\1'$version'," $api_file
}

update_tutorial_version() {
  local version="$1"
  local tutorial_file=docs/docs/tutorial/index.md
  sed -i"" -e "s,\(hydra_version=\).*,\1$version," $tutorial_file
}

update_demo_version() {
  local version="$1"
  (
    cd demo
    sed -i"" -e "s,\(ghcr.io/cardano-scaling/hydra-[^:]*\):[^[:space:]]*,\1:$version," docker-compose.yaml seed-devnet.sh
  )
}

changelog() {
  local version="$1"
  sed -e '/^## *\['"$version"'\]/ , /^## *\[.*\]/ !d' CHANGELOG.md | sed '$d' | sed -e 's/^\#* //'
}

main "$@"
