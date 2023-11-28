#!/usr/bin/env -S bash -e

set -eo pipefail

cd $(dirname $0)

main() {
  local version="$1"

  [[ -z "$version" ]]      && usage "Missing version number"

  check_can_release "$version"

  prepare_release "$version"

  publish_release "$version" # fake for now
}

usage() {
  local message=$1

  cat >&2 <<EOF
$message

  $0 <version>
Publishes a new release of hydra.
<version> must respect [Semantic Versioning](http://semver.org/)
EOF

  exit 1
}

check_can_release() {
  local version="$1"

  check_no_uncommited_changes  
  check_version_is_valid $version
  check_changelog_is_up_to_date $version

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

  # Make branch release point to tag so that the website is published
  git checkout release
  git merge "${version}" --ff-only
}

publish_release() {
  local version="$1"

  >&2 echo Prepared the release commit and tag, review it now and if everything is okay, push using:
  >&2 echo git push origin master
  >&2 echo git push origin release
  >&2 echo git push origin ${version}
  >&2 echo
  >&2 echo And then you shall manually create the release page, see CONTRIBUTING.md
}

# Checking helper functions

check_no_uncommited_changes() {
  if [ ! -z "$(git status --porcelain)" ]
  then
    git status >&2
    echo >&2
    usage "Please commit your pending changes first"
  fi
}

check_version_is_valid() {
  local version="$1"
  
  echo $version | grep -E '^[0-9]*\.[0-9]*\.[0-9]*$' >/dev/null \
  || usage "Invalid format for version: $version"

  git tag | grep "^$version$" >/dev/null \
  && usage "A tag for this version already exists"

  true #avoid error on last instruction of function (see bash -e)
}

check_changelog_is_up_to_date() {
  local version="$1"

  local next_release="$(sed '/## *\[.*\]/ !d' CHANGELOG.md | head -n1)"

  echo "$next_release" | grep "\[$version\]" >/dev/null \
  || usage "$version is not the next release in CHANGELOG.md"

  echo "$next_release" | grep UNRELEASED >/dev/null \
  && usage "$version is not released in CHANGELOG.md. Please replace UNRELEASED with the current date"

  true #avoid error on last instruction of function (see bash -e)
}

# Prepare helper functions

update_cabal_version() {
  local version="$1" ; shift
  local cabal_files=hydra-*/*.cabal
  
  for file in $cabal_files
  do
    sed -i.bak -e "s,\(^version: *\)[^ ]*,\1$version," $file
  done
}

update_api_version() {
  local version="$1" ; shift
  local api_file=hydra-node/json-schemas/api.yaml

  sed -i.bak -e "s,\(version: *\)'.*',\1'$version'," $api_file
}

update_tutorial_version() {
  local version="$1"
  local tutorial_file=docs/docs/tutorial/index.md
  sed -i.bak -e "s,\(hydra/releases/download/\)[^/]*,\1$version," $tutorial_file
}

update_demo_version() {
  local version="$1"
  (
    cd demo
    sed -i.bak -e "s,\(ghcr.io/input-output-hk/hydra-[^:]*\):[^[:space:]]*,\1:$version," docker-compose.yaml seed-devnet.sh
  )
}

changelog() {
  local version="$1"
  sed -e '/^## *\['"$version"'\]/ , /^## *\[.*\]/ !d' CHANGELOG.md | sed '$d' | sed -e 's/^\#* //'
}

main "$@"
