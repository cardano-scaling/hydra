#!/usr/bin/env -S bash -e

cd $(dirname $0)

main() {
  local version="$1"
  local preview_txid="$2"
  local preprod_txid="$3"

  [[ -z "$version" ]]      && usage "Missing version number"
  [[ -z "$preview_txid" ]] && usage "Missing preview network hydra scripts publication transaction id"
  [[ -z "$preprod_txid" ]] && usage "Missing preprod network hydra scripts publication transaction id"

  check_can_release "$version"

  prepare_release "$version" "$preview_txid" "$preprod_txid"

  publish_release # fake for now
}

usage() {
  local message=$1

  cat >&2 <<EOF
$message

  $0 <version> <preview_txid> <preprod_txid>
Publishes a new release of hydra.
<version> must respect [Semantic Versioning](http://semver.org/)
<preview_txid> is the id of the transaction used to publish the hydra scripts on the preview network (see smoke tests)
<preprod_txid> is the id of the transaction used to publish the hydra scripts on the preprod network (see smoke tests)
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
  local preview_txid="$2"
  local preprod_txid="$3"

  update_cabal_version "$version"
  update_api_version   "$version"
  update_preview_txid "$version" "$preview_txid"
  update_preprod_txid "$version" "$preprod_txid"
  update_demo_version "$version"

  find . -name '*-e' -exec rm '{}' \; # cleanup BSD sed mess
  
  git add .
  
  git commit -m "Prepare release $version"

  git tag -as "$version" -F <(changelog "$version")
}

publish_release() {
  local version="$1"

  echo Prepared the release commit and tag, review it now and if everything is okay, push using: >&2:
  echo git push >&2
  echo git push ${version} >&2
  echo >&2
  echo And then you shall manually create the release page, see CONTRIBUTING.md >&2
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
    sed -i -e "s,\(^version: *\)[^ ]*,\1$version," $file
  done
}

update_api_version() {
  local version="$1" ; shift
  local api_file=hydra-node/json-schemas/api.yaml

  sed -i -e "s,\(version: *\)'.*',\1'$version'," $api_file
}

update_preview_txid() {
  local version="$1"
  local txid="$2"

  update_txid preview "$version" "$txid"
}

update_preprod_txid() {
  local version="$1"
  local txid="$2"

  update_txid preprod "$version" "$txid"
} 

update_txid() {
  local network="$1"
  local version="$2"
  local txid="$3"

  rm "testnets/${network}/hydra-scripts.txid"
  echo "$txid" > "testnets/${network}/hydra-scripts-${version}.txid"
  ln -s "hydra-scripts-${version}.txid" testnets/${network}/hydra-scripts.txid
}

update_demo_version() {
  local version="$1"
  (
    cd demo
    sed -i -e "s,\(ghcr.io/input-output-hk/hydra-[^:]*\):[^[:space:]]*,\1:$version," *
  )
}

changelog() {
  local version="$1"
  sed -e '/^## *\['"$version"'\]/ , /^## *\[.*\]/ !d' CHANGELOG.md | sed '$d' | sed -e 's:^#* *::'
}

main "$@"
