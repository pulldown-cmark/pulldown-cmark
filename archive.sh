#!/bin/bash
version=$(cat version.txt)
if [[ "${version}" == "" ]]; then
  echo "cannot get version tag from HEAD commit"
  exit 1
fi
bin=$(basename "$(git rev-parse --show-toplevel)")
target="$*"

if [[ ! -d "release" ]]; then
 mkdir "release"
fi

for target in "$@"; do
  archive="${bin}_${version}_${target}"
  echo "## make archive ${archive}"
  if [[ "$target" == *"windows"* ]]; then
    ( cd "target/${target}/release/" && zip "../../../release/${archive}.zip" "${bin}.exe" )
  else
    ( cd "target/${target}/release/" && tar -Jcvf "../../../release/${archive}.tar.xz" "${bin}" )
  fi
done
