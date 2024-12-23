#!/usr/bin/env bash
set -euo pipefail

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

_tc() {
  local name=$1
  shift

  printf ':: %s ' "$name"

  aws s3 sync --dryrun "${@}" | sort >"$tmp"/aws.txt
  stack exec -- amazonka-s3-sync --dryrun "${@}" | sort >"$tmp"/amazonka.txt

  if diff -U 3 "$tmp"/{aws,amazonka}.txt >"$tmp"/output.diff; then
    echo "✓"
  else
    echo "✗"
    echo " +- Diff ==="
    sed 's/^/ | /' "$tmp"/output.diff
    echo " +- Diff ==="
  fi

  rm "$tmp"/*
}

_tc "local-remote" src/ s3://files.pbrisbin.com/docs/
_tc "local-remote w/ delete" --delete src/ s3://files.pbrisbin.com/docs/
_tc "local-remote size-only" --size-only src/ s3://files.pbrisbin.com/docs/
_tc "remote-local" s3://files.pbrisbin.com/docs/ src/
_tc "remote-local w/ delete" --delete s3://files.pbrisbin.com/docs/ src/
_tc "remote-local size-only" --size-only s3://files.pbrisbin.com/docs/ src/
_tc "remote-remote" s3://files.pbrisbin.com/{docs,music}/
_tc "remote-remote w/ delete" --delete s3://files.pbrisbin.com/{docs,music}/
_tc "remote-remote size-only" --size-only s3://files.pbrisbin.com/{docs,music}/
_tc "remote-remote reversed" s3://files.pbrisbin.com/{music,docs}/
_tc "remote-remote reversed w/ delete" --delete s3://files.pbrisbin.com/{music,docs}/
_tc "remote-remote reversed size-only" --size-only s3://files.pbrisbin.com/{music,docs}/
