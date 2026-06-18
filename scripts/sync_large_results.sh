#!/usr/bin/env bash
set -euo pipefail

REMOTE_HOST="${REMOTE_HOST:-dell}"
REMOTE_ROOT="${REMOTE_ROOT:-/work/users/chaoliu/Test/20250320-mlr3-test}"
DIRECTION="${DIRECTION:-push}"
APPLY="${APPLY:-0}"
MODE="${MODE:-ignore-existing}"

if [[ -n "${RESULT_PATHS:-}" ]]; then
  # shellcheck disable=SC2206
  paths=( $RESULT_PATHS )
else
  paths=()
  while IFS= read -r path; do
    paths+=("$path")
  done < <(find clients -mindepth 2 -maxdepth 2 -type d -name results | sort)
fi

if [[ ${#paths[@]} -eq 0 ]]; then
  echo "No result paths found. Set RESULT_PATHS explicitly or create clients/<client>/results." >&2
  exit 2
fi

if [[ "$APPLY" == "1" || "$APPLY" == "true" || "$APPLY" == "yes" ]]; then
  dry_run=()
else
  dry_run=(--dry-run)
fi

case "$MODE" in
  update)
    mode_flags=()
    ;;
  ignore-existing)
    mode_flags=(--ignore-existing)
    ;;
  checksum)
    mode_flags=(--checksum)
    ;;
  *)
    echo "Unsupported MODE=$MODE. Use update, ignore-existing, or checksum." >&2
    exit 2
    ;;
esac

case "$DIRECTION" in
  push|pull)
    ;;
  *)
    echo "Unsupported DIRECTION=$DIRECTION. Use push or pull." >&2
    exit 2
    ;;
esac

echo "Large result sync"
echo "Remote: ${REMOTE_HOST}:${REMOTE_ROOT}"
echo "Direction: $DIRECTION"
echo "Mode: $MODE"
if [[ ${#dry_run[@]} -gt 0 ]]; then
  echo "Apply: no, dry-run only. Set APPLY=1 to copy files."
else
  echo "Apply: yes."
fi
echo "Delete: disabled. This script never passes --delete."
echo

for rel in "${paths[@]}"; do
  rel="${rel#/}"
  rel="${rel%/}"
  echo "== $rel =="
  if [[ "$DIRECTION" == "push" ]]; then
    if [[ ! -e "$rel" ]]; then
      echo "SKIP missing local path: $rel"
      echo
      continue
    fi
    rsync -avh --stats \
      "${dry_run[@]}" "${mode_flags[@]}" \
      --exclude='.DS_Store' \
      -R "$rel/" "${REMOTE_HOST}:${REMOTE_ROOT}/"
  else
    if ! ssh "$REMOTE_HOST" "test -e '$REMOTE_ROOT/$rel'"; then
      echo "SKIP missing remote path: ${REMOTE_ROOT}/${rel}"
      echo
      continue
    fi
    rsync -avh --stats \
      "${dry_run[@]}" "${mode_flags[@]}" \
      --exclude='.DS_Store' \
      "${REMOTE_HOST}:${REMOTE_ROOT}/${rel}/" "$rel/"
  fi
  echo
done
