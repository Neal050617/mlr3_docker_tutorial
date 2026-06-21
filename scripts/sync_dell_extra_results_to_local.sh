#!/usr/bin/env bash
set -euo pipefail

REMOTE_HOST="${REMOTE_HOST:-dell}"
REMOTE_ROOT="${REMOTE_ROOT:-/work/users/chaoliu/Test/20250320-mlr3-test}"
APPLY="${APPLY:-0}"
MAX_SIZE="${MAX_SIZE:-}"

if [[ "$APPLY" == "1" || "$APPLY" == "true" || "$APPLY" == "yes" ]]; then
  dry_run=()
else
  dry_run=(--dry-run)
fi

size_args=()
if [[ -n "$MAX_SIZE" ]]; then
  size_args=(--max-size="$MAX_SIZE")
fi

sources=(
  "${REMOTE_HOST}:${REMOTE_ROOT}/./mlr3_docker_tutorial/"
  "${REMOTE_HOST}:${REMOTE_ROOT}/./客户分析结果展示/"
  "${REMOTE_HOST}:${REMOTE_ROOT}/./lilei/"
  "${REMOTE_HOST}:${REMOTE_ROOT}/./masslin3_test/"
  "${REMOTE_HOST}:${REMOTE_ROOT}/./MOBIOR_V0.0.6_UPGRADE_PLAN.md"
  "${REMOTE_HOST}:${REMOTE_ROOT}/./metabo_R_packagess.md"
  "${REMOTE_HOST}:${REMOTE_ROOT}/./metabo_database_download.md"
)

echo "Sync direction: ${REMOTE_HOST}:${REMOTE_ROOT} -> $(pwd)"
if [[ ${#dry_run[@]} -gt 0 ]]; then
  echo "Mode: dry-run. Set APPLY=1 to copy files."
else
  echo "Mode: apply."
fi
if [[ -n "$MAX_SIZE" ]]; then
  echo "Max file size: ${MAX_SIZE}"
fi

rsync -avR --stats --partial --partial-dir=.rsync-partial \
  "${dry_run[@]}" \
  "${size_args[@]}" \
  --exclude='/.git/' \
  --exclude='*/.git/' \
  --exclude='R_package_mobio/' \
  --exclude='R_packages_mobio/' \
  --exclude='chaoliu_R_package_mobio/' \
  --exclude='lvyan_R_package_mobio/' \
  --exclude='*/site-library/' \
  --exclude='*/library/' \
  --exclude='mobior*.tar' \
  --exclude='mobior*.tar.*' \
  --exclude='.rsync-partial/' \
  "${sources[@]}" \
  ./
