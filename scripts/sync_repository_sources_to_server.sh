#!/usr/bin/env bash
set -euo pipefail

REMOTE_HOST="${REMOTE_HOST:-dell}"
REMOTE_ROOT="${REMOTE_ROOT:-/work/users/chaoliu/Test/20250320-mlr3-test}"
APPLY="${APPLY:-0}"

if [[ "$APPLY" == "1" || "$APPLY" == "true" || "$APPLY" == "yes" ]]; then
  DRY_RUN=()
else
  DRY_RUN=(--dry-run)
fi

file_list="$(mktemp)"
trap 'rm -f "$file_list"' EXIT

paths=(
  .gitignore
  README.md
  Dockerfile
  docker-compose.yml
  entrypoint.sh
  create_rstudio_modio_no_port.sh
  modeling_methods.md
  docs
  scripts
  skills
  clients
  mobior_v0.0.6
  resources
)

if [[ -n "${EXTRA_PATHS:-}" ]]; then
  # shellcheck disable=SC2206
  extra_paths=( $EXTRA_PATHS )
  paths+=("${extra_paths[@]}")
fi

if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  while IFS= read -r path; do
    [[ -f "$path" ]] && printf '%s\n' "$path"
  done < <(git ls-files --cached --others --exclude-standard -- "${paths[@]}") >> "$file_list"
else
  for path in "${paths[@]}"; do
    if [[ -f "$path" ]]; then
      printf '%s\n' "$path" >> "$file_list"
    elif [[ -d "$path" ]]; then
      find "$path" \
        \( -path '*/.git' \
        -o -path '*/.Rproj.user' \
        -o -path '*/.quarto' \
        -o -path '*/R_package_mobio' \
        -o -path '*/R_packages_mobio' \
        -o -path '*/chaoliu_R_package_mobio' \
        -o -path '*/lvyan_R_package_mobio' \
        -o -path '*/site-library' \
        -o -path '*/library' \
        -o -path '*/vendor_packages' \
        -o -path '*/results/*' \) -prune \
        -o -type f \
        ! -name '.DS_Store' \
        ! -name '.Rhistory' \
        ! -name '.env' \
        ! -name '*.log' \
        ! -name '*.pdf' \
        ! -name '*.png' \
        ! -name '*.jpg' \
        ! -name '*.jpeg' \
        ! -name '*.tif' \
        ! -name '*.tiff' \
        ! -name '*.svg' \
        ! -name '*.html' \
        ! -name '*.htm' \
        ! -name '*.ppt' \
        ! -name '*.pptx' \
        ! -name '*.xls' \
        ! -name '*.xlsx' \
        ! -name '*.xlsm' \
        ! -name '*.zip' \
        ! -name '*.tar' \
        ! -name '*.tar.gz' \
        ! -name '*.tgz' \
        ! -name '*.RData' \
        ! -name '*.Rdata' \
        ! -name '*.rds' \
        ! -name '*.rdb' \
        ! -name '*.rdx' \
        ! -name '*.so' \
        ! -name '*.parquet' \
        ! -name '*.feather' \
        ! -name '*.h5' \
        ! -name '*.hdf5' \
        ! -name '*.sqlite' \
        ! -name '*.sqlite3' \
        ! -name '*.db' \
        -print >> "$file_list"
    fi
  done
fi
sort -u "$file_list" -o "$file_list"

echo "Sync target: ${REMOTE_HOST}:${REMOTE_ROOT}"
if [[ ${#DRY_RUN[@]} -gt 0 ]]; then
  echo "Mode: dry-run. Set APPLY=1 to copy files."
else
  echo "Mode: apply."
fi
echo "Path list:"
sed 's/^/  /' "$file_list"

rsync -avR "${DRY_RUN[@]}" \
  --files-from="$file_list" \
  ./ "${REMOTE_HOST}:${REMOTE_ROOT}/"
