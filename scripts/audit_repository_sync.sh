#!/usr/bin/env bash
set -euo pipefail

REMOTE_HOST="${REMOTE_HOST:-dell}"
REMOTE_ROOT="${REMOTE_ROOT:-/work/users/chaoliu/Test/20250320-mlr3-test}"
OUT_DIR="${OUT_DIR:-docs/audit/20260612_sync_audit}"

mkdir -p "$OUT_DIR"

run_local() {
  local name="$1"
  shift
  {
    printf '# command:'
    printf ' %q' "$@"
    printf '\n# generated_at: %s\n\n' "$(date '+%Y-%m-%d %H:%M:%S %z')"
    "$@"
  } > "$OUT_DIR/$name"
}

run_remote() {
  local name="$1"
  local script="$2"
  {
    printf '# remote: %s:%s\n' "$REMOTE_HOST" "$REMOTE_ROOT"
    printf '# generated_at: %s\n\n' "$(date '+%Y-%m-%d %H:%M:%S %z')"
    ssh "$REMOTE_HOST" "REMOTE_ROOT='$REMOTE_ROOT' bash -s" <<< "$script"
  } > "$OUT_DIR/$name"
}

run_local git_status.txt git status --short

run_local local_key_sizes.txt bash -lc '
du -sh mobior_v0.0.5 mobior_v0.0.6 lina_20251208/20260403_lilei lina_20260515 scripts skills 2>/dev/null || true
'

run_local local_git_candidate_files.txt bash -lc '
find . -maxdepth 4 \
  \( -path "./.git" -o -path "./R_package_mobio" -o -path "./R_packages_mobio" \) -prune -o \
  -type f \( -name "*.md" -o -name "*.R" -o -name "*.sh" -o -name "Dockerfile" -o -name "*.yml" -o -name "*.yaml" -o -name "*.tsv" \) \
  -exec stat -f "%z\t%N" {} \; | sort -k2
'

run_remote remote_key_sizes.txt '
for p in \
  "$REMOTE_ROOT/mobior_v0.0.6" \
  "$REMOTE_ROOT/R_packages_mobio" \
  "$REMOTE_ROOT/lina_20251208/20260403_lilei" \
  "$REMOTE_ROOT/lina_20260515" \
  "$REMOTE_ROOT/lina_20251208/20260403_lilei/reproduce_formal_v006_msi12_name_20260609_000808"; do
  if [ -e "$p" ]; then
    du -sh "$p"
  else
    echo "MISSING $p"
  fi
done
'

run_remote remote_git_candidate_files.txt '
for p in \
  "$REMOTE_ROOT/mobior_v0.0.6" \
  "$REMOTE_ROOT/R_packages_mobio" \
  "$REMOTE_ROOT/lina_20251208/20260403_lilei" \
  "$REMOTE_ROOT/lina_20260515"; do
  printf "\n### %s\n" "$p"
  find "$p" -maxdepth 4 \
    \( -name site-library -o -name vendor_packages \) -prune -o \
    -type f \( -name "*.md" -o -name "*.R" -o -name "*.sh" -o -name "Dockerfile" -o -name "*.yml" -o -name "*.yaml" -o -name "*.tsv" \) \
    -printf "%TY-%Tm-%Td %TH:%TM\t%s\t%p\n" 2>/dev/null | sort
done
'

run_remote remote_v006_status.txt '
ROOT="$REMOTE_ROOT/lina_20251208/20260403_lilei/reproduce_formal_v006_msi12_name_20260609_000808"
echo "ROOT=$ROOT"
if [ ! -d "$ROOT" ]; then
  echo "MISSING"
  exit 0
fi
du -sh "$ROOT"
printf "step2_dirs\t"; find "$ROOT" -maxdepth 1 -type d -name "run_model*_step2" | wc -l
printf "step3_dirs\t"; find "$ROOT" -maxdepth 2 -type d -name "step3_shap_all_models_v006_msi12_name_20260609" | wc -l
printf "shap_xlsx\t"; find "$ROOT" -path "*/step3_shap_all_models_v006_msi12_name_20260609/*/*_SHAP.xlsx" | wc -l
printf "importance_pdf\t"; find "$ROOT" -path "*/step3_shap_all_models_v006_msi12_name_20260609/*/*importance*.pdf" | wc -l
printf "beeswarm_pdf\t"; find "$ROOT" -path "*/step3_shap_all_models_v006_msi12_name_20260609/*/*beeswarm*.pdf" | wc -l
printf "waterfall_pdf\t"; find "$ROOT" -path "*/step3_shap_all_models_v006_msi12_name_20260609/*/*waterfall*.pdf" | wc -l
printf "force_pdf\t"; find "$ROOT" -path "*/step3_shap_all_models_v006_msi12_name_20260609/*/*force*.pdf" | wc -l
find "$ROOT" -path "*/step3_shap_all_models_v006_msi12_name_20260609/step3_shap_status.tsv" -print0 |
  xargs -0 awk -F "\t" '"'"'
    FNR == 1 { for (i = 1; i <= NF; i++) h[$i] = i; next }
    { status[$h["Status"]]++ }
    END { for (s in status) print "Status[" s "]\t" status[s] }
  '"'"'
'

run_remote remote_mobior_v006_package_status.txt '
echo "docker_image"
docker image ls --format "{{.Repository}}:{{.Tag}}\t{{.ID}}\t{{.CreatedAt}}\t{{.Size}}" | grep -E "^mobior:v0.0.6|^mobior:v0.0.5" || true
echo
echo "core_package_check"
for f in \
  "$REMOTE_ROOT/R_packages_mobio/package_check_core_v0.0.6.tsv" \
  "$REMOTE_ROOT/R_packages_mobio/package_check_full_after_available_install_v0.0.6.tsv" \
  "$REMOTE_ROOT/R_packages_mobio/full_unresolved_v0.0.6.tsv"; do
  echo "FILE=$f"
  if [ -f "$f" ]; then
    awk '"'"'END { print "rows\t" NR - 1 }'"'"' "$f"
    sed -n "1,8p" "$f"
  else
    echo MISSING
  fi
done
'

{
  printf '# Dry-run rsync commands\n\n'
  printf 'These commands intentionally do not change files.\n\n'
  printf '```bash\n'
  printf 'rsync -av --dry-run %s:%q/mobior_v0.0.6/ mobior_v0.0.6/\n' "$REMOTE_HOST" "$REMOTE_ROOT"
  printf 'rsync -av --dry-run %s:%q/R_packages_mobio/package_*_v0.0.6.tsv resources/package_manifests/\n' "$REMOTE_HOST" "$REMOTE_ROOT"
  printf 'rsync -av --dry-run %s:%q/R_packages_mobio/full_*_v0.0.6.tsv resources/package_manifests/\n' "$REMOTE_HOST" "$REMOTE_ROOT"
  printf '```\n'
} > "$OUT_DIR/rsync_dry_run_commands.md"

echo "Audit written to $OUT_DIR"
