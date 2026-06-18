#!/usr/bin/env bash
set -euo pipefail

APPLY="${APPLY:-0}"
MODE="${MODE:-ignore-existing}"

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

mapping_file="$(mktemp)"
trap 'rm -f "$mapping_file"' EXIT

cat > "$mapping_file" <<'EOF'
data_for_analysis/20250211-mlr-JINYU	clients/jinyu_20250211/legacy_mirror/data_for_analysis__20250211-mlr-JINYU	
data_for_analysis/20250908_金域数据单肺炎支原体感染数据建模_repeats20_seed123	clients/jinyu_20250908/legacy_mirror/data_for_analysis__20250908_金域数据单肺炎支原体感染数据建模_repeats20_seed123	
20250729_jinyu	clients/jinyu_20250729/legacy_mirror/20250729_jinyu	
data_for_analysis/金域_脚本	clients/jinyu_scripts/legacy_mirror/data_for_analysis__金域_脚本	
kangwenzhong	clients/kangwenzhong/legacy_mirror/kangwenzhong	
kangwenzhong_delete_RE_15	clients/kangwenzhong_delete_RE_15/legacy_mirror/kangwenzhong_delete_RE_15	
lina	clients/lina_legacy/legacy_mirror/lina	
lina_valid	clients/lina_valid/legacy_mirror/lina_valid	
lina_20251208/20260403_lilei	clients/lilei_20260403/legacy_mirror/lina_20251208__20260403_lilei	
lina_20251208	clients/lina_20251208_legacy/legacy_mirror/lina_20251208_root	/20260403_lilei/
lina_20260515	clients/lina_20260515/legacy_mirror/lina_20260515	
python_xgboost_shap	clients/python_xgboost_shap/legacy_mirror/python_xgboost_shap	
wangxueping	clients/wangxueping/legacy_mirror/wangxueping	
wangxueping_alltax	clients/wangxueping_alltax/legacy_mirror/wangxueping_alltax	
wangxueping_alltax2	clients/wangxueping_alltax2/legacy_mirror/wangxueping_alltax2	
EOF

echo "Materialize legacy roots under clients"
echo "Mode: $MODE"
if [[ ${#dry_run[@]} -gt 0 ]]; then
  echo "Apply: no, dry-run only. Set APPLY=1 to copy files."
else
  echo "Apply: yes."
fi
echo "Delete: disabled. This script never passes --delete."
echo "Excluded duplicate nested roots only; editor/runtime files are preserved in legacy_mirror."
echo

while IFS=$'\t' read -r src dst extra_exclude; do
  echo "== $src -> $dst =="
  if [[ ! -e "$src" ]]; then
    echo "SKIP missing source: $src"
    echo
    continue
  fi
  mkdir -p "$dst"
  exclude_flags=()
  if [[ -n "${extra_exclude:-}" ]]; then
    exclude_flags+=(--exclude="$extra_exclude")
  fi
  rsync -avh --stats \
    "${dry_run[@]}" "${mode_flags[@]}" \
    "${exclude_flags[@]}" \
    "$src/" "$dst/"
  echo
done < "$mapping_file"
