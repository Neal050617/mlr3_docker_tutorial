#!/usr/bin/env bash
set -euo pipefail

APPLY="${APPLY:-0}"
MODE="${MODE:-ignore-existing}"
RESULT_SET="${RESULT_SET:-formal}"

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
formal	lina_20251208/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156	clients/lilei_20260403/results/reproduce_formal_case_positive_fullrerun_20260527_0156
formal	lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531	clients/lina_20260515/results/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531
historical	python_xgboost_shap/output	clients/python_xgboost_shap/results/output
historical	kangwenzhong/efs123	clients/kangwenzhong/results/efs123
historical	kangwenzhong/efs1234	clients/kangwenzhong/results/efs1234
historical	kangwenzhong/efs1234_resampling_10	clients/kangwenzhong/results/efs1234_resampling_10
historical	kangwenzhong/efs1234_resampling_5	clients/kangwenzhong/results/efs1234_resampling_5
historical	kangwenzhong_delete_RE_15/efs1234	clients/kangwenzhong_delete_RE_15/results/efs1234
historical	kangwenzhong_delete_RE_15/efs_test_0.5	clients/kangwenzhong_delete_RE_15/results/efs_test_0.5
historical	kangwenzhong_delete_RE_15/efs_test_youden	clients/kangwenzhong_delete_RE_15/results/efs_test_youden
historical	kangwenzhong_delete_RE_15/efs_test_youden_final	clients/kangwenzhong_delete_RE_15/results/efs_test_youden_final
historical	kangwenzhong_delete_RE_15/efs_test_youden_final2	clients/kangwenzhong_delete_RE_15/results/efs_test_youden_final2
historical	kangwenzhong_delete_RE_15/efs_test_youden_final4	clients/kangwenzhong_delete_RE_15/results/efs_test_youden_final4
historical	lina_valid/efs123	clients/lina_valid/results/efs123
historical	lina_valid/efs123-15features	clients/lina_valid/results/efs123-15features
historical	lina_valid/efs123_20250409	clients/lina_valid/results/efs123_20250409
historical	lina_valid/efs678	clients/lina_valid/results/efs678
historical	lina_valid/外部验证测试	clients/lina_valid/results/外部验证测试
historical	wangxueping/efs123_no_unif	clients/wangxueping/results/efs123_no_unif
historical	wangxueping/efs123_resample5s	clients/wangxueping/results/efs123_resample5s
historical	wangxueping/efs678	clients/wangxueping/results/efs678
historical	wangxueping_alltax/efs123	clients/wangxueping_alltax/results/efs123
historical	wangxueping_alltax/efs678	clients/wangxueping_alltax/results/efs678
historical	wangxueping_alltax2/efs123_5_5_5_split_0.8	clients/wangxueping_alltax2/results/efs123_5_5_5_split_0.8
historical	wangxueping_alltax2/efs123_5_5_5_split_2_3	clients/wangxueping_alltax2/results/efs123_5_5_5_split_2_3
historical	wangxueping_alltax2/efs123_origin	clients/wangxueping_alltax2/results/efs123_origin
historical	wangxueping_alltax2/efs678	clients/wangxueping_alltax2/results/efs678
historical	wangxueping_alltax2/efs678_5_5_5_split_0.8	clients/wangxueping_alltax2/results/efs678_5_5_5_split_0.8
historical	wangxueping_alltax2/efs678_5_5_5_split_2_3	clients/wangxueping_alltax2/results/efs678_5_5_5_split_2_3
historical	wangxueping_alltax2/efs678_5_5_5_split_2_3_bak	clients/wangxueping_alltax2/results/efs678_5_5_5_split_2_3_bak
historical	wangxueping_alltax2/efs678_origin	clients/wangxueping_alltax2/results/efs678_origin
historical	wangxueping_alltax2/efs789_5_5_5_split_0.8	clients/wangxueping_alltax2/results/efs789_5_5_5_split_0.8
historical	wangxueping_alltax2/efs789_5_5_5_split_2_3	clients/wangxueping_alltax2/results/efs789_5_5_5_split_2_3
historical	wangxueping_alltax2/efs789_origin	clients/wangxueping_alltax2/results/efs789_origin
EOF

case "$RESULT_SET" in
  formal|historical|all)
    ;;
  *)
    echo "Unsupported RESULT_SET=$RESULT_SET. Use formal, historical, or all." >&2
    exit 2
    ;;
esac

echo "Materialize client results"
echo "Result set: $RESULT_SET"
echo "Mode: $MODE"
if [[ ${#dry_run[@]} -gt 0 ]]; then
  echo "Apply: no, dry-run only. Set APPLY=1 to copy files."
else
  echo "Apply: yes."
fi
echo "Delete: disabled. This script never passes --delete."
echo

while IFS=$'\t' read -r group src dst; do
  [[ "$RESULT_SET" == "all" || "$RESULT_SET" == "$group" ]] || continue
  echo "== $src -> $dst =="
  if [[ ! -e "$src" ]]; then
    echo "SKIP missing source: $src"
    echo
    continue
  fi
  mkdir -p "$dst"
  rsync -avh --stats \
    "${dry_run[@]}" "${mode_flags[@]}" \
    --exclude='.DS_Store' \
    "$src/" "$dst/"
  echo
done < "$mapping_file"
