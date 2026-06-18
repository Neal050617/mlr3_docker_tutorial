#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORKSPACE_ROOT="$(cd "${REPO_ROOT}/.." && pwd)"

RUN_ROOT="${1:-${REPO_ROOT}/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156}"
RUN_ROOT="$(cd "${RUN_ROOT}" && pwd)"

SHAP_OUTDIR_NAME="${SHAP_OUTDIR_NAME:-step3_shap_formal_case_control_force_20260527}"
SHAP_LOCAL_DISPLAY_FEATURES="${SHAP_LOCAL_DISPLAY_FEATURES:-10}"
SHAP_FEATURE_LABEL_WIDTH="${SHAP_FEATURE_LABEL_WIDTH:-28}"
MLR3_ROOT="${MLR3_ROOT:-${WORKSPACE_ROOT}/scripts}"
BRANCH_MAP="${BRANCH_MAP:-${RUN_ROOT}/branch_map_delivery_for_summary.tsv}"

echo "[lilei-shap-all] run root: ${RUN_ROOT}"
echo "[lilei-shap-all] mlr3 root: ${MLR3_ROOT}"
echo "[lilei-shap-all] output dir name: ${SHAP_OUTDIR_NAME}"
echo "[lilei-shap-all] local display features: ${SHAP_LOCAL_DISPLAY_FEATURES}"
echo "[lilei-shap-all] feature label width: ${SHAP_FEATURE_LABEL_WIDTH}"

MLR3_ROOT="${MLR3_ROOT}" \
SHAP_OUTDIR_NAME="${SHAP_OUTDIR_NAME}" \
SHAP_DATASETS="${SHAP_DATASETS:-train,test,valid}" \
SHAP_WATERFALL_GROUPS="${SHAP_WATERFALL_GROUPS:-case,control}" \
SHAP_WATERFALL_PER_GROUP="${SHAP_WATERFALL_PER_GROUP:-1}" \
SHAP_FORCE_PLOTS="${SHAP_FORCE_PLOTS:-TRUE}" \
SHAP_LOCAL_DISPLAY_FEATURES="${SHAP_LOCAL_DISPLAY_FEATURES}" \
SHAP_FEATURE_LABEL_WIDTH="${SHAP_FEATURE_LABEL_WIDTH}" \
  bash "${WORKSPACE_ROOT}/scripts/run_mlr3_shap_all.sh" "${RUN_ROOT}"

summary_args=(
  --root="${RUN_ROOT}"
  --tasks=shap
  --shap-dir-name="${SHAP_OUTDIR_NAME}"
)
if [[ -f "${BRANCH_MAP}" ]]; then
  summary_args+=(--branch-map="${BRANCH_MAP}")
fi

Rscript "${WORKSPACE_ROOT}/scripts/summarize_mlr3_results.R" "${summary_args[@]}"

echo "[lilei-shap-all] completed"
