#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
RUN_ROOT="${1:-${PROJECT_DIR}/reproduce_gp01_hc_af_20260519_155548_session}"
RUN_ROOT="$(cd "${RUN_ROOT}" && pwd)"
SHAP_SUMMARY_DIR_NAME="${SHAP_SUMMARY_DIR_NAME:-}"
REFRESH_THRESHOLD_METRICS="${REFRESH_THRESHOLD_METRICS:-FALSE}"

echo "[gp01-summary] run root: ${RUN_ROOT}"

if [[ "${REFRESH_THRESHOLD_METRICS}" == "TRUE" ]]; then
  Rscript "${SCRIPT_DIR}/../../scripts/repair_mlr3_threshold_columns.R" \
    --root="${RUN_ROOT}"
fi

Rscript "${SCRIPT_DIR}/../../scripts/summarize_mlr3_results.R" \
  --root="${RUN_ROOT}" \
  --branch-map="${PROJECT_DIR}/v0.2.2_inputs_gp01_hc_af/input_summary.tsv" \
  --project-name="GP01.HC-AF" \
  --title="GP01.HC-AF" \
  --script-label="scripts/summarize_mlr3_results.R" \
  --shap-dir-name="${SHAP_SUMMARY_DIR_NAME}" \
  --tasks=all
