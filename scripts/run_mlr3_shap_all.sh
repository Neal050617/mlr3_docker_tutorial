#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
MLR3_ROOT="${MLR3_ROOT:-${REPO_ROOT}/scripts}"

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 /path/to/result_root" >&2
  exit 2
fi

RUN_ROOT="$1"
RUN_ROOT="$(cd "${RUN_ROOT}" && pwd)"

SHAP_NSIM_TRAIN="${SHAP_NSIM_TRAIN:-20}"
SHAP_NSIM_TEST="${SHAP_NSIM_TEST:-12}"
SHAP_NSIM_VALID="${SHAP_NSIM_VALID:-12}"
SHAP_MAX_TRAIN_ROWS="${SHAP_MAX_TRAIN_ROWS:-60}"
SHAP_MAX_TEST_ROWS="${SHAP_MAX_TEST_ROWS:-60}"
SHAP_MAX_VALID_ROWS="${SHAP_MAX_VALID_ROWS:-60}"
SHAP_BACKGROUND_ROWS="${SHAP_BACKGROUND_ROWS:-80}"
SHAP_MIN_AUC="${SHAP_MIN_AUC:-0.500001}"
SHAP_DATASETS="${SHAP_DATASETS:-train,test,valid}"
SHAP_WATERFALL_SAMPLES="${SHAP_WATERFALL_SAMPLES:-}"
SHAP_WATERFALL_GROUPS="${SHAP_WATERFALL_GROUPS:-case,control}"
SHAP_WATERFALL_PER_GROUP="${SHAP_WATERFALL_PER_GROUP:-1}"
SHAP_FORCE_PLOTS="${SHAP_FORCE_PLOTS:-TRUE}"
SHAP_LOCAL_DISPLAY_FEATURES="${SHAP_LOCAL_DISPLAY_FEATURES:-10}"
SHAP_FEATURE_LABEL_WIDTH="${SHAP_FEATURE_LABEL_WIDTH:-28}"
SHAP_OUTDIR_NAME="${SHAP_OUTDIR_NAME:-step3_shap_all_models_auc_gt_0.5}"
SHAP_MODELS="${SHAP_MODELS:-all}"
SHAP_STEP2_GLOB="${SHAP_STEP2_GLOB:-run_model_*_step2}"
SHAP_CONTINUE_ON_ERROR="${SHAP_CONTINUE_ON_ERROR:-TRUE}"

if [[ ! -f "${MLR3_ROOT}/efs_valid_plus_step3.v0.2.2.R" ]]; then
  echo "[mlr3-shap] missing Step3 script: ${MLR3_ROOT}/efs_valid_plus_step3.v0.2.2.R" >&2
  exit 1
fi

mapfile -t STEP2_DIRS < <(find "${RUN_ROOT}" -maxdepth 1 -type d -name "${SHAP_STEP2_GLOB}" | sort)
if [[ "${#STEP2_DIRS[@]}" -eq 0 ]]; then
  echo "[mlr3-shap] no Step2 directories found under ${RUN_ROOT} with glob ${SHAP_STEP2_GLOB}" >&2
  exit 1
fi

run_step3_one_dir() {
  local step2_dir="$1"
  local rdata="${step2_dir}/efs.RData"
  local outdir="${step2_dir}/${SHAP_OUTDIR_NAME}"

  if [[ ! -f "${rdata}" ]]; then
    echo "[mlr3-shap] missing efs.RData: ${rdata}" >&2
    return 1
  fi

  echo "[mlr3-shap] $(basename "${step2_dir}")"
  (
    cd "${MLR3_ROOT}"
    Rscript efs_valid_plus_step3.v0.2.2.R \
      --load_rdata="${rdata}" \
      --models="${SHAP_MODELS}" \
      --datasets="${SHAP_DATASETS}" \
      --require_all=FALSE \
      --skip_poor_models=TRUE \
      --min_auc_for_shap="${SHAP_MIN_AUC}" \
      --skip_zero_recall_models=FALSE \
      --shap_metric_priority=test,cv,train \
      --waterfall_samples="${SHAP_WATERFALL_SAMPLES}" \
      --waterfall_groups="${SHAP_WATERFALL_GROUPS}" \
      --waterfall_per_group="${SHAP_WATERFALL_PER_GROUP}" \
      --force_plots="${SHAP_FORCE_PLOTS}" \
      --local_display_features="${SHAP_LOCAL_DISPLAY_FEATURES}" \
      --feature_label_width="${SHAP_FEATURE_LABEL_WIDTH}" \
      --nsim_train="${SHAP_NSIM_TRAIN}" \
      --nsim_test="${SHAP_NSIM_TEST}" \
      --nsim_valid="${SHAP_NSIM_VALID}" \
      --max_train_rows="${SHAP_MAX_TRAIN_ROWS}" \
      --max_test_rows="${SHAP_MAX_TEST_ROWS}" \
      --max_valid_rows="${SHAP_MAX_VALID_ROWS}" \
      --background_rows="${SHAP_BACKGROUND_ROWS}" \
      --outdir="${outdir}"
  )
}

echo "[mlr3-shap] run root: ${RUN_ROOT}"
echo "[mlr3-shap] mlr3 root: ${MLR3_ROOT}"
echo "[mlr3-shap] step2 dirs: ${#STEP2_DIRS[@]}"
echo "[mlr3-shap] min_auc_for_shap: ${SHAP_MIN_AUC}"
echo "[mlr3-shap] datasets: ${SHAP_DATASETS}"
echo "[mlr3-shap] outdir_name: ${SHAP_OUTDIR_NAME}"
echo "[mlr3-shap] models: ${SHAP_MODELS}"
echo "[mlr3-shap] waterfall_samples=${SHAP_WATERFALL_SAMPLES}, waterfall_groups=${SHAP_WATERFALL_GROUPS}, waterfall_per_group=${SHAP_WATERFALL_PER_GROUP}, force_plots=${SHAP_FORCE_PLOTS}, local_display_features=${SHAP_LOCAL_DISPLAY_FEATURES}, feature_label_width=${SHAP_FEATURE_LABEL_WIDTH}"
echo "[mlr3-shap] shap: nsim_train=${SHAP_NSIM_TRAIN}, nsim_test=${SHAP_NSIM_TEST}, nsim_valid=${SHAP_NSIM_VALID}, max_train_rows=${SHAP_MAX_TRAIN_ROWS}, max_test_rows=${SHAP_MAX_TEST_ROWS}, max_valid_rows=${SHAP_MAX_VALID_ROWS}, background_rows=${SHAP_BACKGROUND_ROWS}"

failed=0
for step2_dir in "${STEP2_DIRS[@]}"; do
  if ! run_step3_one_dir "${step2_dir}"; then
    failed=$((failed + 1))
    if [[ "${SHAP_CONTINUE_ON_ERROR}" == "TRUE" ]]; then
      echo "[mlr3-shap] failed but continue: $(basename "${step2_dir}")" >&2
    else
      exit 1
    fi
  fi
done

if [[ "${failed}" -gt 0 ]]; then
  echo "[mlr3-shap] completed with failed branches: ${failed}" >&2
else
  echo "[mlr3-shap] completed"
fi
