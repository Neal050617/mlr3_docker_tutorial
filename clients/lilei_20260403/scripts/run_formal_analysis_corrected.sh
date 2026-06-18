#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
WORKSPACE_ROOT="$(cd "${REPO_ROOT}/.." && pwd)"
MLR3_ROOT="${MLR3_ROOT:-${WORKSPACE_ROOT}/scripts}"
cd "${REPO_ROOT}"

SCOPE="${1:-step2}"
OUTROOT="${2:-20260403_lilei/reproduce_formal_corrected_$(date +%Y%m%d_%H%M%S)}"

case "${SCOPE}" in
  step2|all) ;;
  *)
    echo "Usage: bash 20260403_lilei/scripts/run_formal_analysis_corrected.sh [step2|all] [output_root]" >&2
    exit 1
    ;;
esac

INPUT_DIR="20260403_lilei/v0.2.2_inputs_corrected"
FEATURE_FRACTION="${FEATURE_FRACTION:-0.5}"
# efs_valid_plus Step2 treats the first group in --gp as the positive class.
# Biomedical direction: LAEC/P_NRT are cases; Con/NP_NRT are controls.
GP_DIAG="${GP_DIAG:-LAEC-Con}"
GP_RESPONSE="${GP_RESPONSE:-P_NRT-NP_NRT}"
SHAP_NSIM_TRAIN="${SHAP_NSIM_TRAIN:-20}"
SHAP_NSIM_TEST="${SHAP_NSIM_TEST:-12}"
SHAP_NSIM_VALID="${SHAP_NSIM_VALID:-12}"
SHAP_MAX_TRAIN_ROWS="${SHAP_MAX_TRAIN_ROWS:-60}"
SHAP_MAX_TEST_ROWS="${SHAP_MAX_TEST_ROWS:-60}"
SHAP_MAX_VALID_ROWS="${SHAP_MAX_VALID_ROWS:-60}"
SHAP_BACKGROUND_ROWS="${SHAP_BACKGROUND_ROWS:-80}"
SHAP_OUTDIR_NAME="${SHAP_OUTDIR_NAME:-step3_shap_formal_case_control_force_20260527}"
SHAP_WATERFALL_GROUPS="${SHAP_WATERFALL_GROUPS:-case,control}"
SHAP_WATERFALL_PER_GROUP="${SHAP_WATERFALL_PER_GROUP:-1}"
SHAP_FORCE_PLOTS="${SHAP_FORCE_PLOTS:-TRUE}"
SHAP_LOCAL_DISPLAY_FEATURES="${SHAP_LOCAL_DISPLAY_FEATURES:-10}"
SHAP_FEATURE_LABEL_WIDTH="${SHAP_FEATURE_LABEL_WIDTH:-28}"

mkdir -p "${OUTROOT}"

echo "[corrected] prepare inputs"
Rscript 20260403_lilei/scripts/prepare_v022_inputs_corrected.R

STEP2_COMMON=(
  --part 3/4
  --split none
  --n_features 15
  --feature_fraction "${FEATURE_FRACTION}"
  --outer_cv 5
  --inner_cv 3
  --tune_evals 20
  --tune_batch_size 5
  --mlr3_log_level warn
  --cores 1
  --model_subset all
  --feature_combine_method majority
  --valid none
)

STEP2_SMOTE=(
  --use_smote TRUE
  --smote_k 1
  --smote_dup_size 1
)

run_step1() {
  local input="$1"
  local map="$2"
  local preset="$3"
  local gp="$4"
  local outdir="$5"
  Rscript "${MLR3_ROOT}/efs_valid_plus_step1.v0.2.2.R" \
    --input="${input}" \
    --map="${map}" \
    --gp="${gp}" \
    --preset="${preset}" \
    --outdir="${outdir}"
}

run_step2_plain() {
  local step1_dir="$1"
  local map="$2"
  local gp="$3"
  local outdir="$4"
  Rscript "${MLR3_ROOT}/efs_valid_plus_step2.v0.2.2.R" \
    --load_preprocessed "${step1_dir}/preprocessed_data.RData" \
    --map "${map}" \
    --gp "${gp}" \
    "${STEP2_COMMON[@]}" \
    --outdir "${outdir}"
}

run_step2_smote() {
  local step1_dir="$1"
  local map="$2"
  local gp="$3"
  local outdir="$4"
  Rscript "${MLR3_ROOT}/efs_valid_plus_step2.v0.2.2.R" \
    --load_preprocessed "${step1_dir}/preprocessed_data.RData" \
    --map "${map}" \
    --gp "${gp}" \
    "${STEP2_COMMON[@]}" \
    "${STEP2_SMOTE[@]}" \
    --outdir "${outdir}"
}

archive_incomplete_dir() {
  local dir="$1"
  if [[ -d "${dir}" && ! -f "${dir}/model_performance_comparison.xlsx" && ! -f "${dir}/efs.RData" ]]; then
    local archived="${dir}.interrupted_$(date +%Y%m%d_%H%M%S)"
    echo "[corrected] archive incomplete dir: ${dir} -> ${archived}"
    mv "${dir}" "${archived}"
  elif [[ -d "${dir}" && ! -f "${dir}/model_performance_comparison.xlsx" && -f "${dir}/efs.RData" ]]; then
    echo "[corrected] ${dir} has efs.RData checkpoint, keep for Step2 resume"
  fi
}

step1_ready() {
  local step1_dir="$1"
  [[ -f "${step1_dir}/preprocessed_data.RData" ]]
}

step2_ready() {
  local step2_dir="$1"
  [[ -f "${step2_dir}/model_performance_comparison.xlsx" && -f "${step2_dir}/efs.RData" ]]
}

run_model_plain() {
  local tag="$1"
  local input="$2"
  local map="$3"
  local preset="$4"
  local gp="$5"
  local step1_dir="${OUTROOT}/${tag}_step1"
  local step2_dir="${OUTROOT}/${tag}_step2"
  if step2_ready "${step2_dir}"; then
    echo "[corrected] ${tag} step2 already complete, skip"
    return 0
  fi
  archive_incomplete_dir "${step2_dir}"
  if step1_ready "${step1_dir}"; then
    echo "[corrected] ${tag} step1 already complete, skip"
  else
    archive_incomplete_dir "${step1_dir}"
    echo "[corrected] ${tag} step1"
    run_step1 "${input}" "${map}" "${preset}" "${gp}" "${step1_dir}"
  fi
  echo "[corrected] ${tag} step2 plain"
  run_step2_plain "${step1_dir}" "${map}" "${gp}" "${step2_dir}"
}

run_model_smote() {
  local tag="$1"
  local input="$2"
  local map="$3"
  local preset="$4"
  local gp="$5"
  local step1_dir="${OUTROOT}/${tag}_step1"
  local step2_dir="${OUTROOT}/${tag}_step2"
  if step2_ready "${step2_dir}"; then
    echo "[corrected] ${tag} step2 already complete, skip"
    return 0
  fi
  archive_incomplete_dir "${step2_dir}"
  if step1_ready "${step1_dir}"; then
    echo "[corrected] ${tag} step1 already complete, skip"
  else
    archive_incomplete_dir "${step1_dir}"
    echo "[corrected] ${tag} step1"
    run_step1 "${input}" "${map}" "${preset}" "${gp}" "${step1_dir}"
  fi
  echo "[corrected] ${tag} step2 smote"
  run_step2_smote "${step1_dir}" "${map}" "${gp}" "${step2_dir}"
}

echo "[corrected] output root: ${OUTROOT}"
echo "[corrected] scope: ${SCOPE}"
echo "[corrected] mlr3 root: ${MLR3_ROOT}"
echo "[corrected] feature_fraction: ${FEATURE_FRACTION}"
echo "[corrected] gp_diag: ${GP_DIAG}"
echo "[corrected] gp_response: ${GP_RESPONSE}"
echo "[corrected] shap: nsim_train=${SHAP_NSIM_TRAIN}, nsim_test=${SHAP_NSIM_TEST}, max_train_rows=${SHAP_MAX_TRAIN_ROWS}, max_test_rows=${SHAP_MAX_TEST_ROWS}, background_rows=${SHAP_BACKGROUND_ROWS}, local_display_features=${SHAP_LOCAL_DISPLAY_FEATURES}, feature_label_width=${SHAP_FEATURE_LABEL_WIDTH}"

MAP_DIAG="${INPUT_DIR}/map-group_con_laec_246.txt"
MAP_RESPONSE="${INPUT_DIR}/map-group_nrt_response_77.txt"

# 0.1-0.3: LAEC case vs Con control diagnostic branch, no SMOTE by default.
run_model_plain "run_model_0.1" "${INPUT_DIR}/model_0.1_con_laec_microbiome_246.tsv" "${MAP_DIAG}" "microbiome_standard" "${GP_DIAG}"
run_model_plain "run_model_0.2" "${INPUT_DIR}/model_0.2_con_laec_metabolome_246.tsv" "${MAP_DIAG}" "metabolome_standard" "${GP_DIAG}"
run_model_plain "run_model_0.3" "${INPUT_DIR}/model_0.3_con_laec_multiomics_246.tsv" "${MAP_DIAG}" "mixed_omics" "${GP_DIAG}"

# 1.0-1.6: P_NRT case vs NP_NRT control response branch, SMOTE because P_NRT is small.
run_model_smote "run_model_1.0" "${INPUT_DIR}/model_1.0_clinical_77.tsv" "${MAP_RESPONSE}" "clinical_only" "${GP_RESPONSE}"
run_model_smote "run_model_1.1" "${INPUT_DIR}/model_1.1_clinical_microbiome_77.tsv" "${MAP_RESPONSE}" "mixed_omics" "${GP_RESPONSE}"
run_model_smote "run_model_1.2" "${INPUT_DIR}/model_1.2_clinical_metabolome_77.tsv" "${MAP_RESPONSE}" "mixed_omics" "${GP_RESPONSE}"
run_model_smote "run_model_1.3" "${INPUT_DIR}/model_1.3_clinical_multiomics_77.tsv" "${MAP_RESPONSE}" "mixed_omics" "${GP_RESPONSE}"
run_model_smote "run_model_1.4" "${INPUT_DIR}/model_1.4_clinical_microbiome_delta_77.tsv" "${MAP_RESPONSE}" "mixed_omics" "${GP_RESPONSE}"
run_model_smote "run_model_1.5" "${INPUT_DIR}/model_1.5_clinical_metabolome_delta_77.tsv" "${MAP_RESPONSE}" "mixed_omics" "${GP_RESPONSE}"
run_model_smote "run_model_1.6" "${INPUT_DIR}/model_1.6_clinical_multiomics_delta_77.tsv" "${MAP_RESPONSE}" "mixed_omics" "${GP_RESPONSE}"

if [[ "${SCOPE}" == "all" ]]; then
  echo "[corrected] SHAP all models via repository-level scripts/run_mlr3_shap_all.sh"
  SHAP_OUTDIR_NAME="${SHAP_OUTDIR_NAME}" \
  SHAP_DATASETS=train,test,valid \
  SHAP_WATERFALL_GROUPS="${SHAP_WATERFALL_GROUPS}" \
  SHAP_WATERFALL_PER_GROUP="${SHAP_WATERFALL_PER_GROUP}" \
  SHAP_FORCE_PLOTS="${SHAP_FORCE_PLOTS}" \
  SHAP_LOCAL_DISPLAY_FEATURES="${SHAP_LOCAL_DISPLAY_FEATURES}" \
  SHAP_FEATURE_LABEL_WIDTH="${SHAP_FEATURE_LABEL_WIDTH}" \
  SHAP_NSIM_TRAIN="${SHAP_NSIM_TRAIN}" \
  SHAP_NSIM_TEST="${SHAP_NSIM_TEST}" \
  SHAP_NSIM_VALID="${SHAP_NSIM_VALID}" \
  SHAP_MAX_TRAIN_ROWS="${SHAP_MAX_TRAIN_ROWS}" \
  SHAP_MAX_TEST_ROWS="${SHAP_MAX_TEST_ROWS}" \
  SHAP_MAX_VALID_ROWS="${SHAP_MAX_VALID_ROWS}" \
  SHAP_BACKGROUND_ROWS="${SHAP_BACKGROUND_ROWS}" \
  MLR3_ROOT="${MLR3_ROOT}" \
    bash "${WORKSPACE_ROOT}/scripts/run_mlr3_shap_all.sh" "${OUTROOT}"
fi

echo "[corrected] completed"
echo "[corrected] results root: ${OUTROOT}"
