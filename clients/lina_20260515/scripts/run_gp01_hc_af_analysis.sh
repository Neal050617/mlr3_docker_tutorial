#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
WORKSPACE_ROOT="$(cd "${PROJECT_DIR}/.." && pwd)"
MLR3_ROOT="${MLR3_ROOT:-${WORKSPACE_ROOT}/scripts}"

SCOPE="${1:-single}"
OUTROOT="${2:-${PROJECT_DIR}/reproduce_gp01_hc_af_$(date +%Y%m%d_%H%M%S)}"

case "${SCOPE}" in
  prepare|single|joint|all) ;;
  *)
    echo "Usage: bash lina_20260515/scripts/run_gp01_hc_af_analysis.sh [prepare|single|joint|all] [output_root]" >&2
    exit 1
    ;;
esac

INPUT_DIR="${PROJECT_DIR}/v0.2.2_inputs_gp01_hc_af"
MAP_FILE="${INPUT_DIR}/map-group_gp01_hc_af_common_145.txt"
CASE_GROUP="${CASE_GROUP:-AF}"
CONTROL_GROUP="${CONTROL_GROUP:-HC}"
GROUP_ORDER="${GROUP_ORDER:-${CASE_GROUP}-${CONTROL_GROUP}}"

FEATURE_FRACTION="${FEATURE_FRACTION:-0.5}"
N_FEATURES="${N_FEATURES:-15}"
OUTER_CV="${OUTER_CV:-5}"
INNER_CV="${INNER_CV:-3}"
TUNE_EVALS="${TUNE_EVALS:-20}"
TUNE_BATCH_SIZE="${TUNE_BATCH_SIZE:-5}"
USE_SMOTE="${USE_SMOTE:-FALSE}"
SMOTE_K="${SMOTE_K:-3}"
SMOTE_DUP_SIZE="${SMOTE_DUP_SIZE:-1}"

mkdir -p "${OUTROOT}"
OUTROOT="$(cd "${OUTROOT}" && pwd)"

echo "[gp01] project: ${PROJECT_DIR}"
echo "[gp01] mlr3 root: ${MLR3_ROOT}"
echo "[gp01] output root: ${OUTROOT}"
echo "[gp01] scope: ${SCOPE}"
echo "[gp01] case/control: case=${CASE_GROUP}, control=${CONTROL_GROUP}, gp=${GROUP_ORDER}"

Rscript "${SCRIPT_DIR}/prepare_gp01_hc_af_inputs.R" \
  --project_dir="${PROJECT_DIR}" \
  --out_dir="${INPUT_DIR}"

case_n="$(awk -v g="${CASE_GROUP}" 'NR > 1 && $2 == g {n++} END {print n + 0}' "${MAP_FILE}")"
control_n="$(awk -v g="${CONTROL_GROUP}" 'NR > 1 && $2 == g {n++} END {print n + 0}' "${MAP_FILE}")"
if [[ "${case_n}" -eq 0 || "${control_n}" -eq 0 ]]; then
  echo "[gp01] ERROR: case/control groups not found in ${MAP_FILE}: case=${CASE_GROUP} n=${case_n}, control=${CONTROL_GROUP} n=${control_n}" >&2
  exit 1
fi
echo "[gp01] confirmed case/control counts: ${CASE_GROUP}=${case_n}, ${CONTROL_GROUP}=${control_n}"

if [[ "${SCOPE}" == "prepare" ]]; then
  echo "[gp01] prepared inputs only"
  exit 0
fi

cd "${MLR3_ROOT}"

STEP2_COMMON=(
  --part 3/4
  --split none
  --n_features "${N_FEATURES}"
  --feature_fraction "${FEATURE_FRACTION}"
  --outer_cv "${OUTER_CV}"
  --inner_cv "${INNER_CV}"
  --tune_evals "${TUNE_EVALS}"
  --tune_batch_size "${TUNE_BATCH_SIZE}"
  --mlr3_log_level warn
  --cores 1
  --model_subset all
  --feature_combine_method majority
  --valid none
)

if [[ "${USE_SMOTE}" == "TRUE" ]]; then
  STEP2_COMMON+=(--use_smote TRUE --smote_k "${SMOTE_K}" --smote_dup_size "${SMOTE_DUP_SIZE}")
fi

run_step1() {
  local input="$1"
  local preset="$2"
  local outdir="$3"
  Rscript efs_valid_plus_step1.v0.2.2.R \
    --input="${input}" \
    --map="${MAP_FILE}" \
    --gp "${GROUP_ORDER}" \
    --preset="${preset}" \
    --outdir="${outdir}"
}

run_step2() {
  local step1_dir="$1"
  local outdir="$2"
  Rscript efs_valid_plus_step2.v0.2.2.R \
    --load_preprocessed "${step1_dir}/preprocessed_data.RData" \
    --map "${MAP_FILE}" \
    --gp "${GROUP_ORDER}" \
    "${STEP2_COMMON[@]}" \
    --outdir "${outdir}"
}

step1_ready() {
  [[ -f "$1/preprocessed_data.RData" ]]
}

step2_ready() {
  [[ -f "$1/model_performance_comparison.xlsx" && -f "$1/efs.RData" ]]
}

run_model() {
  local tag="$1"
  local input="$2"
  local preset="$3"
  local step1_dir="${OUTROOT}/${tag}_step1"
  local step2_dir="${OUTROOT}/${tag}_step2"

  if step2_ready "${step2_dir}"; then
    echo "[gp01] ${tag} step2 already complete, skip"
    return 0
  fi
  if step1_ready "${step1_dir}"; then
    echo "[gp01] ${tag} step1 already complete, skip"
  else
    echo "[gp01] ${tag} step1"
    run_step1 "${input}" "${preset}" "${step1_dir}"
  fi
  echo "[gp01] ${tag} step2"
  run_step2 "${step1_dir}" "${step2_dir}"
}

run_single_models() {
  run_model "run_model_0.1_metabolome" "${INPUT_DIR}/model_0.1_gp01_hc_af_metabolome_145.tsv" "metabolome_standard"
  run_model "run_model_0.2_oral_bacteria" "${INPUT_DIR}/model_0.2_gp01_hc_af_oral_bacteria_145.tsv" "microbiome_standard"
  run_model "run_model_0.3_oral_fungi" "${INPUT_DIR}/model_0.3_gp01_hc_af_oral_fungi_145.tsv" "microbiome_standard"
  run_model "run_model_0.4_gut_bacteria" "${INPUT_DIR}/model_0.4_gp01_hc_af_gut_bacteria_145.tsv" "microbiome_standard"
  run_model "run_model_0.5_gut_fungi" "${INPUT_DIR}/model_0.5_gp01_hc_af_gut_fungi_145.tsv" "microbiome_standard"
}

run_joint_model() {
  Rscript "${SCRIPT_DIR}/build_joint_biomarker_input.R" \
    --input_dir="${INPUT_DIR}" \
    --run_root="${OUTROOT}" \
    --out_dir="${INPUT_DIR}"
  run_model "run_model_0.6_joint_selected_biomarkers" \
    "${INPUT_DIR}/model_0.6_gp01_hc_af_joint_selected_biomarkers_145.tsv" \
    "mixed_omics"
}

case "${SCOPE}" in
  single)
    run_single_models
    ;;
  joint)
    run_joint_model
    ;;
  all)
    run_single_models
    run_joint_model
    ;;
esac

echo "[gp01] completed"
echo "[gp01] results root: ${OUTROOT}"
