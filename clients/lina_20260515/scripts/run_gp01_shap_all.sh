#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_ROOT="$(cd "${PROJECT_DIR}/.." && pwd)"

RUN_ROOT="${1:-${PROJECT_DIR}/reproduce_gp01_hc_af_20260519_155548_session}"

exec "${REPO_ROOT}/scripts/run_mlr3_shap_all.sh" "${RUN_ROOT}"
