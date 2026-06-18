---
name: mlr3-project-delivery
description: "Standardize and audit mlr3/R multi-omics modeling project delivery. Use when Codex needs to package a project like lina_20251208/20260403_lilei or lina_20260515: compare project organization, create README/docs/result summaries, verify Step1/Step2/Step3 SHAP outputs, summarize model metrics without unsupported best-model claims, or prepare reusable handoff documents."
---

# mlr3 Project Delivery

## Core Rule

Deliver the project as a reproducible result package, not only as raw model folders. Always inspect actual files before writing conclusions.

Use `references/delivery-checklist.md` for the full checklist. Use `scripts/audit_mlr3_delivery.py` for a quick structural audit.
Use repository-level result utilities when present:

- `scripts/efs_valid_plus_step1.v0.2.2.R`, `scripts/efs_valid_plus_step2.v0.2.2.R`, `scripts/efs_valid_plus_step3.v0.2.2.R`: stable repository-level wrappers for the v0.2.2 modeling scripts.
- `scripts/run_mlr3_shap_all.sh`: run Step3 SHAP over all `run_model_*_step2/efs.RData` folders in a result root.
- `scripts/repair_mlr3_threshold_columns.R`: clean older Step2 workbooks when needed; new full reruns should not depend on this repair as a required correctness step.
- `scripts/summarize_mlr3_results.R`: generate performance, Step2, SHAP, and manual summaries.

Before any modeling run, confirm case/control direction as a hard gate. Do not start Step1/Step2 until the intended case group, control group, positive class, explicit `--gp <case>-<control>` order, and map-file counts have been recorded. Do not infer it after the fact from finished results. For binary diagnostic work, the case group should normally be the positive class. In this mlr3 pipeline, Step2 uses `positive = gp[1]`, so the intended case/control order must be passed explicitly with `--gp <case>-<control>` or encoded in the map factor levels before Step1/Step2.

## Expected Package Shape

At project root:

- `README.md`: Chinese entrypoint with formal result directory and reading order.
- `docs/`: either the legacy detailed docs or the consolidated core doc set:
  - `01_建模设计与脚本来源.md`
  - `02_复现与参数.md`
  - `03_结果读取与技术汇总.md`
  - `04_结果解释边界与Methods.md`
  - optional but recommended for pipeline migrations: `05_v0.2.2管线修正与跨项目复用说明.md`
- `scripts/README.md`: script purpose and run order.
- input directory such as `v0.2.2_inputs_*`: generated model matrices, mapping/manifests, input summary, README.
- formal result directory such as `reproduce_*`: Step1/Step2 outputs, model performance summary, SHAP summary.

Inside the formal result directory:

- `README.md`: result reading order, scope, interpretation boundaries, reproduction commands.
- `RESULT_INTERPRETATION_MANUAL.md`: preferred single manual generated after model performance, Step2 technical, and SHAP summaries are refreshed.
- `run_model_*_step1/`: must include `data_type_classification.xlsx` when Step1 completed.
- `run_model_*_step2/`: must include `efs.RData`, `model_performance_comparison.xlsx`, `analysis_flowchart.md`, and run logs when available.
- `01_model_performance_summary/`: must include a Markdown summary, all-model TSV, sorted TSVs, and Excel workbook.
- `01_model_performance_summary/README.md`: preferred integrated result-reading hub; it may link to sibling `02_step2_technical_summary/` and `03_shap_output_summary/` rather than physically moving those generated outputs.
- `02_step2_technical_summary/`: should exist for formal packages; include one row per Step2 branch and one row per branch/model hyperparameter where available.
- `03_shap_output_summary/`: should exist when SHAP has been run; include branch status, model file list, and Markdown summary.

## Interpretation Standards

- Do not declare a unique "best model" from a custom `Balanced_Score` unless the user explicitly asks for an engineering ranking and the limitation is stated.
- State the positive class and control class in the design/reproduction docs. For disease diagnostic models, interpret sensitivity/recall, precision, F1, TP, FP, TN, and FN relative to the positive/case class.
- Treat `Balanced_Score` as descriptive only. Report AUC, AUPRC, ACC, MCC, sensitivity, specificity, TP, FP, TN, FN, and the confusion matrix.
- When Step2 uses `threshold=youden`, do not add a separate fixed `threshold=0.5` result. Use AUC/AUPRC as threshold-free metrics and use the recorded Youden threshold for ACC, MCC, sensitivity, specificity, precision, F1, and confusion-matrix counts.
- Formal summaries should not expose duplicate `_thr`, `.x`, or `.y` columns. New full reruns should write clean Step2 workbooks directly. If older `model_performance_comparison.xlsx` files contain `_thr` columns, `.x/.y` columns, or `_thr` rows in model sheets, run the threshold-column repair script so `_thr` values are merged into the ordinary metric names and the actual threshold is retained as `Train_Threshold`/`Test_Threshold`.
- Sensitivity and specificity should be visible in both cross-model summaries and single-model workbook details. If Step2 only wrote `Recall`, treat `Sensitivity = TP/(TP+FN)` and `Specificity = TN/(TN+FP)`, and repair the workbook so `混淆矩阵_Train/Test` plus each model sheet show both metrics explicitly.
- SHAP is for interpretation, not for model selection.
- Attempt SHAP for every Step2 model unless the project defines an exclusion rule. For GP01-style diagnostic work, skip only `AUC <= 0.5` or actual runtime failures.
- Prefer `scripts/run_mlr3_shap_all.sh` for batch SHAP in other projects. It should scan `run_model_*_step2` folders rather than hard-coding branch names. Project-level SHAP scripts should normally be thin wrappers only.
- For single-sample SHAP plots, support automatic case/control waterfall and force plots, explicit `SHAP_WATERFALL_SAMPLES` / `--waterfall_samples`, and `train:ID`, `test:ID`, `valid:ID` prefixes. If valid data are absent, record valid as skipped rather than failed.
- For `AUC=1` or near-perfect performance, add an explicit leakage/batch-effect warning instead of treating it as final biological proof.
- Separate computational completion from scientific conclusion. A project can be complete computationally while still requiring interpretation and validation.

## Workflow

1. Read the repo-local instructions and current project README/docs first.
2. Identify the intended case group, control group, positive class, formal result root, and expected branch count before running or summarizing models. If case/control is unclear, stop and resolve it before launching computation.
3. Audit inputs, Step1, Step2, model summaries, Step2 technical summaries, and SHAP summaries. If a completed run has the wrong positive class, mark it stale and rerun rather than only relabeling outputs.
4. Compare against `20260403_lilei`-style delivery:
   - entry README and reading order
   - modeling design and script provenance, including sample exclusions and input manifests
   - reproduction commands and parameters, including Step1 presets, CV, tuning, feature selection, and threshold policy
   - Step2/performance/SHAP reading guide, including where to inspect raw branch outputs
   - interpretation boundaries and Methods draft, including what not to conclude
   - input README and manifests
   - result README
   - model performance summary
   - Step2 technical summary
   - SHAP output summary
5. Patch missing docs or summary tables in the project, using actual outputs only. Prefer consolidated docs for new projects, but do not over-compress them; merge useful details from legacy docs into the consolidated set. Archive superseded long docs by default; if the user explicitly asks to clean up old files after consolidation, remove the redundant archived files and update README references accordingly.
6. If Step2 workbooks from an older run contain `_thr`, `.x`, or `.y` columns/rows, or if single-model sheets lack explicit `Sensitivity`/`Specificity`, run `Rscript scripts/repair_mlr3_threshold_columns.R --root=<result_root>` before generating final summaries. For newly rerun results, first verify that Step2 itself writes clean workbooks so the repair script is optional rather than required.
7. When batch SHAP is needed, prefer `scripts/run_mlr3_shap_all.sh <result_root>` with environment variables such as `SHAP_OUTDIR_NAME`, `SHAP_MODELS`, `SHAP_WATERFALL_GROUPS`, `SHAP_WATERFALL_SAMPLES`, and `SHAP_FORCE_PLOTS`. Keep project-specific SHAP runners as wrappers only.
8. When the project has separate model-performance, Step2, and SHAP summary scripts, prefer one repository-level, parameterized, project-neutral result-summary script, such as `scripts/summarize_mlr3_results.R`. It should discover `run_model_*_step2` folders from a supplied result root, optionally accept a branch map for display labels, support `--shap-dir-name` when multiple SHAP directories coexist, and support task selection such as `performance`, `step2`, `shap`, `manual`, and `all`. Project-specific files may remain as thin wrappers that pass the title, branch map, SHAP directory, and task flags for compatibility.
9. Run the audit script again and report remaining gaps separately from completed items.

## Quick Audit

```bash
python3 skills/mlr3-project-delivery/scripts/audit_mlr3_delivery.py \
  --project lina_20260515 \
  --result lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531 \
  --expected-branches 6 \
  --expected-models-per-branch 9
```

Interpret output conservatively:

- `OK`: present and structurally plausible.
- `WARN`: not necessarily a failure, but should be explained in the final delivery.
- `MISSING`: should be created or explicitly marked not applicable.
