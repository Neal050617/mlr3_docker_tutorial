# mlr3 Project Delivery Checklist

Use this checklist to package projects in the same style as `lina_20251208/20260403_lilei`.

## 1. Project-Level Documents

Required:

- `README.md`: entrypoint, formal result directory, task summary, recommended reading order, key directories, reproduction commands.
- `docs/README.md`: index of documentation.
- Either the legacy detailed docs below or the consolidated core doc set:
  - `docs/01_建模设计与脚本来源.md`
  - `docs/02_复现与参数.md`
  - `docs/03_结果读取与技术汇总.md`
  - `docs/04_结果解释边界与Methods.md`
  - optional but recommended for pipeline migrations: `docs/05_v0.2.2管线修正与跨项目复用说明.md`
- In consolidated mode, archive superseded long docs under `docs/archive/` by default. If the user explicitly asks to remove old files after their contents have been merged, delete the redundant archive files and update README references.
- Consolidated docs should still carry enough detail for reuse: sample inclusion/exclusion, input files, script provenance, Step1 presets, CV/tuning settings, feature-selection policy, result-reading paths, interpretation boundaries, and Methods text.
- Before modeling starts, document the intended case group, control group, positive class, explicit group order, and map-file counts. Treat this as a launch gate: if any item is unknown, do not run Step1/Step2 yet. In this pipeline Step2 sets `positive = gp[1]`; diagnostic case-positive analyses must pass an explicit group order such as `--gp AF-HC` rather than relying on map file row order.
- `scripts/README.md`: script roles and run order.
- Result-summary scripts should be consolidated where practical: one repository-level, parameterized, project-neutral script such as `scripts/summarize_mlr3_results.R` should accept a result root, discover `run_model_*_step2` folders, optionally accept a branch map for labels, support `--shap-dir-name`, and generate model performance, Step2 technical, SHAP, and manual outputs by task flag. Project-level scripts may remain only as thin wrappers for default title, branch map, SHAP directory, and task selection.
- Step3 SHAP batch runners should be consolidated where practical: one repository-level script such as `scripts/run_mlr3_shap_all.sh` should accept a result root, discover `run_model_*_step2/efs.RData`, and support environment variables for output directory, model subset, train/test/valid datasets, case/control waterfall samples, explicit sample IDs, and force plots. Project-level SHAP scripts may remain only as thin wrappers.
- If Step2 output workbooks from older runs contain duplicate threshold metrics such as `_thr` columns in `模型比较`, `.x/.y` columns, or `_thr` rows in each model sheet, run `scripts/repair_mlr3_threshold_columns.R` before final summaries. The repaired workbook should keep `Train_Threshold`/`Test_Threshold`, merge threshold-based ACC/CE/Precision/Recall/F1/MCC into ordinary metric names, remove `_thr` labels, and explicitly show `Sensitivity` and `Specificity` in `混淆矩阵_Train/Test` plus every single-model sheet. New full reruns should write clean workbooks directly, so the repair script should be optional rather than required.

Optional but useful:

- version evolution docs such as `efs_valid版本演进说明.md` and `v0.2.2与v0.2.1区别说明.md` when comparing pipeline versions.
- Legacy detailed docs such as `docs/建模计划_*.md`, `docs/参数设置教程_*.md`, `docs/Step2特征选择_CV_调参查看说明.md`, `docs/FORMAL_REPRODUCTION.md`, and `docs/METHODS_FOR_MANUSCRIPT_CN_EN.md` when the project needs a more verbose handoff.

## 2. Input Directory

Required:

- `README.md`: input generation source and intended model branches.
- `input_summary.tsv`: one row per model input matrix.
- case/control statement or manifest field when the analysis is binary diagnostic.
- model input matrices named consistently, for example `model_0.1_*.tsv`.
- sample mapping/cohort files when the project has cohort restrictions.
- feature or biomarker manifests for multi-omics inputs.
- missing-sample reports when sample exclusion occurred.

For joint biomarker models:

- state whether inputs are full concatenation or selected biomarkers.
- include `joint_selected_biomarkers_manifest.tsv` and a summary by source modality.

## 3. Formal Result Directory

Required:

- `README.md`: result reading order, analysis scope, metric interpretation, warnings, reproduction commands.
- `RESULT_INTERPRETATION_MANUAL.md`: single result manual generated after all analyses and summaries are complete.
- all expected `run_model_*_step1` and `run_model_*_step2` directories.
- each Step2 branch: `efs.RData`, `model_performance_comparison.xlsx`, `analysis_flowchart.md`, run logs if produced.
- `01_model_performance_summary/` containing:
  - `README.md` as an integrated result-reading hub when Step2 technical and SHAP summaries live as sibling directories
  - Markdown summary
  - all-model TSV
  - branch-level sorted TSVs
  - Excel workbook
- confusion-matrix-derived metrics if available
  - explicit sensitivity and specificity in both summary tables and single-model workbook details

Recommended:

- `02_step2_technical_summary/` containing:
  - `README.md`
  - `step2_technical_summary.tsv`
  - `step2_best_hyperparameters.tsv`
  - `step2_technical_summary.xlsx`

When SHAP is run:

- `03_shap_output_summary/SHAP_OUTPUT_SUMMARY.md`
- `03_shap_output_summary/shap_status_summary.tsv`
- `03_shap_output_summary/shap_model_files.tsv`
- `03_shap_output_summary/shap_output_summary.xlsx` when an Excel writer is available
- each SHAP branch has `step3_shap_status.tsv` and `step3_shap_summary.xlsx`
- waterfall and force outputs are present when requested
- valid rows are marked `skipped` when no valid dataset exists, not treated as failures
- if multiple SHAP directories coexist, final summaries use the intended directory via `--shap-dir-name` or a project wrapper variable such as `SHAP_SUMMARY_DIR_NAME`

## 4. Result Interpretation

Report, at minimum:

- positive class and control class
- AUC
- AUPRC
- threshold used for classification, when applicable
- ACC
- MCC
- sensitivity
- specificity
- TP, FP, TN, FN
- confusion matrix
- Balanced_Score only as auxiliary display if present

Threshold policy:

- If the Step2 run used `threshold=youden`, do not add a separate fixed `threshold=0.5` result table.
- Treat AUC/AUPRC as threshold-free metrics.
- Treat ACC, MCC, sensitivity, specificity, precision, F1, TP, FP, TN, and FN as threshold-based metrics under the recorded Step2 threshold.
- Do not expose `_thr` suffixes in formal summaries; use ordinary metric names after the threshold policy is fixed.
- If only recall is present in a raw workbook, report it as recall/sensitivity but also compute specificity from the confusion matrix before formal delivery.

Avoid:

- claiming a single best model solely from `Balanced_Score`
- ranking biological conclusions by SHAP completion
- treating `AUC=1` as final proof without leakage and batch-effect checks

Use wording like:

> 本结果目录不声明唯一最优模型。模型表现以完整指标表展示；Balanced_Score 仅作为辅助展示列，不作为独立科学结论依据。

For SHAP:

> SHAP 用于解释已完成模型，不用于定义最优模型；未运行或失败的模型应在状态表中记录。

## 5. Completion vs Conclusion

State these separately:

- Computational completion: expected branches ran, output files exist, summaries generated, SHAP status known.
- Scientific interpretation: requires reviewing metric stability, class imbalance, confusion matrices, feature plausibility, leakage risk, batch effects, and external validation availability.
