# Git index cleanup

Date: 2026-06-17

The repository policy is to keep reproducibility assets in Git and keep generated analysis results out of Git.
This cleanup removed only clear generated modeling outputs from the Git index.
The files themselves were not deleted from the working tree.

Command class used:

```bash
git rm --cached --ignore-unmatch -- <clear-generated-output-files>
```

## Removed from Git index

```text
daixie/efs123/model_performance_comparison.xlsx
daixie/model_analysis_results.xlsx
daixie/model_performance_comparison.xls
daixie/model_performance_comparison.xlsx
data_for_analysis/20250211-mlr-JINYU/1-首次分析结果.pptx
data_for_analysis/20250211-mlr-JINYU/analysis_report/data_report.html
data_for_analysis/20250211-mlr-JINYU/test2.2.efs.clinic/model_analysis_results.xlsx
data_for_analysis/20250211-mlr-JINYU/test2.2.efs.clinic/model_performance_comparison.xls
data_for_analysis/20250211-mlr-JINYU/test2.2.efs.tNGS/model_analysis_results.xlsx
data_for_analysis/20250211-mlr-JINYU/test2.2.efs.tNGS/model_performance_comparison.xls
data_for_analysis/20250211-mlr-JINYU/建模结果.zip
data_for_analysis/20250211-mlr-JINYU/弹性网络-tNGS/model_report.xlsx
data_for_analysis/20250211-mlr-JINYU/弹性网络-临床诊断-tNGS/model_report.xlsx
data_for_analysis/20250211-mlr-JINYU/弹性网络-临床诊断/model_report.xlsx
data_for_analysis/20250211-mlr-JINYU/柱形图/model_report.xlsx
data_for_analysis/20250211-mlr-JINYU/柱形图/柱形图-修订.zip
data_for_analysis/20250211-mlr-JINYU/柱形图/柱形图-修订2.zip
data_for_analysis/20250211-mlr-JINYU/柱形图/柱形图-修订3.zip
lina/efs123/model_performance_comparison.xlsx
```

## Deferred for input review

The remaining tracked `.xls` / `.xlsx` files are listed in:

```text
docs/audit/20260612_sync_audit/tracked_large_generated_files_in_git.txt
```

They were left tracked for now because several look like raw abundance tables, parameter sheets, or input handoff sheets.
