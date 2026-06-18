# GP01.HC-AF 统一结果说明书

本说明书由 `scripts/summarize_mlr3_results.R --tasks=manual` 或 `--tasks=all` 生成。

## 一次性总汇总命令

```bash
Rscript scripts/summarize_mlr3_results.R \
  --root=/Users/colinliu/Desktop/mlr3_docker_tutorial/lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531 \
  --branch-map=lina_20260515/v0.2.2_inputs_gp01_hc_af/input_summary.tsv \
  --title=GP01.HC-AF \
  --tasks=all
```

## 输出目录

| 模块 | 输出目录 | 作用 |
|---|---|---|
| 模型性能 | `01_model_performance_summary/` | 汇总所有 `run_model_*_step2` 的 AUC、AUPRC、ACC、MCC、Sensitivity、Specificity 和混淆矩阵 |
| Step2 技术设置 | `02_step2_technical_summary/` | 汇总每套模型的样本数、特征数、CV、调参次数、最终超参数 |
| SHAP 输出 | `03_shap_output_summary/` | 汇总每个分支、每个模型的 SHAP 完成状态和 SHAP Excel 路径 |

## 结果解读原则

- 不声明唯一最优模型。
- `Balanced_Score` 只作为工程化辅助展示列。
- 正式表格展示 AUC、AUPRC、ACC、MCC、Sensitivity、Specificity、TP、FP、TN、FN 和 Balanced_Score；其中 AUC/AUPRC 为无阈值指标，其余分类指标使用 Step2 记录的分类阈值。
- 分类阈值以各 Step2 输出记录为准；当前正式分析使用 Youden 阈值，不补充固定 `0.5` 分类阈值结果。
- SHAP 用于解释模型，不用于模型选择。
- AUC=1 模型应作为强分离信号处理，并在正式结论前排查样本泄漏、批次效应和外部验证条件。
