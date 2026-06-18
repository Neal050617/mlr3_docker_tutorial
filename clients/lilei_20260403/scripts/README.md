# scripts

本目录只保留 `20260403_lilei` 项目专用脚本。Step1/Step2/Step3 主流程、批量 SHAP 和统一汇总逻辑维护在仓库根目录 `scripts/`。

## 当前保留脚本

| 脚本 | 用途 |
|---|---|
| `prepare_v022_inputs_corrected.R` | 项目专用输入整理；按 `20260403_lilei` 样本映射表生成 `v0.2.2_inputs_corrected/` |
| `run_formal_analysis_corrected.sh` | 项目最终 wrapper；定义 0.1-1.6 分支、case-positive 分组和正式运行参数 |
| `run_shap_all_models_sensitivity.sh` | 项目 SHAP wrapper；默认指向当前正式 full rerun，并调用仓库级 `scripts/run_mlr3_shap_all.sh` 和 `scripts/summarize_mlr3_results.R` |

## 已归档脚本

旧版兼容统计脚本已移入：

```text
scripts/archive/20260527_legacy_summarizers/
```

这些脚本曾用于旧结果追溯；当前正式结果和后续项目统一使用仓库级：

```text
scripts/summarize_mlr3_results.R
scripts/run_mlr3_shap_all.sh
scripts/redraw_mlr3_shap_local_plots.R
```

## 常用命令

从仓库根目录运行完整分析：

```bash
GP_DIAG=LAEC-Con \
GP_RESPONSE=P_NRT-NP_NRT \
bash lina_20251208/20260403_lilei/scripts/run_formal_analysis_corrected.sh all \
  20260403_lilei/reproduce_formal_case_positive_manual
```

重新汇总当前正式结果：

```bash
Rscript scripts/summarize_mlr3_results.R \
  --root=lina_20251208/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156 \
  --branch-map=lina_20251208/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156/branch_map_delivery_for_summary.tsv \
  --project-name=20260403_lilei \
  --title=20260403_lilei \
  --shap-dir-name=step3_shap_formal_case_control_force_20260527 \
  --tasks=all
```

只重画 waterfall/force 单样本局部解释图，不重新计算 SHAP：

```bash
Rscript scripts/redraw_mlr3_shap_local_plots.R \
  --root=lina_20251208/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156 \
  --shap-dir-name=step3_shap_formal_case_control_force_20260527 \
  --local-display-features=10
```

重跑当前正式结果的 SHAP：

```bash
bash lina_20251208/20260403_lilei/scripts/run_shap_all_models_sensitivity.sh
```
