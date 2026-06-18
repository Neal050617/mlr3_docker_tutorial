# 20260403_lilei 中文文档入口

本目录当前正式结论必须以 case-positive full rerun 为准：

```text
lina_20251208/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156
```

旧方向结果、上一版 case-positive 结果和中间 fullrerun 结果已删除；当前目录不再保留这些旧分析结果作为正式入口。原先较分散的说明文件已移入：

```text
lina_20251208/20260403_lilei/docs/archive/20260527_pre_numbered_docs/
```

新的说明文件结构参考 `lina_20260515/docs`，只保留 01-05 编号文档作为当前交付入口。

## 推荐阅读顺序

1. `docs/01_建模设计与脚本来源.md`
2. `docs/02_复现与参数.md`
3. `docs/03_结果读取与技术汇总.md`
4. `docs/04_结果解释边界与Methods.md`
5. `docs/05_v0.2.2管线修正与跨项目复用说明.md`
6. `reproduce_formal_case_positive_fullrerun_20260527_0156/RESULT_INTERPRETATION_MANUAL.md`
7. `reproduce_formal_case_positive_fullrerun_20260527_0156/01_model_performance_summary/MODEL_PERFORMANCE_SUMMARY.md`
8. `reproduce_formal_case_positive_fullrerun_20260527_0156/02_step2_technical_summary/README.md`
9. `reproduce_formal_case_positive_fullrerun_20260527_0156/03_shap_output_summary/SHAP_OUTPUT_SUMMARY.md`

## 当前目录结构

| 路径 | 用途 |
|---|---|
| `reproduce_formal_case_positive_fullrerun_20260527_0156/` | 当前唯一正式 case-positive full rerun 结果 |
| `v0.2.2_inputs_corrected/` | 当前正式输入数据 |
| `scripts/` | 项目专用输入整理、正式运行和 SHAP wrapper |
| `docs/` | 当前 01-05 交付说明；旧说明在 `docs/archive/` |
| `1Gp01.Con-LAEC/` | 原始 Con-LAEC 菌群数据 |
| `2GP.1.NP_NRT-P_NRT/` | 原始 NRT 前菌群数据 |
| `2GP.2.NP_RT-P_RT/` | 原始 RT 后菌群数据 |
| `李磊-食管癌项目-多组学样本及临床信息表20251223.xlsx` | 样本映射和临床信息 |
| `代谢物鉴定定量列表_0107.xlsx` | 原始代谢组数据 |

## Positive Class

当前医学定义固定为：

| 分支 | control/negative | case/positive | `--gp` |
|---|---|---|---|
| `0.1-0.3` | `Con` | `LAEC` | `LAEC-Con` |
| `1.0-1.6` | `NP_NRT` | `P_NRT` | `P_NRT-NP_NRT` |

`efs_valid_plus` Step2 将 `--gp` 的第 1 个组作为 positive class。正式解释不要使用旧方向结果或旧表头修正文档。

## 复现命令

从仓库根目录运行：

```bash
GP_DIAG=LAEC-Con \
GP_RESPONSE=P_NRT-NP_NRT \
bash lina_20251208/20260403_lilei/scripts/run_formal_analysis_corrected.sh all \
  20260403_lilei/reproduce_formal_case_positive_manual
```

当前正式目录汇总：

```bash
Rscript scripts/summarize_mlr3_results.R \
  --root=lina_20251208/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156 \
  --branch-map=lina_20251208/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156/branch_map_delivery_for_summary.tsv \
  --project-name=20260403_lilei \
  --title=20260403_lilei \
  --shap-dir-name=step3_shap_formal_case_control_force_20260527 \
  --tasks=all
```
