# lina_20260515 GP01.HC-AF 正式交付入口

当前正式结果目录：

```text
lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531
```

本项目在 `GP01.HC-AF` 中构建 `AF` vs `HC` 诊断模型：非靶代谢、口腔细菌属、口腔真菌属、肠道细菌属、肠道真菌属 5 套数据分别建模，再用各单组学 Step2 入选 biomarkers 合并为 5 组学联合诊断模型。

## 必读顺序

1. `docs/01_建模设计与脚本来源.md`
   - 样本纳入、模型编号、5 个单组学和联合 biomarkers 模型设计、v0.2.2 脚本来源。

2. `docs/02_复现与参数.md`
   - 正式复现命令、Step1/Step2/Step3 参数、SMOTE 敏感性分析和汇总命令。

3. `docs/03_结果读取与技术汇总.md`
   - 模型性能表、Step2 特征选择/CV/调参、SHAP 输出和 generated flowchart 的读取边界。

4. `docs/04_结果解释边界与Methods.md`
   - Balanced_Score、0.5 阈值、SHAP、AUC=1 的解释边界，以及论文 Methods 中英文草稿。

5. `docs/05_v0.2.2管线修正与跨项目复用说明.md`
   - 本次 EFS、Excel 表头、CV ROC、SHAP 和汇总脚本升级记录，以及其他已分析项目如何复用。

核心结果优先读：

```text
lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531/01_model_performance_summary/README.md
lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531/01_model_performance_summary/MODEL_PERFORMANCE_SUMMARY.md
lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531/02_step2_technical_summary/README.md
lina_20260515/CURRENT_AF_POSITIVE_HANDOFF.md
```

SHAP 已完成并刷新，可直接读取：

```text
lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531/03_shap_output_summary/SHAP_OUTPUT_SUMMARY.md
lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531/RESULT_INTERPRETATION_MANUAL.md
```

## 目录结构

| 路径 | 用途 |
|---|---|
| `reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531/` | 当前正式全量结果 |
| `v0.2.2_inputs_gp01_hc_af/` | 当前正式输入矩阵、共同样本和 biomarker manifest |
| `scripts/` | 输入生成、正式运行、联合 biomarkers 输入生成、SHAP 批量运行和最终交付包装脚本 |
| `docs/` | 当前归类后的完整说明文档 |

## 当前口径

- 当前正式结果以 `AF` 为 positive/case class、`HC` 为 control class；旧方向结果、上一版 AF-positive 结果和中间 fullrerun 结果已删除，只保留当前正式结果目录。
- 当前 AF-positive 全量重跑结果已完成：6 个 Step1、6 个 Step2、54 条模型性能记录、54 个 SHAP 模型输出；formal SHAP 另产出 216 个 waterfall PDF 和 216 个 force PDF。
- Step1.1-Step1.5 的 EFS learner 已恢复为 `xgb,rf,svm,lgbm` 四个；Step2 最终模型比较仍为 9 种候选分类器。
- `model_performance_comparison.xlsx` 已由 Step2 主流程直接产出干净表头，`模型比较` sheet 不应再出现 `.x/.y/_thr` 列。
- `03_shap_output_summary/` 和 `RESULT_INTERPRETATION_MANUAL.md` 已刷新；train/test SHAP 均已完成，valid 因本项目无外部验证集记录为 `skipped`。
- 正式结果不声明唯一最优模型。
- `Balanced_Score` 只作为辅助展示列，不作为独立统计学或临床结论依据。
- 没有保留“0.5 作为最优分类临界点”的正式结果；`feature_fraction=0.5` 是特征比例参数，`AUC <= 0.5` 只是 SHAP 跳过规则。
- SHAP 用于解释已完成模型，不用于模型选择。

## 完成度复核

```bash
find lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531 \
  -path '*step3_shap_formal_case_control_force_20260526/*/*_SHAP.xlsx' | wc -l
```

按分支查看：

```bash
for d in lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531/run_model_*_step2; do
  printf '%s\t' "$(basename "$d")"
  find "$d/step3_shap_formal_case_control_force_20260526" -maxdepth 2 -name '*_SHAP.xlsx' 2>/dev/null | wc -l
done
```
