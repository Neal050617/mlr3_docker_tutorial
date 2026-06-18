# SHAP 输出汇总

分析目录：`/workspace/lina_20251208/20260403_lilei/reproduce_formal_v006_msi12_name_20260609_000808`

本文件由 `scripts/summarize_mlr3_results.R --tasks=shap` 自动生成。

## 读取原则

- SHAP 用于模型解释，不用于声明“最优模型”。
- 当前汇总按每个 Step2 分支读取 `step3_shap_status.tsv` 和各模型 `*_SHAP.xlsx` 文件。
- `AUC <= 0.5` 是 SHAP 跳过低性能模型的技术门槛，不是分类临界点。
- SHAP PDF 图只对较长特征名自动换行，默认超过 28 个字符时优先在靠近中点的 `-` 后断为最多两行；短特征名不换行，importance、beeswarm、waterfall 和 force 均使用该规则以减少图形区域被挤压。
- waterfall/force PDF 是单样本局部解释图；waterfall 以多行条形展示 Top 10 `|SHAP|` 特征；force 以较低的箭头形状单行正负累积贡献条展示同一批 Top 10 特征，SHAP 值标在箭头内，Positive 特征名按箭头起点对齐，Negative 特征名按箭头尾部右对齐，完整 SHAP 数值仍以各模型 `*_SHAP.xlsx` 为准。

## 输出文件

- `shap_status_summary.tsv`：每个分支一行，记录 SHAP 完成状态。
- `shap_model_files.tsv`：每个模型一行，记录 SHAP Excel 文件路径。
- `shap_output_summary.xlsx`：以上两个表的 Excel 版本。

## 当前汇总

- 分支数：10
- SHAP 模型文件数：84
- 未完成状态行数：12
