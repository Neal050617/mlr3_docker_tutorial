# mlr3 项目交付整理规则

本文档用于在另一个终端继续整理同类 mlr3/R 多组学建模项目。目标是把项目整理成可复现、可阅读、可审计的交付包，而不是只保留原始 `run_model_*` 文件夹。

## 1. 先确认入口

在仓库根目录执行。先确认项目目录、正式结果目录、输入目录：

```bash
export PROJECT=lina_20260515
export RESULT=lina_20260515/reproduce_gp01_hc_af_af_positive_20260522_100732
export BRANCH_MAP=lina_20260515/v0.2.2_inputs_gp01_hc_af/input_summary.tsv
export TITLE=GP01.HC-AF
export CASE_GROUP=AF
export CONTROL_GROUP=HC
```

其他项目替换这些变量。正式结果目录应包含 `run_model_*_step1/` 和 `run_model_*_step2/`。

## 2. 建模前确认 case/control

不能在结果完成后才倒推 case/control。二分类诊断模型必须在建模开始前确认：

- case/阳性类是什么。
- control/阴性类是什么。
- map 文件中 case/control 的样本数是否符合预期。
- Step1/Step2 是否显式传入了正确顺序。

这一步是建模启动门禁；任何一项不明确时，不启动 Step1/Step2。

当前 mlr3 Step2 使用 `positive = gp[1]`，因此如果 AF 是 case、HC 是 control，必须使用：

```bash
--gp AF-HC
```

若已完成结果发现 positive class 反了，应标记旧结果为方向错误并重跑，不建议只改表头或解释文字。

## 3. 清理 Step2 阈值冗余列

如果 Step2 原始 `model_performance_comparison.xlsx` 中有 `_thr` 后缀列或模型详情 sheet 中有 `ACC_thr`、`Recall_thr` 等行，先运行：

```bash
Rscript scripts/repair_mlr3_threshold_columns.R \
  --root="$RESULT"
```

修复后的规则：

- `threshold=youden` 时，不再补充固定 `threshold=0.5` 的结果。
- AUC、AUPRC 是无阈值指标。
- ACC、MCC、Sensitivity、Specificity、Precision、F1、TP、FP、TN、FN 使用 Step2 记录的分类阈值。
- `_thr` 指标合并回正式无后缀列名。
- 原始 workbook 保留 `Train_Threshold` 和 `Test_Threshold`。
- `Sensitivity = TP/(TP+FN)`，与当前表中的 `Recall` 等价，但正式表应显式展示。
- `Specificity = TN/(TN+FP)`，必须补写到 `混淆矩阵_Train/Test` 和每个单模型 sheet，不应只存在于 54 模型总表。

检查是否清干净：

```bash
Rscript -e 'suppressPackageStartupMessages(library(openxlsx)); files <- list.files(Sys.getenv("RESULT"), pattern="model_performance_comparison.xlsx", recursive=TRUE, full.names=TRUE); hits <- list(); for (f in files) for (s in getSheetNames(f)) { x <- read.xlsx(f, sheet=s, colNames=FALSE); if (any(grepl("_thr", as.character(unlist(x)), fixed=TRUE), na.rm=TRUE)) hits[[paste(basename(dirname(f)), s, sep="/")]] <- TRUE }; if (length(hits)) { print(names(hits)); quit(status=1) } else cat("No _thr text\n")'
```

如果没有导出环境变量，可把 `Sys.getenv("RESULT")` 改成实际结果目录字符串。

## 4. 生成统一汇总

使用仓库级通用脚本，不再为每个项目维护多套 `summarize_*` 脚本：

```bash
Rscript scripts/summarize_mlr3_results.R \
  --root="$RESULT" \
  --branch-map="$BRANCH_MAP" \
  --title="$TITLE" \
  --tasks=all
```

输出应包括：

```text
01_model_performance_summary/
02_step2_technical_summary/
03_shap_output_summary/
RESULT_INTERPRETATION_MANUAL.md
```

正式性能表不声明唯一最优模型。`Balanced_Score` 仅作为辅助展示列，不作为科学结论依据。

## 5. 文档结构

项目目录建议只保留当前正式阅读路径：

```text
README.md
docs/README.md
docs/01_建模设计与脚本来源.md
docs/02_复现与参数.md
docs/03_结果读取与技术汇总.md
docs/04_结果解释边界与Methods.md
scripts/README.md
```

旧版长文档默认先归档到 `docs/archive/`。如果用户明确要求删除旧文件，并且内容已经合并进 4 份正式文档，可以删除 archive 并同步 README。

## 6. 解读边界

正式解读必须写清：

- 不声明唯一最优模型。
- 明确写出 positive/case class 和 control class。
- 不用 `Balanced_Score` 作为独立科学结论。
- 不补充固定 `0.5` 分类阈值结果。
- SHAP 用于解释模型，不用于模型选择。
- 每个 Step2 的 9 个模型尽量做 SHAP；若 `AUC <= 0.5` 或运行失败，应在状态表中记录。
- `AUC=1` 或接近 1 的模型作为强分离信号处理，正式结论前需提示样本泄漏、批次效应和外部验证风险。

## 7. 审计

整理完成后运行：

```bash
python3 skills/mlr3-project-delivery/scripts/audit_mlr3_delivery.py \
  --project "$PROJECT" \
  --result "$RESULT" \
  --expected-branches 6 \
  --expected-models-per-branch 9
```

不同项目按实际分支数和每分支模型数调整参数。

再校验 skill：

```bash
python3 /Users/colinliu/.codex/skills/.system/skill-creator/scripts/quick_validate.py \
  skills/mlr3-project-delivery
```

## 8. 给另一个终端的简短提示词

```text
请使用 skills/mlr3-project-delivery 的规则整理当前 mlr3 项目。先确认 PROJECT、RESULT、BRANCH_MAP、TITLE、CASE_GROUP、CONTROL_GROUP；建模开始前必须确认 case/control，当前管线 positive=gp[1]，诊断模型要显式传 --gp <case>-<control>，不能结果完成后再倒推。GP01 当前正式结果是 lina_20260515/reproduce_gp01_hc_af_af_positive_20260522_100732，AF 是 case/positive，HC 是 control；旧目录 reproduce_gp01_hc_af_20260519_155548_session 不再作为正式结果入口。若 Step2 的 model_performance_comparison.xlsx 中有 _thr 列或 _thr 行，先运行 scripts/repair_mlr3_threshold_columns.R 清理，并确认混淆矩阵 sheet 和每个单模型 sheet 都有 Sensitivity、Specificity；再运行 scripts/summarize_mlr3_results.R --tasks=all 生成 01/02/03 汇总和 RESULT_INTERPRETATION_MANUAL.md。正式汇总不补充 threshold=0.5，不展示 _thr 后缀，不声明唯一最优模型，Balanced_Score 仅作辅助展示。最后运行 audit_mlr3_delivery.py 审计并报告剩余问题。
```
