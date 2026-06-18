# 04 结果解释边界与 Methods

## 当前判断

GP01.HC-AF 主线已经按 AF-positive 方向重跑。计算层面当前状态为：

- 6 个 Step1 和 6 个 Step2 分支。
- 54 条模型性能记录。
- `AUC <= 0.5` 的模型数为 0。
- `01_model_performance_summary/`、`02_step2_technical_summary/` 和 `03_shap_output_summary/` 已补齐。
- formal SHAP 已完成：54 个 SHAP Excel、216 个 waterfall PDF 和 216 个 force PDF。
- Step1.1-Step1.5 的 EFS learner 已恢复为 `xgb,rf,svm,lgbm` 四个。
- Step2 workbook 已由主流程直接产出干净表头，`模型比较` sheet 不应再出现 `.x/.y/_thr` 列。

旧方向结果、上一版 AF-positive 结果和中间 fullrerun 结果已删除。当前正式结果目录是：

```text
lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531
```

当前更需要做的是基于 fullrerun 结果控制解释边界，而不是在旧方向结果上继续优化。

## 与 20260403_lilei 更新口径的关系

当前项目需要吸收 `20260403_lilei` 更新后的解释原则，但不需要照搬其任务策略。

| 项目 | `20260403_lilei` | `lina_20260515` |
|---|---|---|
| 任务 | 诊断、疗效预测、动态差值分析 | GP01.HC-AF 诊断 |
| 分支数 | 10 个左右，任务类型更多 | 6 个固定分支 |
| 类别不平衡 | 部分疗效分支更明显 | `HC=46, AF=99`，约 2.15:1 |
| SHAP 口径 | 低性能模型可 eligible-only | 当前 formal SHAP 已完成 54 个模型；失败或跳过仍需记录状态 |
| 结果结论 | 不用 Balanced_Score 声明唯一最优模型 | 同样不声明唯一最优模型 |

因此，当前 GP01.HC-AF 不应再使用旧方向结果；需要做的是基于 AF-positive 结果保持完整指标展示、控制 AUC=1 的解释边界，并把 SHAP 定位为解释资源。

## 0.5 阈值口径

当前正式交付不保留“0.5 作为最优分类临界点”的结果结论。

需要保留的 `0.5` 只有两类技术含义：

| 表述 | 是否保留 | 含义 |
|---|---|---|
| `--feature_fraction 0.5` | 保留 | Step2 特征选择比例参数 |
| `AUC <= 0.5` | 保留 | SHAP 跳过低性能模型的技术门槛 |

Step2 生成的 `analysis_flowchart.md` 中 `threshold = youden` 表示当前流程使用 Youden 方法确定分类阈值，不是固定 0.5 阈值。

另一个容易误读的位置是：

```text
02_step2_technical_summary/step2_best_hyperparameters.tsv
```

该表第一列 `Branch_ID` 中出现的 `0.5` 表示 `Model 0.5` 肠道真菌属分支编号，不是临界点或阈值参数。

## Balanced_Score

当前项目不使用 `Balanced_Score` 声明唯一最优模型。

正式展示应包括：

- AUC
- AUPRC
- ACC
- MCC
- Sensitivity / Recall
- Specificity
- TP, FP, TN, FN
- Balanced_Score

`Balanced_Score` 仅作为工程化辅助展示列，不作为独立统计学或临床终点。当前公式为：

```text
0.45 * Test_AUC + 0.35 * max(Test_MCC, -0.25) + 0.15 * Test_ACC + 0.05 * Test_AUPRC
```

## SHAP

当前 54 个模型均 `AUC > 0.5`，原则上每个模型都应尝试 SHAP。若某个模型因为运行失败、模型对象不可预测或其他技术原因未完成，应在状态表中记录，而不是把 SHAP 失败解释为模型优劣。

正式写作时应注意：

- SHAP 用于解释模型，不用于定义最优模型。
- 对低性能或退化模型的 SHAP 不应过度解释。
- 如果需要论文主图，建议优先从表现稳定、混淆矩阵合理、MCC 较高的候选模型中选取 SHAP 图。
- 当前批量 SHAP 使用中等抽样参数，适合形成完整解释资源；若用于最终发表图片，可只对少数重点模型用更高 `nsim` 重新生成解释图。
- 单样本解释图默认按 case/control 各选 1 个样本输出 waterfall 和 force 图；当前正式局部图只展示每个样本 `|SHAP|` 最大的 Top 10 特征，其中 waterfall 为多行局部贡献图，force 为较低的箭头形状单行正负累积贡献条。importance、beeswarm、waterfall 和 force 只对较长特征名自动换行，默认超过 28 个字符时优先在靠近中点的 `-` 后断为最多两行；短特征名不换行。完整贡献矩阵保留在 SHAP Excel 中。如需指定样本，应在重跑 SHAP 时通过 `SHAP_WATERFALL_SAMPLES` 或 `--waterfall_samples` 明确给出样本 ID。

## AUC=1 风险

当前性能表显示：

```text
模型总数 = 54
AUC <= 0.5 = 0
AUC = 1 = 17
Recall = 0 = 0
Specificity = 0 = 0
```

需要重点关注：

- 口腔真菌属分支 9 个模型均 `AUC=1`。
- 5 组学 biomarkers 联合分支 8 个模型 `AUC=1`。
- 当前 AF-positive 汇总中未见 `Recall=0` 或 `Specificity=0` 的测试集退化模型。

口腔真菌属和联合 biomarkers 分支应作为强分离信号处理，但正式结论前必须排查样本泄漏、批次效应、测序/检测批次与分组是否绑定，以及特征是否直接或间接编码样本来源或分组信息。

## 结果解读建议

正式结果不建议写成“某一个模型为最优模型”。更稳妥的写法是按分支描述表现范围，并把候选模型放在表格中展示：

- 对每个单组学和联合模型，展示 9 种算法的 AUC、AUPRC、ACC、MCC、Sensitivity、Specificity 和混淆矩阵。
- 对明显退化模型单独标注；若后续敏感性分析出现 `Recall=0` 或 `Specificity=0`，不应作为重点解释模型。
- 对 `AUC=1` 的分支不直接写成生物学定论，而应写为“强分离信号”，并补充泄漏、批次和外部验证限制。
- 对联合模型，需要同时报告 `joint_selected_biomarkers_manifest.tsv` 中 biomarker 的组学来源构成。
- SHAP 图只用于说明模型特征贡献，不用于证明模型优劣。

## 什么时候才需要重跑

只有出现以下情况，才建议另开敏感性分析或重跑：

- 发现输入矩阵或分组文件有错误。
- 发现批次效应或样本泄漏，需要剔除问题特征或样本后重跑。
- 审稿或内部评审要求 SMOTE 敏感性分析。
- 需要验证当前 `AUC=1` 是否对划分方式敏感。
- 需要生成发表级 SHAP 图，且当前轻量抽样图不够稳定。

建议新建目录，不覆盖当前正式结果，例如：

```text
lina_20260515/reproduce_gp01_hc_af_smote
lina_20260515/reproduce_gp01_hc_af_batch_checked
lina_20260515/reproduce_gp01_hc_af_high_nsim_shap
```

## 用于文章发表的中英文 Methods 描述

以下文字按文章 Methods 风格整理，可直接作为初稿使用。正式投稿前仍应按目标期刊格式压缩或扩展。

### 中文

本研究在 GP01.HC-AF 队列中构建 AF 与 HC 的二分类诊断模型。原始数据包括非靶代谢、口腔细菌属、口腔真菌属、肠道细菌属和肠道真菌属 5 类组学矩阵。原始分组文件包含 147 个样本，其中 HC 48 例、AF 99 例。由于口腔真菌数据缺少 HC20、肠道真菌数据缺少 HC37，正式多组学比较统一使用 145 个五组学共同样本，其中 HC 46 例、AF 99 例。

每类组学首先分别建立单组学诊断模型。非靶代谢使用 `metabolome_standard` 预处理流程，微生物属水平数据使用 `microbiome_standard` 预处理流程。随后使用 v0.2.2 三步式机器学习流程进行特征选择、嵌套交叉验证、模型调参和测试集评估。候选分类器包括 glmnet、xgboost、随机森林、支持向量机、朴素贝叶斯、加权 k 近邻、单棵决策树、AdaBoost 和单隐层神经网络。模型训练采用 3/4 训练集和 1/4 测试集划分，外层 5 折交叉验证用于模型评估，内层 3 折交叉验证用于超参数调优；每个模型的调参评估次数为 20，调参批次大小为 5。特征选择采用 Boruta、RFE、LASSO 和 XGBoost 等方法的多数策略整合；其中口腔细菌属分支实际使用 Boruta、RFE 和 XGBoost 参与多数策略整合。目标特征数设置为 15，但最终入选特征数由多数策略结果决定，因此各分支最终入选特征数为 17-22 个不等。当前分析未使用外部验证集，也未在主线分析中启用 SMOTE。

在完成 5 个单组学模型后，从各单组学 Step2 结果中的最终入选特征中提取 biomarkers，并合并为一个 5 组学 biomarkers 联合输入矩阵，再使用 `mixed_omics` 预处理流程构建联合诊断模型。模型性能根据测试集 AUC、AUPRC、准确率、MCC、Sensitivity、Specificity 和混淆矩阵进行展示。分类阈值采用 Youden 方法确定，不使用固定 0.5 作为最优临界点。考虑到 HC 与 AF 样本存在轻中度不平衡，本研究不基于单一综合分数声明唯一最优模型，而是完整报告各模型指标。Balanced score 仅作为辅助展示指标，不作为独立科学结论依据。SHAP 分析用于解释已完成模型，不用于模型选择。

### English

Binary diagnostic models for AF versus HC were developed in the GP01.HC-AF cohort. Five omics data types were included: untargeted metabolomics, oral bacterial genera, oral fungal genera, gut bacterial genera, and gut fungal genera. The original grouping file contained 147 samples, including 48 HC and 99 AF samples. Because HC20 was missing from the oral fungal table and HC37 was missing from the gut fungal table, the formal multi-omics comparison used the 145 samples shared by all five omics datasets, including 46 HC and 99 AF samples.

Single-omics diagnostic models were first constructed separately for each omics data type. Untargeted metabolomics data were processed using the `metabolome_standard` preset, whereas genus-level microbial abundance tables were processed using the `microbiome_standard` preset. Feature selection, nested cross-validation, hyperparameter tuning, and test-set evaluation were then performed using the modular v0.2.2 machine-learning workflow. Candidate classifiers included glmnet, xgboost, random forest, support vector machine, naive Bayes, weighted k-nearest neighbors, single decision tree, AdaBoost, and a single-hidden-layer neural network. Models were trained using a 3/4 training split and evaluated on the held-out test set. A 5-fold outer cross-validation scheme was used for model evaluation, and a 3-fold inner cross-validation scheme was used for hyperparameter tuning. Each model used 20 tuning evaluations with a tuning batch size of 5. Feature selection was based on a majority-vote integration of Boruta, recursive feature elimination, LASSO, and XGBoost-based selection, except for the oral bacterial branch where Boruta, recursive feature elimination, and XGBoost-based selection were used. The target number of features was set to 15, but the final number of selected features was determined by the majority-vote rule and ranged from 17 to 22 across branches. No external validation set or SMOTE resampling was used in the main analysis.

After the five single-omics models were completed, the final selected features from each single-omics Step2 result were extracted as biomarkers and merged into a five-omics biomarker matrix. This combined biomarker matrix was then processed using the `mixed_omics` preset and used to train the joint diagnostic model. Model performance was reported using test-set AUC, AUPRC, accuracy, MCC, sensitivity, specificity, and confusion matrices. Classification thresholds were determined using the Youden method rather than a fixed 0.5 cutoff. Because the HC and AF groups were moderately imbalanced, no single overall score was used to claim a unique best model. The balanced score was retained only as an auxiliary descriptive metric and was not treated as an independent scientific criterion. SHAP analysis was used for model interpretation and was not used for model selection.
