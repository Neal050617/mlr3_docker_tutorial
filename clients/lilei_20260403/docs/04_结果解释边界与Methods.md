# 04 结果解释边界与 Methods

## 当前判断

`20260403_lilei` 当前正式结果已经按 case-positive 方向完成 full rerun。计算层面当前状态为：

- 10 个 Step1 和 10 个 Step2 分支。
- 90 条模型性能记录。
- Step2 workbook 不再依赖旧 threshold 修复脚本。
- EFS 低/中维分支恢复为 `xgb,rf,svm,lgbm`；高维 SMOTE 分支按规则跳过 ensemble EFS，改走多方法特征选择。
- SHAP 已支持 case/control waterfall、显式样本选择、valid 口径和 force plot。
- 旧方向结果、上一版 case-positive 结果和中间 fullrerun 结果已删除。

当前更需要做的是基于 full rerun 结果控制解释边界，而不是继续修改建模脚本。

## 指标解释

正式展示建议至少包括：

- AUC
- AUPRC
- ACC
- MCC
- Sensitivity / Recall
- Specificity
- TP, FP, TN, FN
- Balanced_Score

`Balanced_Score` 只作为工程化辅助排序，不作为独立统计学终点，也不用于声明唯一最优模型。

## 阈值口径

Step2 使用 Youden 方法确定分类阈值。报告中不要把 `0.5` 误写为固定分类阈值。

需要保留的 `0.5` 主要是：

| 表述 | 含义 |
|---|---|
| `--feature_fraction 0.5` | 特征选择每轮保留比例 |
| `AUC <= 0.5` | SHAP 跳过低性能模型的技术阈值 |

## SHAP 解释边界

SHAP 用于解释已训练模型的特征贡献，不用于替代模型性能评估。当前正式 SHAP PDF 只对较长特征名自动换行，默认超过 28 个字符时优先在靠近中点的 `-` 后断为最多两行；短特征名不换行。waterfall 和 force 图适合展示单个样本的局部解释；当前正式局部图只展示每个样本 `|SHAP|` 最大的 Top 10 特征，其中 waterfall 为多行条形图，force 为较低的箭头形状单行正负累积贡献条并按正负方向对齐特征名。完整数值以 `*_SHAP.xlsx` 为准。若要做病例级叙述，应同时说明样本来自 train、test 还是 valid。

本项目没有外部 valid 数据，因此 valid 图跳过是数据条件导致的正常状态。

## Methods 草稿

本研究基于 `efs_valid_plus v0.2.2` 三步式流程进行二分类建模。首先对输入矩阵进行预处理和分组方向检查；随后在训练集中进行特征选择、嵌套交叉验证和超参数调优；最后基于完成训练的模型生成性能汇总和 SHAP 解释结果。二分类 positive class 由 `--gp` 参数第一个分组指定。本项目诊断分支使用 `LAEC-Con`，疗效相关分支使用 `P_NRT-NP_NRT`。

模型候选集包含 9 种分类器：正则化广义线性模型、XGBoost、随机森林、支持向量机、朴素贝叶斯、加权 k 近邻、单棵决策树、神经网络和 AdaBoost。模型调参采用随机搜索，内层 3-fold CV 用于调参，外层 5-fold CV 用于性能估计。固定 train/test 划分比例为 3/4，测试集用于最终泛化性能评估。

模型解释使用 SHAP。正式 SHAP 输出按模型性能规则筛选，并为 case/control 样本生成 waterfall 和 force plot；单样本局部图展示 Top 10 absolute SHAP contributors，waterfall 为多行局部贡献图，force 为较低的箭头形状单行累积贡献条，较长特征名按 28 字符阈值和靠近中点的 `-` 最多断为两行，完整贡献矩阵保留在 SHAP Excel 中。所有模型性能、Step2 技术设置和 SHAP 状态均由仓库级 `scripts/summarize_mlr3_results.R` 统一汇总。
