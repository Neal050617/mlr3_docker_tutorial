# 机器学习建模项目 - 代谢组学数据分析

## 项目简介

本项目是一个基于mlr3框架的机器学习建模工具，专门用于代谢组学数据的特征选择和模型比较。项目采用集成特征选择(Ensemble Feature Selection, EFS)方法，并集成了多种机器学习算法进行性能比较。

## 主要功能

### 1. 数据预处理
- 自动移除常量特征和低方差特征
- 缺失值处理（连续变量使用直方图填补，分类变量使用众数填补）
- 可选的数据归一化处理
- 支持用户自定义特征选择和排除

### 2. 集成特征选择 (EFS)
- 使用递归特征消除(RFE)作为基础选择器
- 集成多种学习器：XGBoost、随机森林、SVM、LightGBM
- 特征稳定性评估（Jaccard相似度）
- 自动识别最优特征数量（knee point detection）

### 3. 模型训练与优化
支持的机器学习算法：
- **XGBoost**: 梯度提升树算法
- **随机森林 (Random Forest)**: 集成学习算法
- **支持向量机 (SVM)**: 支持线性和非线性核函数
- **LightGBM**: 轻量级梯度提升框架
- **Glmnet**: 弹性网络回归

每个模型都配置了自动超参数优化：
- 使用随机搜索策略
- 5折交叉验证
- 多种终止条件（性能停滞、最大评估次数、时间限制）

### 4. 性能评估
- **交叉验证评估**: 5折CV评估
- **训练集评估**: 内部性能评估
- **测试集验证**: 外部独立验证
- **评估指标**: AUC、准确率、分类错误率

### 5. 结果可视化
- ROC曲线比较（训练集、测试集、交叉验证）
- 特征重要性可视化
- 模型性能对比图表
- EFS过程可视化（性能、特征数量、帕累托前沿、稳定性）

## 使用方法

### 基本用法
```bash
Rscript efs.R -i input_data.xls -g group_file.txt -o output_directory
```

### 主要参数说明

#### 输入文件参数
- `-i, --input`: 输入数据文件（默认: "new.all.metabo-env.xls"）
- `-g, --map`: 分组文件（默认: "map-group.txt"）
- `-c, --color`: 颜色配置文件（可选）

#### 数据划分参数
- `--part`: 训练测试集划分比例（默认: "2/3"）
- `--split`: 自定义划分文件（可选）
- `--inner_cv`: 内层交叉验证折数（默认: 5）
- `--outer_cv`: 外层交叉验证折数（默认: 5）
- `--resample`: 重抽样次数（默认: 5）

#### 特征选择参数
- `--n_features`: RFE算法选择的特征数量（默认: 10）
- `--feature_fraction`: RFE每次迭代保留的特征比例（默认: 0.85）

#### 其他参数
- `-u, --unif`: 是否进行归一化（默认: FALSE）
- `--select`: 指定选择的特征列表文件
- `--delete`: 指定排除的特征列表文件
- `-s, --seed`: 随机种子（默认: 1234）
- `--cores`: 并行计算核心数（默认: 8）
- `-o, --outdir`: 输出目录

### 数据格式要求

#### 输入数据文件格式
- 第一列：特征名称
- 其他列：样本数据（数值型）
- 文件格式：Excel (.xls/.xlsx) 或 TSV

#### 分组文件格式
- 第一列：样本ID
- 第二列：分组信息
- 文件格式：制表符分隔的文本文件

## 输出结果

### 文件结构
```
output_directory/
├── step1.1.efs.performance.pdf        # EFS性能比较
├── step1.2.efs.n_features.pdf         # 特征数量分布
├── step1.3.efs.pareto.pdf             # 帕累托前沿（经验）
├── step1.4.efs.pareto_estimated.pdf   # 帕累托前沿（估计）
├── step1.5.efs.stability.pdf          # 特征稳定性分析
├── step1.6.efs.feature_importance.pdf # 特征重要性排序
├── step2.1.1.model_comparison.auc.pdf # 模型AUC比较
├── step2.1.2.model_comparison.acc.pdf # 模型准确率比较
├── step2.1.3.model_comparison.ce.pdf  # 模型错误率比较
├── step2.2.model_comparison.roc.pdf   # 模型ROC曲线比较
├── train_roc_plot.pdf                 # 训练集ROC曲线
├── test_roc_plot.pdf                  # 测试集ROC曲线
├── cv_roc_plot.pdf                    # 交叉验证ROC曲线
├── step3.{model}.patchwork.pdf        # 各模型详细分析
├── step3.{model}_pod_wilcox_test.tsv  # Wilcoxon检验结果
├── model_performance_comparison.xlsx  # 完整性能比较表
├── split.map-group.txt                # 数据划分记录
└── efs.RData                          # R工作空间数据
```

### Excel结果文件
`model_performance_comparison.xlsx` 包含：
- 各模型性能指标详细比较
- 最优超参数设置
- 特征重要性排序
- 原始特征名称映射

## 技术细节

### 算法特点
1. **鲁棒性**: 使用模型封装和回退机制
2. **可重复性**: 固定随机种子确保结果一致
3. **并行计算**: 支持多核并行加速
4. **自动化**: 全流程自动化，减少人工干预

### 特征选择策略
- 基于多学习器的集成评估
- 递归特征消除保证特征质量
- 稳定性评估确保特征可靠性
- 帕累托优化平衡特征数量和性能

### 模型优化策略
- 组合终止条件避免过度搜索
- 网格搜索和随机搜索结合
- 交叉验证避免过拟合
- 多指标评估全面评价模型

## 依赖包要求

```r
# 核心包
pacman::p_load(
    optparse, tidyverse, openxlsx, mlr3verse, data.table, mltools, 
    mlr3tuningspaces, future, readxl, treeshap, kernelshap, shapviz, 
    mlr3extralearners, ranger, randomForest, pROC, patchwork, Boruta,
    showtext, xgboost, mlr3learners, mlr3tuning, paradox, future.apply, 
    ggplot2, glmnet, e1071, nnet, lightgbm
)
```

## 故障排除

### 常见错误及解决方案

1. **数据类型错误**: 确保输入数据为数值型，分组变量为字符型
2. **内存不足**: 减少并行核心数或增加系统内存
3. **包版本冲突**: 使用指定版本的R包
4. **特征重要性为空**: 某些模型可能无法提供特征重要性，这是正常现象

### 性能优化建议

1. **样本大小**: 建议样本数 > 50，特征数 < 1000
2. **计算资源**: 推荐8核以上CPU，16GB以上内存
3. **参数调优**: 根据数据集大小调整重抽样次数和CV折数

## 更新日志

### v1.0 (当前版本)
- 实现完整的EFS特征选择流程
- 集成5种主流机器学习算法
- 完善的错误处理和数据验证
- 全面的结果可视化和报告生成

### 已修复问题
- 修复了非数值参数导致的median函数错误
- 改进了特征重要性数据的处理逻辑
- 增强了数据验证和错误处理机制
- 修正了变量名引用错误

## 许可证

本项目遵循MIT许可证。

## 联系方式

如有问题或建议，请通过以下方式联系：
- 项目维护者：[您的姓名]
- 邮箱：[您的邮箱]
- 项目地址：[项目仓库链接]

---

*本文档最后更新时间：2024年1月* 