# XGBoost-SHAP 微生物组年龄预测分析脚本说明

## 简介

本脚本 `xgb-shap251216.py` 是一个完整的机器学习分析工具，专门用于基于微生物组数据预测人类年龄。它结合了 XGBoost 回归算法和 SHAP (SHapley Additive exPlanations) 解释框架，不仅能够建立准确的预测模型，还能深入解释哪些微生物特征对年龄预测最为重要。

## 功能特点

### 1. 数据处理与预处理
- 自动加载微生物组数据（CLR转换后的特征）和样本元数据（年龄信息）
- 数据质量控制和一致性验证
- 自动创建必要的输出目录结构

### 2. 模型训练与优化
- 使用嵌套交叉验证（Nested Cross-Validation）确保模型泛化能力
- 集成递归特征消除（RFE）进行特征选择
- 通过随机搜索（RandomizedSearchCV）优化超参数
- 支持模型保存和加载功能

### 3. 模型评估
- 计算多种评估指标：R²、MSE、RMSE、MAE
- 生成实际值vs预测值散点图
- 残差分析图
- 训练集和测试集性能对比

### 4. SHAP可解释性分析
- 全面的SHAP值计算和可视化
- 特征重要性分析（XGBoost内置和SHAP全局重要性）
- 个体预测解释（Waterfall图、Force图、Decision图）
- 特征依赖关系分析（Dependence图、散点图）
- 年龄组特异性分析（Age Group Aggregation Analysis）

### 5. 高级可视化功能
- SHAP摘要图（条形图、蜜蜂图）
- SHAP热力图
- 特征交互分析（依赖图对）
- 年龄组间差异分析

## 输入文件

### 必需文件
1. `input/clr_variance_filtered.xls` - 微生物组特征数据（已进行CLR转换）
   - 行代表样本，列代表微生物分类单元（ASV）
   - 第一列为OTU_tax标识符

2. `input/age.txt` - 样本元数据
   - 包含样本ID和对应年龄信息
   - 列名：#SampleID, age

## 输出文件

脚本运行后会在 `output/` 目录下生成以下内容：

### 模型相关
- `models/final_pipeline.pkl` - 训练好的完整模型管道
- `results/final_features.txt` - 最终选择的特征列表
- `results/final_model_params.json` - 最优超参数配置

### 评估结果
- `results/cross_validation_summary.csv` - 交叉验证结果汇总
- `results/sample_predictions.csv` - 每个样本的实际值和预测值

### 可视化图表
- `plots/` - 各类图表文件
  - 实际值vs预测值散点图
  - 残差分析图
  - XGBoost特征重要性图
  - SHAP各类解释图（PDF格式）
- `shap_values/shap_values.npy` - 计算的SHAP值

### SHAP分析专用
- `csv_paths/` - SHAP分析数据
  - SHAP全局重要性表
  - SHAP局部值表
  - 年龄组聚合分析结果

## 使用方法

### 基本运行
```bash
python xgb-shap251216.py
```

### 控制参数
脚本顶部的全局配置可以调整：
- `TRAIN_MODEL = False` - 设置为True重新训练模型，False使用已保存模型
- `RANDOM_STATE = 42` - 随机种子，确保结果可重现

### 特殊分析功能

#### 1. 年龄组特异性分析
脚本会自动按年龄段分组，分析不同年龄组的微生物特征贡献差异：
- 5岁年龄段分组（2-7岁, 8-12岁, ...）
- 生成各年龄组的聚合SHAP值
- 创建年龄组间对比热力图

#### 2. 自定义特征分析
可通过创建以下文件来指定要分析的特征：
- `select.txt` - 指定要重点分析的特征列表
- `select1.txt` 和 `select2.txt` - 指定要分析特征交互关系的两组特征

#### 3. 特定样本分析
创建 `select_sample.txt` 文件列出要单独分析的样本名，脚本会为这些样本生成详细的SHAP解释图。

## 技术细节

### 模型架构
采用Pipeline设计模式，包含两个主要步骤：
1. 特征选择：使用XGBoost作为评估器的递归特征消除（RFE）
2. 回归预测：XGBoost回归器

### 超参数优化空间
- n_estimators: [100, 200, 300]
- max_depth: [3, 5, 7]
- learning_rate: [0.01, 0.05, 0.1]
- subsample: [0.8, 0.9, 1.0]
- colsample_bytree: [0.8, 0.9, 1.0]
- reg_alpha: [0, 0.1, 0.5]
- reg_lambda: [0.1, 1, 2]

### 交叉验证策略
- 外层：5折交叉验证
- 内层：5折交叉验证用于超参数优化
- 使用随机搜索迭代50次寻找最优参数组合

## 注意事项

1. 确保输入文件格式正确，特别是列名和数据类型
2. 脚本会自动创建输出目录结构，无需手动创建
3. 首次运行建议设置 `TRAIN_MODEL = True` 进行模型训练
4. 大量可视化图表生成可能需要较长时间
5. SHAP分析对于特征较多的数据集计算量较大

## 依赖库

- pandas
- numpy
- scikit-learn
- xgboost
- shap
- matplotlib
- seaborn

## 最终模型性能

根据输出结果显示，最终模型使用了1211个特征，主要的超参数配置为：
- n_estimators: 300
- max_depth: 5
- learning_rate: 0.05
- subsample: 0.9
- colsample_bytree: 0.9