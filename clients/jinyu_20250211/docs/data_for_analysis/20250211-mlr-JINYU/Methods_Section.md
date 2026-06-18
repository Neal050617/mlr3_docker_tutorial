# Methods Section: Ensemble Feature Selection for Clinical Prediction

## English Version

## Data Preprocessing and Task Definition

Clinical data were preprocessed using a standardized pipeline implemented in the MLR3 framework. Missing values in continuous variables were imputed using historical imputation (`imputehist`), while categorical variables were imputed using mode imputation (`imputemode`). Constant features were automatically removed to reduce dimensionality. Specific features (feature58, feature27, feature26, and tNGS-related features) were excluded based on prior domain knowledge and data quality assessment.

The preprocessed dataset was randomly partitioned into training (90%) and testing (10%) sets using stratified sampling to maintain class balance. A binary classification task was defined with feature13 as the target variable, where "1" was designated as the positive class.

## Ensemble Feature Selection (EFS) Framework

### Overview
We implemented an ensemble feature selection approach that combines multiple machine learning algorithms to identify the most informative and stable feature subset. This method addresses the limitations of single-algorithm feature selection by leveraging the complementary strengths of different learning paradigms.

### Base Learners
Four diverse machine learning algorithms were selected as base learners:

1. **XGBoost**: Gradient boosting framework with early stopping (20 rounds) and maximum 500 iterations
2. **Random Forest**: Ensemble method with permutation-based feature importance
3. **Support Vector Machine (SVM)**: Linear kernel with C-classification
4. **LightGBM**: Gradient boosting with leaf-wise tree growth, configured with 31 leaves and 0.05 learning rate

### Recursive Feature Elimination (RFE)
Each base learner employed recursive feature elimination with the following configuration:
- Target feature count: 20 features
- Feature fraction: 0.85 (eliminating 15% of least important features per iteration)
- Selection criterion: Cross-validation performance on classification error

### Dual-Layer Resampling Strategy

**Outer Layer (Stability Assessment)**:
- 100 independent subsampling iterations
- 80% sampling ratio per iteration
- Purpose: Evaluate feature selection stability across different data subsets

**Inner Layer (Performance Estimation)**:
- 5-fold cross-validation within each subsample
- Purpose: Provide unbiased performance estimates
- Total model training instances: 500 (100 × 5)

### Callback Mechanisms
Algorithm-specific callbacks were implemented to optimize feature selection:

- **One Standard Error Rule**: Applied to all algorithms to select parsimonious models within one standard error of optimal performance
- **XGBoost Internal Tuning**: Simultaneous optimization of nrounds parameter during feature selection
- **SVM-RFE**: Specialized recursive feature elimination based on SVM weight vectors

### Pareto Frontier Analysis and Knee Point Detection

**Empirical Pareto Frontier**: Non-dominated solutions from actual observations plotting classification accuracy against feature count.

**Estimated Pareto Frontier**: Smooth curve fitted using the relationship `performance ~ f(1/n_features)` to facilitate knee point detection.

**Automatic Knee Point Detection**: The optimal feature count was determined using Normal Boundary Intersection (NBI) method:
1. Connect endpoints of the Pareto frontier
2. Calculate perpendicular distance from each point to the connecting line
3. Identify the point with maximum distance as the knee point
4. This point represents the optimal trade-off between performance and model complexity

### Feature Ranking and Selection

The final feature ranking was computed using the Sum of Absolute Values (SAV) method:

1. **Standardization**: Feature importance scores from each algorithm were normalized to [0,1]
2. **Weighted Aggregation**: `SAV_score = Σ(w_i × importance_i)` where weights consider:
   - Algorithm performance (cross-validation AUC)
   - Feature selection stability (Jaccard coefficient)
   - Algorithm diversity contribution
3. **Final Selection**: Top-k features based on SAV scores, where k was determined by knee point detection

### Stability Assessment

Feature selection stability was quantified using the Jaccard coefficient:
```
J(A,B) = |A ∩ B| / |A ∪ B|
```

Two stability measures were computed:
- **Global Stability**: Consistency across all algorithms
- **Local Stability**: Consistency within individual algorithms across resampling iterations

## Model Training and Hyperparameter Optimization

### Model Configuration
Five machine learning models were trained on the selected feature subset:
- Elastic Net (GLMNet)
- XGBoost
- Random Forest
- Support Vector Machine
- LightGBM

### Hyperparameter Optimization
- **Search Strategy**: Random search with 100 evaluations per model
- **Inner Resampling**: 5-fold cross-validation
- **Optimization Metric**: Area Under the ROC Curve (AUC)
- **Termination Criteria**: Combined terminator including:
  - Performance stagnation (10 iterations, 0.005 threshold)
  - Maximum evaluations (100)
  - Runtime limit (1 hour)
  - Target performance (85% accuracy)

### Model Evaluation Framework

**Cross-Validation Performance**: 5-fold outer cross-validation on training data to assess model stability and generalization.

**Independent Testing**: Final model evaluation on the held-out test set (10% of original data).

**Performance Metrics**: 
- Area Under the ROC Curve (AUC)
- Classification Accuracy
- Classification Error

## Feature Importance Analysis

Model-specific feature importance was extracted using algorithm-appropriate methods:
- **Random Forest**: Permutation importance
- **XGBoost/LightGBM**: Split-based gain importance
- **GLMNet**: Absolute coefficient values
- **SVM**: Support vector weights

## Statistical Analysis

**Model Comparison**: Wilcoxon signed-rank tests were performed to assess statistical significance of performance differences between models.

**Visualization**: Comprehensive visualization suite including:
- ROC curves with confidence intervals
- Feature importance plots
- Pareto frontier analysis
- Stability heatmaps
- Performance comparison boxplots

## Computational Implementation

All analyses were implemented using the MLR3 ecosystem in R, with parallel processing enabled (6 workers) to optimize computational efficiency. Reproducibility was ensured through fixed random seeds (seed = 123) and version-controlled analysis scripts.

## Quality Assurance

**Validation Strategy**:
1. Cross-validation for internal validation
2. Independent test set for external validation
3. Feature stability assessment across multiple data subsets
4. Statistical significance testing for model comparisons

**Robustness Checks**:
- Sensitivity analysis of hyperparameters
- Stability assessment across different random seeds
- Performance consistency across different data splits

This ensemble feature selection framework provides a robust, automated approach to identify clinically relevant biomarkers while ensuring both predictive performance and feature stability, essential for reliable clinical decision support systems.

---

## 中文版本

## 数据预处理与任务定义

临床数据采用MLR3框架中的标准化流水线进行预处理。连续变量的缺失值采用历史插补法(`imputehist`)处理，分类变量采用众数插补法(`imputemode`)处理。自动移除常数特征以降低维度。基于先验领域知识和数据质量评估，排除特定特征(feature58、feature27、feature26以及tNGS相关特征)。

预处理后的数据集采用分层抽样方法随机划分为训练集(90%)和测试集(10%)，以保持类别平衡。定义二分类任务，以feature13作为目标变量，其中"1"被指定为正类。

## 集成特征选择(EFS)框架

### 概述
我们实施了一种集成特征选择方法，该方法结合多种机器学习算法来识别最具信息性和稳定性的特征子集。该方法通过利用不同学习范式的互补优势，解决了单一算法特征选择的局限性。

### 基础学习器
选择四种不同的机器学习算法作为基础学习器：

1. **XGBoost**：梯度提升框架，采用早停机制(20轮)，最大迭代次数500次
2. **随机森林**：基于置换重要性的集成方法
3. **支持向量机(SVM)**：采用线性核的C分类
4. **LightGBM**：叶子优先的梯度提升，配置31个叶子节点，学习率0.05

### 递归特征消除(RFE)
每个基础学习器采用递归特征消除，配置如下：
- 目标特征数：20个特征
- 特征比例：0.85(每次迭代消除15%最不重要的特征)
- 选择准则：基于分类误差的交叉验证性能

### 双层重采样策略

**外层(稳定性评估)**：
- 100次独立子采样迭代
- 每次迭代80%采样比例
- 目的：评估不同数据子集上特征选择的稳定性

**内层(性能估计)**：
- 每个子样本内进行5折交叉验证
- 目的：提供无偏的性能估计
- 总模型训练实例：500次(100 × 5)

### 回调机制
实施算法特定的回调函数以优化特征选择：

- **一标准误差规则**：应用于所有算法，在最优性能一个标准误差内选择简约模型
- **XGBoost内部调优**：在特征选择过程中同时优化nrounds参数
- **SVM-RFE**：基于SVM权重向量的专门递归特征消除

### 帕累托前沿分析与膝点检测

**经验帕累托前沿**：来自实际观测的非支配解，绘制分类准确率与特征数量的关系。

**估计帕累托前沿**：使用关系式`performance ~ f(1/n_features)`拟合的平滑曲线，便于膝点检测。

**自动膝点检测**：使用法线边界交点(NBI)方法确定最优特征数：
1. 连接帕累托前沿的端点
2. 计算每个点到连接线的垂直距离
3. 识别距离最大的点作为膝点
4. 该点代表性能与模型复杂度之间的最优权衡

### 特征排名与选择

使用绝对值求和(SAV)方法计算最终特征排名：

1. **标准化**：将每个算法的特征重要性分数标准化到[0,1]
2. **加权聚合**：`SAV_score = Σ(w_i × importance_i)`，其中权重考虑：
   - 算法性能(交叉验证AUC)
   - 特征选择稳定性(Jaccard系数)
   - 算法多样性贡献
3. **最终选择**：基于SAV分数选择前k个特征，其中k由膝点检测确定

### 稳定性评估

使用Jaccard系数量化特征选择稳定性：
```
J(A,B) = |A ∩ B| / |A ∪ B|
```

计算两种稳定性指标：
- **全局稳定性**：所有算法间的一致性
- **局部稳定性**：单个算法在重采样迭代中的一致性

## 模型训练与超参数优化

### 模型配置
在选定的特征子集上训练五种机器学习模型：
- 弹性网络(GLMNet)
- XGBoost
- 随机森林
- 支持向量机
- LightGBM

### 超参数优化
- **搜索策略**：每个模型进行100次评估的随机搜索
- **内层重采样**：5折交叉验证
- **优化指标**：ROC曲线下面积(AUC)
- **终止准则**：组合终止器包括：
  - 性能停滞(10次迭代，0.005阈值)
  - 最大评估次数(100次)
  - 运行时间限制(1小时)
  - 目标性能(85%准确率)

### 模型评估框架

**交叉验证性能**：在训练数据上进行5折外层交叉验证，评估模型稳定性和泛化能力。

**独立测试**：在保留的测试集(原始数据的10%)上进行最终模型评估。

**性能指标**：
- ROC曲线下面积(AUC)
- 分类准确率
- 分类误差

## 特征重要性分析

使用算法适当的方法提取模型特定的特征重要性：
- **随机森林**：置换重要性
- **XGBoost/LightGBM**：基于分裂增益的重要性
- **GLMNet**：系数绝对值
- **SVM**：支持向量权重

## 统计分析

**模型比较**：采用Wilcoxon符号秩检验评估模型间性能差异的统计显著性。

**可视化**：综合可视化套件包括：
- 带置信区间的ROC曲线
- 特征重要性图
- 帕累托前沿分析
- 稳定性热图
- 性能比较箱线图

## 计算实现

所有分析均使用R语言的MLR3生态系统实现，启用并行处理(6个工作进程)以优化计算效率。通过固定随机种子(seed = 123)和版本控制的分析脚本确保可重现性。

## 质量保证

**验证策略**：
1. 交叉验证进行内部验证
2. 独立测试集进行外部验证
3. 多个数据子集的特征稳定性评估
4. 模型比较的统计显著性检验

**鲁棒性检查**：
- 超参数的敏感性分析
- 不同随机种子的稳定性评估
- 不同数据划分的性能一致性

该集成特征选择框架提供了一种稳健、自动化的方法来识别临床相关的生物标志物，同时确保预测性能和特征稳定性，这对于可靠的临床决策支持系统至关重要。