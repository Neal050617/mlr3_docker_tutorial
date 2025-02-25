library(mlr3)
library(mlr3pipelines)

# 创建预处理+模型的流水线
graph = po("imputeoor") %>>%  # 处理缺失值
  po("scale") %>>%            # 标准化数据
  po("learner", lrn("classif.log_reg"))  # 逻辑回归模型

# 转换为可训练的学习器
glrn = as_learner(graph)

# 加载数据
task_pima = tsk("pima")

# 训练模型
glrn$train(task_pima)

# 方法1：通过管道操作访问
coef_method1 = glrn$graph_model$pipeops$classif.log_reg$learner_model$model

# 方法2：使用基础学习器方法
coef_method2 = glrn$base_learner()$model

# 显示结果
cat("方法1系数:\n"); print(coef_method1$coefficients)
cat("\n方法2系数:\n"); print(coef_method2$coefficients)

# 启用中间结果保存
glrn$graph_model$keep_results = TRUE

# 重新训练
glrn$train(task_pima)

# 获取标准化后的数据
scale_output = glrn$graph_model$pipeops$scale$.result[[1]]$data()

# 计算age列的统计量
age_stats = scale_output[, .(mean = mean(age), sd = sd(age))]
cat("\n标准化验证:\n")
print(age_stats)





# 数据清洗
1. 数据类型为因子，但类别只有一个
2. 某些列数据完全相同
3. 某些列是相同数据的不同标准形式
4. ID列，即每条观测值都唯一的列应该被移除或标记
5. 缺失值没有正确编码，例如被编码为"NA"或""
6. 数据中的语义错误，例如Lot_Area为负值
7. 数值特征被编码为分类特征，而某些学习器无法处理这种情况





library(tidyverse)
library(mlr3)
library(mlr3data)
library(mlr3pipelines)

# 加载数据并移除日期列
kc_housing = tsk("kc_housing")
kc_housing$select(setdiff(kc_housing$feature_names, "date"))

# 查看数据摘要
kc_housing$data() %>% 
  skimr::skim() %>% 
  skimr::focus(
    skim_type, n_missing, 
    numeric.mean, numeric.sd
  )