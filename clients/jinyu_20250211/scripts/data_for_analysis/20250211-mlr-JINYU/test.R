setwd("~/analysis/data_for_analysis/20250211-mlr-JINYU/")
# RFE for Random Forest 示例 (需要安装 mlr3fselect 和 mlr3learners)
library(mlr3verse)
library(mlr3learners)
library(mlr3tuning)
library(mlr3extralearners)
library(paradox)
library(future)
library(future.apply)
library(ggplot2)
library(tidyverse)
library(ranger)
library(glmnet)
library(e1071)
library(nnet)
library(xgboost)
library(lightgbm)

# 设置并行计算
future::plan("multisession", workers = 6) # 使用6个并行工作进程
# future::plan(list(
#   tweak("multisession", workers = 3),
#   tweak("multisession", workers = 2)
# ))
set.seed(123) # 设置随机种子，确保结果可重复
load("./step1.RData") # 加载之前准备好的数据
# 减少屏幕输出
#lgr::get_logger("mlr3")$set_threshold("warn")
#lgr::get_logger("bbotk")$set_threshold("warn")
# 恢复默认屏幕输出 (显示 info 级别及以上的消息)
#lgr::get_logger("mlr3")$set_threshold("info")
#lgr::get_logger("bbotk")$set_threshold("info")

# ========== 第二步：数据处理 ==========
# 准备数据
data <- data14 |>
  mutate(!!sym(feature_names$group) := as.factor(!!sym(feature_names$group)))# |>
#  select(-starts_with("feature24_")) # -starts_with("feature17_"),

# 创建分类任务
pbp_task <- TaskClassif$new(
  id = "model_comparison",
  backend = data,
  target = "feature13",
  positive = "1"
)

pbp_prep = po("imputehist",affect_columns = selector_name(continuous_cols)) %>>% 
  po("imputemode",affect_columns = selector_name(categorical_cols)) %>>% 
  po("removeconstants")

task_prep <- pbp_prep$clone()$train(pbp_task)[[1]]
dim(task_prep$data())

# 数据划分
split_task <- partition(task=pbp_task, ratio=0.9 )

task_train <- pbp_task$clone()$filter(split_task$train)
task_test  <- pbp_task$clone()$filter(split_task$test)

# ========== 第三步：缺失值处理 ==========
# 识别分类变量和连续变量
is_binary <- function(x) {
  vals <- unique(x[!is.na(x)])
  length(vals) <= 2 && all(vals %in% c(0, 1))
}

binary_check <- sapply(data, is_binary)
categorical_cols <- names(data)[binary_check]
continuous_cols <- names(data)[!binary_check]

# ========== 第四步：模型训练 ==========
# 随机森林（带参数调优）
# 1. 定义预处理 pipeline (假设已经定义了 pbp_prep，与之前的代码相同)
# task_train
# 2. 定义基础学习器 (随机森林 ranger)，不包含任何预处理或特征选择步骤，只关注模型本身
learner_base_rf = lrn("classif.ranger", predict_type = "prob", importance = "impurity")
# 3. 定义特征选择方法 (RFE)
fselector_rfe = fs("rfe",n_features = 20)
# 定义参数搜索空间
rf_tuning_params = ps(
  num.trees = p_int(lower = 500, upper = 1000),
  min.node.size = p_int(lower = 3, upper = 10),
  mtry = p_int(lower = 3, upper = ceiling(sqrt(ncol(task_prep$data()))))
)
# *** 定义停滞终止器 (trm("stagnation")) ***
terminator_stagnation = trm("stagnation",
  iters = 10,    # 监控最近 10 次迭代
  threshold = 0.005 # 相对提升阈值为 0.05% (0.0005)
)
# 5. 定义调优方法
tuner = tnr("random_search")
terminator = trm("evals", n_evals = 10)
inner_resampling = rsmp("cv", folds = 5)
outer_resampling = rsmp("cv", folds = 5)

# 8. 修正学习流水线（使用AutoFSelector包装特征选择）
learner_rfe_tuned_rf = auto_fselector(
  fselector = fselector_rfe,
  learner = as_learner(pbp_prep %>>%
    po("select", selector = selector_invert(selector_grep("^feature24_"))) %>>%
    learner_base_rf),
  resampling = inner_resampling,
  measure = msr("classif.ce"),
  terminator = trm("combo", list(terminator, terminator_stagnation), any = TRUE)
)
learner_rfe_tuned_rf$id <- "rfe_tuned_rf"

# 9. 创建自动调优器 (更简洁的方法)
at = auto_tuner(
  tuner = tuner,
  learner = learner_rfe_tuned_rf,
  resampling = inner_resampling,
  measure = msr("classif.ce"),
  search_space = rf_tuning_params,
  terminator = trm("combo", list(terminator, terminator_stagnation), any = TRUE),
  store_tuning_instance = TRUE  # 保存调优实例
)

# 10. 使用嵌套重抽样
future::plan("sequential")  # 先尝试使用串行计算来测试

rr = resample(
  task = task_train,
  learner = at,
  resampling = outer_resampling,
  store_models = TRUE
)

# 获取结果
tuning_result_nested = rr$aggregate()

# 11. 分析结果
print(tuning_result_nested) # 查看最佳参数组合和最佳性能

# 获取最佳 Learner (包含预处理, 特征选择, 和调优后的随机森林)
best_learner_nested <- tuning_result_nested$learner

# 在整个训练集上训练最佳 Learner
best_learner_nested$train(task)

# 可以使用 best_learner_nested 进行预测
predictions_nested <- best_learner_nested$predict(task_test) # 假设有测试集 task_test


# *** 提取嵌套重抽样过程中的特征选择和超参数调优结果 ***

# 访问外层重抽样的结果 (每个外层 fold 的结果)
outer_results = tuning_result_nested$tuning_result
# 遍历每个外层 fold 的结果
for (i in 1:length(outer_results$errors)) {
  cat(paste("Outer Fold", i, ":\n"))
  outer_fold_result = outer_results[i,]

  # 获取当前外层 fold 内，内层调优选择的最佳参数 (超参数 + 特征选择参数)
  best_params_inner = outer_fold_result$params[[1]] # params 列是 list 类型，每行一个 list
  print("  Best Params (Inner Tuning):")
  print(best_params_inner)

  # 获取当前外层 fold 内，内层调优选择的最佳特征子集 (RFE 选择的特征)
  # 注意： RFE 的特征选择结果通常不会直接保存在 tuning_result_nested 中，
  # 需要手动访问 FSelectInstance 的结果 (如果 RFE 是在 FSelectInstance 中运行)
  #  在嵌套交叉验证中，特征选择是在 *每个内层 fold* 的训练集上进行的，
  #  如果要获取 *每个外层 fold* 最终选择的特征子集，可能需要更复杂的代码逻辑来追踪和提取，
  #  或者修改上面的代码，在每次 RFE 迭代后，手动保存选择的特征子集。

  cat("\n")
}

# 获取最终模型在 *外层重抽样* 的性能评估结果 (例如，每个外层 fold 的分类错误率)
performance_outer_resampling = tuning_result_nested$performance
print("Performance across Outer Resampling Folds:")
print(performance_outer_resampling)

# 平均性能 (例如，平均分类错误率)
mean_performance = mean(performance_outer_resampling)
print(paste("Mean Performance (Classification Error):", mean_performance))















rf_glr <- as_learner(pbp_prep %>>% 
  po("select", selector = selector_invert(selector_grep("^feature24_"))) %>>%
  lrn("classif.ranger", predict_type="prob"))

rf_glr$id <- "randomForest"
rf_glr$param_set$values$importance <- "impurity"  # 或 "permutation"

# 逻辑回归
log_glr <-as_learner(pbp_prep %>>% lrn("classif.log_reg", predict_type="prob")) 
log_glr$id <- "logistic"

# 决策树
tree_glr <- as_learner(pbp_prep %>>% lrn("classif.rpart", predict_type="prob")) 
tree_glr$id <- "decisionTree"

# 5折交叉验证
cv <- rsmp("cv",folds=5)

# 建立多个模型
design <- benchmark_grid(
  tasks = task_train,
  learners = list(rf_glr,log_glr,tree_glr),
  resampling = cv
)


# 开始运行
bmr <- benchmark(design,store_models = T)

# 获得单个结果
bmr$score()

# 获得平均的模型表现
bmr$aggregate()

# 获得单个模型
bmr$learners$learner[[1]]

measures <- msrs(c("classif.auc","classif.acc","classif.bbrier"))
bmr_res <- bmr$aggregate(measures)

autoplot(bmr)+theme(axis.text.x = element_text(angle = 45))
autoplot(bmr,type = "roc")

# ========== 第五步：模型评估 ==========
# 获取最佳模型
best_model <- bmr$filter(measures)$model

# 使用最佳模型进行预测
# 训练
rf_glr$train(task_train)
# 测试
prediction <- rf_glr$predict(task_test)
head(as.data.table(prediction))
# 混淆矩阵
prediction$confusion

autoplot(prediction)

prediction$score(msrs(c("classif.auc","classif.acc","classif.bbrier")))

autoplot(prediction,type = "roc")

###############

learner = lrn("classif.ranger", predict_type="prob")
learner$param_set

learner = lrn("classif.log_reg", predict_type="prob")

learner = lrn("classif.rpart", predict_type="prob")

learner = lrn("classif.kknn", predict_type="prob")

learner = lrn("classif.svm", predict_type="prob")

learner = lrn("classif.nnet", predict_type="prob")

learner = lrn("classif.xgboost", predict_type="prob")

learner = lrn("classif.lightgbm", predict_type="prob")
