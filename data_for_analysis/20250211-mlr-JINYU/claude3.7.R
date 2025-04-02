# mlr3分析正式启动
## 加载必要的包
library(mlr3verse)
library(mlr3extralearners)
library(paradox)
library(future)
library(future.apply)
library(ggplot2)
library(tidyverse)

## 设置并行计算
future::plan("multisession", workers = 6) #  parallel::detectCores() - 1
set.seed(123)
load("./step1.RData")

## 创建mlr3任务
data <- data14 |>
  mutate(!!sym(feature_names$group) := as.factor(!!sym(feature_names$group))) |>
  select(-starts_with("feature17_"),-starts_with("feature24_"))

task <- TaskClassif$new(
  id = "model_comparison",
  backend = data,
  target = "feature13",
  positive = "1"
)

#插补方法选择：
##对连续型变量使用直方图插补(imputehist)比简单的均值插补更好，因为它保留了原始数据的分布特性
##对分类型变量使用众数插补(imputemode)更合适，特别是对于二元哑变量
#连续型变量的其他选择：
##imputemean：使用均值插补，简单但忽略分布
##imputemedian：使用中位数插补，对异常值更稳健
##imputeoor：使用范围外值插补，便于识别插补数据
##imputelearner：使用机器学习模型预测缺失值
#分类型变量的其他选择：
##imputeconstant：使用常数插补
##imputelearner：使用分类模型预测缺失值

### 获取分类型变量和连续型变量的列名
is_binary <- function(x) {
  vals <- unique(x[!is.na(x)])
  length(vals) <= 2 && all(vals %in% c(0, 1))
}

#使用sapply来简化循环
binary_check <- sapply(data, is_binary)
categorical_cols <- names(data)[binary_check]
continuous_cols <- names(data)[!binary_check]

### 创建缺失值插补管道
cont_imputer <- po("imputehist", affect_columns = selector_name(continuous_cols))
cat_imputer <- po("imputemode", affect_columns = selector_name(categorical_cols))
imputation_pipeline <- gunion(list(cont_imputer, cat_imputer)) %>>% po("featureunion")

# 特征筛选管道部分
# 定义不同的特征筛选方法
#filter_methods <- flt("random_forest_importance")
stability_selector <- po("fselect_stability", 
                        filter = flt("importance"),
                        ratio = 0.8,  # 使用80%数据的随机子样本
                        threshold = 0.7,  # 特征需要在70%的子样本中被选中
                        nselect = ceiling(ncol(inner_task$data()) * 0.5))  # 保持选择50%特征

# 创建超参数空间
learner_params <- list(
  # 逻辑回归
  logistic = ParamSet$new(list(
    ParamDbl$new("cost", lower = 0.01, upper = 10, logscale = TRUE),
    ParamDbl$new("epsilon", lower = 0.01, upper = 1)
  )),
  
  # 支持向量机
  svm = ParamSet$new(list(
    ParamDbl$new("cost", lower = 0.1, upper = 100, logscale = TRUE),
    ParamDbl$new("gamma", lower = 0.001, upper = 1, logscale = TRUE)
  )),
  
  # LASSO回归
  lasso = ParamSet$new(list(
    ParamDbl$new("s", lower = 0.001, upper = 1, logscale = TRUE)
  )),
  
  # 弹性网络
  glmnet = ParamSet$new(list(
    ParamDbl$new("alpha", lower = 0, upper = 1),
    ParamDbl$new("lambda", lower = 0.001, upper = 1, logscale = TRUE)
  )),
  
  # 岭回归
  ridge = ParamSet$new(list(
    ParamDbl$new("lambda", lower = 0.001, upper = 1, logscale = TRUE)
  )),
  
  # 随机森林
  ranger = ParamSet$new(list(
    ParamInt$new("num.trees", lower = 100, upper = 1000),
    ParamInt$new("mtry", lower = 5, upper = 50),
    ParamInt$new("min.node.size", lower = 1, upper = 20)
  )),
  
  # XGBoost
  xgboost = ParamSet$new(list(
    ParamInt$new("nrounds", lower = 50, upper = 500),
    ParamDbl$new("eta", lower = 0.01, upper = 0.3, logscale = TRUE),
    ParamInt$new("max_depth", lower = 3, upper = 10),
    ParamDbl$new("gamma", lower = 0, upper = 5),
    ParamDbl$new("colsample_bytree", lower = 0.5, upper = 1),
    ParamDbl$new("subsample", lower = 0.5, upper = 1)
  )),
  
  # LightGBM
  lightgbm = ParamSet$new(list(
    ParamInt$new("num_iterations", lower = 50, upper = 500),
    ParamDbl$new("learning_rate", lower = 0.01, upper = 0.3, logscale = TRUE),
    ParamInt$new("max_depth", lower = 3, upper = 10),
    ParamDbl$new("min_data_in_leaf", lower = 5, upper = 50),
    ParamDbl$new("feature_fraction", lower = 0.5, upper = 1),
    ParamDbl$new("bagging_fraction", lower = 0.5, upper = 1)
  )),
  
  # 神经网络
  nnet = ParamSet$new(list(
    ParamInt$new("size", lower = 3, upper = 15),
    ParamDbl$new("decay", lower = 0.0001, upper = 0.1, logscale = TRUE)
  ))
)

# 创建不同类型的学习器
learners <- list(
  lrn("classif.log_reg", predict_type = "prob"),
  lrn("classif.svm", predict_type = "prob", type = "C-classification", kernel = "radial"),
  lrn("classif.glmnet", predict_type = "prob", alpha = 1), # LASSO
  lrn("classif.glmnet", predict_type = "prob"), # 弹性网络
  lrn("classif.glmnet", predict_type = "prob", alpha = 0), # 岭回归
  lrn("classif.ranger", predict_type = "prob"),
  lrn("classif.xgboost", predict_type = "prob"),
  lrn("classif.lightgbm", predict_type = "prob"),
  lrn("classif.nnet", predict_type = "prob", trace = FALSE)
)

# 设置学习器ID
learners[[1]]$id <- "logistic"
learners[[2]]$id <- "svm"
learners[[3]]$id <- "lasso"
learners[[4]]$id <- "elastic_net"
learners[[5]]$id <- "ridge"
learners[[6]]$id <- "random_forest"
learners[[7]]$id <- "xgboost"
learners[[8]]$id <- "lightgbm"
learners[[9]]$id <- "neural_network"

# 定义性能指标
measures <- list(
  msr("classif.auc"),
  msr("classif.acc"),
  msr("classif.sensitivity"),
  msr("classif.specificity"),
  msr("classif.fbeta", beta = 1)
)

# 为每个模型创建嵌套CV管道
# 外部CV用于评估模型性能
# 内部CV用于特征选择和超参数优化
results <- data.table()
selected_features_all <- list()

# 设置外部CV
outer_cv <- rsmp("cv", folds = 5)
outer_cv$instantiate(task)

# 遍历所有模型
for (i in seq_along(learners)) {
  cat("处理模型:", learners[[i]]$id, "\n")
  
  learner <- learners[[i]]
  learner_id <- learner$id
  
  # 只考虑当前模型的超参数
  param_set <- NULL
  if (learner_id == "logistic") param_set <- learner_params$logistic
  else if (learner_id == "svm") param_set <- learner_params$svm
  else if (learner_id == "lasso") param_set <- learner_params$lasso
  else if (learner_id == "elastic_net") param_set <- learner_params$glmnet
  else if (learner_id == "ridge") param_set <- learner_params$ridge
  else if (learner_id == "random_forest") param_set <- learner_params$ranger
  else if (learner_id == "xgboost") param_set <- learner_params$xgboost
  else if (learner_id == "lightgbm") param_set <- learner_params$lightgbm
  else if (learner_id == "neural_network") param_set <- learner_params$nnet
  
  # 存储每个折叠的预测结果
  cv_predictions <- list()
  selected_features_model <- list()
  
  # 在外部CV上循环
  for (fold in seq_len(outer_cv$iters)) {
    train_set <- outer_cv$train_set(fold)
    test_set <- outer_cv$test_set(fold)
    
    # 创建内部CV任务
    inner_task <- task$clone()
    inner_task$filter(train_set)
    
    # 特征筛选
    filter_method <- stability_selector
    filtered <- filter_method$calculate(inner_task)
    selected_features <- filtered$filter(ceiling(ncol(inner_task$data()) * 0.5)) # 选取前50%的特征
    selected_features_model[[fold]] <- selected_features
    
    # 对内部任务应用特征筛选
    inner_task$select(selected_features)
    
    # 为每个特定模型设置内部CV
    inner_cv <- rsmp("cv", folds = 3)
    
    # 创建自动调参的学习器
    at <- AutoTuner$new(
      learner = learner,
      resampling = inner_cv,
      measure = msr("classif.auc"),
      search_space = param_set,
      terminator = trm("evals", n_evals = 20),
      tuner = tnr("random_search")
    )
    
    # 在内部数据上训练
    at$train(inner_task)
    
    # 准备测试数据（应用相同的特征筛选）
    test_task <- task$clone()
    test_task$filter(test_set)
    test_task$select(selected_features)
    
    # 预测并存储结果
    prediction <- at$predict(test_task)
    cv_predictions[[fold]] <- prediction
    
    # 输出当前折叠的性能
    perf <- list()
    for (measure in measures) {
      perf[[measure$id]] <- measure$score(prediction)
    }
    perf$fold <- fold
    perf$learner <- learner_id
    results <- rbindlist(list(results, as.data.table(perf)), fill = TRUE)
    
    cat(sprintf("  折叠 %d 完成 - AUC: %.4f\n", fold, perf$classif.auc))
  }
  
  # 保存所选特征
  selected_features_all[[learner_id]] <- selected_features_model
}

# 分析结果
# 计算每个模型的平均性能
model_performance <- results[, lapply(.SD, mean), by = "learner", 
                             .SDcols = c("classif.auc", "classif.acc", "classif.sensitivity", 
                                        "classif.specificity", "classif.fbeta")]

# 排序并找出最佳模型
model_performance <- model_performance[order(-classif.auc)]

# 输出性能表
print(model_performance)

# 可视化结果
ggplot(results, aes(x = learner, y = classif.auc, fill = learner)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "各模型AUC表现", x = "模型", y = "AUC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 分析特征选择的结果
feature_importance <- list()
for (model_name in names(selected_features_all)) {
  model_features <- selected_features_all[[model_name]]
  all_selected <- character(0)
  
  for (fold_features in model_features) {
    all_selected <- c(all_selected, fold_features)
  }
  
  feature_counts <- table(all_selected)
  top_features <- names(sort(feature_counts, decreasing = TRUE)[1:20])
  feature_importance[[model_name]] <- top_features
}

# 输出每个模型选择的最重要特征
for (model_name in names(feature_importance)) {
  cat("模型:", model_name, "\n")
  cat("最重要的20个特征:", paste(feature_importance[[model_name]], collapse = ", "), "\n\n")
}

# 最终报告最佳模型
best_model <- model_performance$learner[1]
cat("**最佳模型:**", best_model, "\n")
cat("**平均AUC:**", model_performance$classif.auc[1], "\n")
cat("**平均准确率:**", model_performance$classif.acc[1], "\n")
cat("**平均灵敏度:**", model_performance$classif.sensitivity[1], "\n")
cat("**平均特异度:**", model_performance$classif.specificity[1], "\n")
cat("**平均F1分数:**", model_performance$classif.fbeta[1], "\n")