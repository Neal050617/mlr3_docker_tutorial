# ========== 第一步：准备环境和数据 ==========
# 加载必要的包
library(mlr3verse)
library(mlr3learners)
library(mlr3tuning)
library(mlr3extralearners)
library(paradox)
library(future)
library(future.apply)
library(ggplot2)
library(tidyverse)
# 检查并安装缺失的包
required_packages <- c("ranger", "glmnet", "e1071", "nnet", "xgboost", "lightgbm")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
  cat("正在安装缺失的包:", paste(missing_packages, collapse=", "), "\n")
  install.packages(missing_packages)
}

# 加载机器学习包
for(pkg in required_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("包", pkg, "安装失败，请手动安装"))
  }
  library(pkg, character.only = TRUE)
}

# 设置并行计算
future::plan("multisession", workers = 6) # 使用6个并行工作进程
set.seed(123) # 设置随机种子，确保结果可重复
load("./step1.RData") # 加载之前准备好的数据

# ========== 第二步：数据处理 ==========
# 准备数据
data <- data14 |>
  mutate(!!sym(feature_names$group) := as.factor(!!sym(feature_names$group)))# |>
#  select(-starts_with("feature24_")) # -starts_with("feature17_"),

# 创建分类任务
task <- TaskClassif$new(
  id = "model_comparison",
  backend = data,
  target = "feature13",
  positive = "1"
)

# 识别分类变量和连续变量
is_binary <- function(x) {
  vals <- unique(x[!is.na(x)])
  length(vals) <= 2 && all(vals %in% c(0, 1))
}

binary_check <- sapply(data, is_binary)
categorical_cols <- names(data)[binary_check]
continuous_cols <- names(data)[!binary_check]

# ========== 第三步：缺失值处理 ==========
# 为连续变量创建直方图插补器
impute_continuous <- function(task) {
  task_copy <- task$clone()
  data_copy <- task_copy$data()
  
  for (col in continuous_cols) {
    if (task_copy$missings(cols = col) > 0) {
      # 获取非缺失值
      values <- data_copy[[col]]
      values <- values[!is.na(values)]
      # 创建直方图并从直方图中采样
      hist_data <- hist(values, plot = FALSE)
      probs <- hist_data$counts / sum(hist_data$counts)
      bin_mids <- hist_data$mids
      # 替换缺失值
      missing_indices <- which(is.na(data_copy[[col]]))
      if (length(missing_indices) > 0) {
        replacements <- sample(bin_mids, size = length(missing_indices), replace = TRUE, prob = probs)
        data_copy[missing_indices, col] <- replacements
      }
    }
  }
  
  # 创建新任务，替换原数据
  new_task <- TaskClassif$new(
    id = task_copy$id,
    backend = data_copy,
    target = task_copy$target_names
  )
  
  return(new_task)
}

# 为分类变量创建众数插补器
impute_categorical <- function(task) {
  task_copy <- task$clone()
  data_copy <- task_copy$data()
  
  for (col in categorical_cols) {
    if (task_copy$missings(cols = col) > 0) {
      # 计算众数
      values <- data_copy[[col]]
      # 获取列的类型
      col_type <- class(data_copy[[col]])
      # 计算众数
      mode_val <- names(sort(table(values[!is.na(values)]), decreasing = TRUE))[1]
      
      # 根据列类型转换众数值
      if (is.numeric(data_copy[[col]])) {
        mode_val <- as.numeric(mode_val)
      } else if (is.factor(data_copy[[col]])) {
        mode_val <- as.factor(mode_val)
      } else if (is.logical(data_copy[[col]])) {
        mode_val <- as.logical(mode_val)
      }
      
      # 替换缺失值
      missing_indices <- which(is.na(data_copy[[col]]))
      if (length(missing_indices) > 0) {
        data_copy[missing_indices, col] <- mode_val
      }
    }
  }
  
  # 创建新任务，替换原数据
  new_task <- TaskClassif$new(
    id = task_copy$id,
    backend = data_copy,
    target = task_copy$target_names,
    positive = task_copy$positive
  )
  
  return(new_task)
}
# 执行插补
task_imputed <- task
task_imputed <- impute_continuous(task_imputed)
task_imputed <- impute_categorical(task_imputed)

# 在插补完成后删除不需要的特征
data_imputed <- task_imputed$data()
data_imputed <- data_imputed |>
  select(-starts_with("feature24_")) # -starts_with("feature17_")

# 用过滤后的数据创建新任务
task_imputed <- TaskClassif$new(
  id = "model_comparison",
  backend = data_imputed,
  target = "feature13",
  positive = "1"
)
# ========== 第四步：创建学习器和参数空间 ==========
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
  lrn("classif.nnet", predict_type = "prob")
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
# 创建超参数空间
learner_params <- list(
  # 逻辑回归
  logistic = ps(
    cost = p_dbl(lower = 0.01, upper = 20, logscale = TRUE),
    epsilon = p_dbl(lower = 0.01, upper = 1)
  ),
  
  # 支持向量机
  svm = ps(
    cost = p_dbl(lower = 0.1, upper = 50, logscale = TRUE),
    gamma = p_dbl(lower = 0.001, upper = 0.5, logscale = TRUE)
  ),
  
  # LASSO回归
  lasso = ps(
    s = p_dbl(lower = 0.0001, upper = 1, logscale = TRUE)
  ),
  
  # 弹性网络
  elastic_net = ps(
    alpha = p_dbl(lower = 0, upper = 1),
    lambda = p_dbl(lower = 0.0001, upper = 1, logscale = TRUE)
  ),
  
  # 岭回归
  ridge = ps(
    lambda = p_dbl(lower = 0.0001, upper = 2, logscale = TRUE)
  ),
  
  # 随机森林
  ranger = ps(
    num.trees = p_int(lower = 100, upper = 1000),
    mtry = p_int(lower = 5, upper = 35),
    min.node.size = p_int(lower = 2, upper = 30)
  ),
  
  # XGBoost
  xgboost = ps(
    nrounds = p_int(lower = 50, upper = 500),
    eta = p_dbl(lower = 0.01, upper = 0.3, logscale = TRUE),
    max_depth = p_int(lower = 3, upper = 12),
    gamma = p_dbl(lower = 0, upper = 5),
    colsample_bytree = p_dbl(lower = 0.3, upper = 0.8),
    subsample = p_dbl(lower = 0.6, upper = 1)
  ),
  
  # LightGBM
  lightgbm = ps(
    num_iterations = p_int(lower = 50, upper = 500),
    learning_rate = p_dbl(lower = 0.01, upper = 0.3, logscale = TRUE),
    max_depth = p_int(lower = 3, upper = 12),
    min_data_in_leaf = p_int(lower = 5, upper = 60),
    feature_fraction = p_dbl(lower = 0.3, upper = 0.8),
    bagging_fraction = p_dbl(lower = 0.6, upper = 1)
  ),
  
  # 神经网络
  nnet = ps(
    size = p_int(lower = 5, upper = 25),
    decay = p_dbl(lower = 0.0001, upper = 0.2, logscale = TRUE),
    maxit = p_int(lower = 100, upper = 500)
  )
)

# 定义性能指标
measures <- list(
  msr("classif.auc"),
  msr("classif.acc"),
  msr("classif.sensitivity"),
  msr("classif.specificity"),
  msr("classif.fbeta", beta = 1)
)

# ========== 第五步：特征选择和模型评估 ==========
# 初始化结果存储
results <- data.table()
selected_features_all <- list()

# 设置外部交叉验证
outer_cv <- rsmp("cv", folds = 5)
outer_cv$instantiate(task_imputed)

# 定义特征选择函数（使用随机森林重要性）
select_features <- function(inner_task, threshold = 0.7, nselect = NULL) {
  # 如果没有指定要选择的特征数，则默认选择50%的特征
  if(is.null(nselect)) {
    # 排除目标变量后的特征数量
    feature_count <- ncol(inner_task$data()) - 1
    nselect <- ceiling(feature_count * 0.5)
  }
  
  # 创建随机森林重要性筛选器
  filter <- flt("importance", learner = lrn("classif.ranger"),importance = "impurity")
  filter$calculate(inner_task)
  
  # 检查scores的结构并适当处理
  selected <- filter$scores
  
  # 打印调试信息
  cat("特征选择器返回的结构:", class(selected), "\n")
  
  selected <- sort(selected, decreasing = TRUE)
  selected <- head(selected, nselect)
  return(names(selected))
}

# 在第五步循环中添加存储结构
best_params <- list()
best_features <- list()

# 遍历所有模型进行评估
for (i in seq_along(learners)) {# i = 1
  cat("处理模型:", learners[[i]]$id, "\n")
  
  learner <- learners[[i]]
  learner_id <- learner$id
  
  # 获取当前模型的超参数空间
  param_set <- NULL
  if (learner_id == "logistic") {
    param_set <- learner_params$logistic
  } else if (learner_id == "svm") {
    param_set <- learner_params$svm
  } else if (learner_id == "lasso") {
    param_set <- learner_params$lasso
  } else if (learner_id == "elastic_net") {
    param_set <- learner_params$elastic_net
  } else if (learner_id == "ridge") {
    param_set <- learner_params$ridge
  } else if (learner_id == "random_forest") {
    param_set <- learner_params$ranger
  } else if (learner_id == "xgboost") {
    param_set <- learner_params$xgboost
  } else if (learner_id == "lightgbm") {
    param_set <- learner_params$lightgbm
  } else if (learner_id == "neural_network") {
    param_set <- learner_params$nnet
  }
  
  # 存储每个折叠的预测结果
  cv_predictions <- list()
  selected_features_model <- list()
  
  # 在外部交叉验证的每个折叠上循环
  for (fold in seq_len(outer_cv$iters)) {# fold = 1
    cat(sprintf("  正在处理折叠 %d/%d\n", fold, outer_cv$iters))
    
    # 获取训练集和测试集
    train_set <- outer_cv$train_set(fold)
    test_set <- outer_cv$test_set(fold)
    
    # 创建训练任务
    inner_task <- task_imputed$clone()
    inner_task$filter(train_set)
    
    # 特征选择
    selected_features <- select_features(inner_task)
    selected_features_model[[fold]] <- selected_features
    
    # 对训练任务应用特征筛选
    inner_task$select(selected_features)
    
    # 设置内部交叉验证
    inner_cv <- rsmp("cv", folds = 5)
    inner_cv$instantiate(inner_task)
    
    # 使用auto_tuner函数替代AutoTuner$new
    at <- auto_tuner(
      tuner = tnr("random_search"),
      learner = learner,
      resampling = inner_cv,
      measure = msr("classif.auc"),
      search_space = param_set,
      terminator = trm("evals", n_evals = 20)
    )
    
    # 训练模型
    at$train(inner_task)
    
    # 准备测试数据
    test_task <- task_imputed$clone()
    test_task$filter(test_set)
    test_task$select(selected_features)
    
    # 预测并存储结果
    prediction <- at$predict(test_task)
    cv_predictions[[fold]] <- prediction
    
    # 计算并记录性能指标
    perf <- list()
    for (measure in measures) {
      perf[[measure$id]] <- measure$score(prediction)
    }
    perf$fold <- fold
    perf$learner <- learner_id
    results <- rbindlist(list(results, as.data.table(perf)), fill = TRUE)
    
    cat(sprintf("    折叠 %d 完成 - AUC: %.4f\n", fold, perf$classif.auc))
  }
  
  # 保存所选特征
  selected_features_all[[learner_id]] <- selected_features_model
  
  # 在折叠循环结束后添加
  # 获取最佳参数组合
  best_params[[learner_id]] <- at$tuning_result[, .SD[which.max(classif.auc)]]
  
  # 获取特征出现频率
  feature_freq <- table(unlist(selected_features_model))
  best_features[[learner_id]] <- names(sort(feature_freq, decreasing = TRUE))
}

# ========== 第六步：结果分析 ==========
# 计算每个模型的平均性能
model_performance <- results[, lapply(.SD, mean), by = "learner", 
                             .SDcols = c("classif.auc", "classif.acc", "classif.sensitivity", 
                                         "classif.specificity", "classif.fbeta")]

# 按AUC排序以找出最佳模型
model_performance <- model_performance[order(-classif.auc)]

# 输出性能表
print(model_performance)

# 可视化结果
perf_plot <- ggplot(results, aes(x = learner, y = classif.auc, fill = learner)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "各模型AUC表现", x = "模型", y = "AUC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(perf_plot)
ggsave("model_performance.png", perf_plot, width = 10, height = 6)

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

# 保存结果以便后续分析
save(results, model_performance, selected_features_all, 
     best_params, best_features, file = "model_results.RData")

# ========== 第七步：绘制ROC曲线 ==========
# 初始化存储预测结果的数据框
all_predictions <- data.frame()

# 整合所有模型的预测结果
for (learner_name in names(selected_features_all)) {
  if (!exists(paste0("cv_predictions_", learner_name))) {
    next
  }
  
  learner_predictions <- get(paste0("cv_predictions_", learner_name))
  
  for (fold in seq_along(learner_predictions)) {
    pred <- learner_predictions[[fold]]
    # 提取真实标签和预测概率
    truth <- pred$truth
    prob <- pred$prob[, "1"]  # 假设正类标签为"1"
    
    # 合并到总数据框
    fold_df <- data.frame(
      learner = learner_name,
      fold = fold,
      truth = truth,
      prob = prob
    )
    all_predictions <- rbind(all_predictions, fold_df)
  }
}

# 使用pROC包绘制ROC曲线
library(pROC)

# 为每个模型绘制ROC曲线
plot_roc_curves <- function() {
  # 获取唯一的模型名称
  models <- unique(all_predictions$learner)
  
  # 创建空白的ROC图
  plot(x = c(0, 1), y = c(0, 1), type = "l", lty = 2, 
       xlab = "1 - 特异度", ylab = "敏感度", 
       main = "各模型ROC曲线比较", col = "gray")
  
  # 颜色向量
  colors <- rainbow(length(models))
  
  # 为每个模型添加ROC曲线
  for (i in seq_along(models)) {
    model_data <- all_predictions[all_predictions$learner == models[i], ]
    model_roc <- roc(response = model_data$truth, 
                    predictor = model_data$prob, 
                    levels = c("0", "1"))
    
    # 绘制ROC曲线
    lines(1 - model_roc$specificities, model_roc$sensitivities, 
          col = colors[i], lwd = 2)
    
    # 在图例中包含AUC值
    auc_val <- auc(model_roc)
    models[i] <- paste0(models[i], " (AUC = ", round(auc_val, 3), ")")
  }
  
  # 添加图例
  legend("bottomright", legend = models, col = colors, lwd = 2)
}

# 为每个模型分别绘制ROC曲线
plot_roc_curves_separate <- function() {
  # 获取唯一的模型名称
  models <- unique(all_predictions$learner)
  
  # 创建多面板图
  par(mfrow = c(2, ceiling(length(models)/2)))
  
  for (model in models) {
    model_data <- all_predictions[all_predictions$learner == model, ]
    model_roc <- roc(response = model_data$truth, 
                     predictor = model_data$prob, 
                     levels = c("0", "1"))
    
    # 绘制ROC曲线
    plot(model_roc, main = paste(model, "- AUC:", round(auc(model_roc), 3)),
         col = "blue", lwd = 2)
    # 添加参考线
    abline(a = 0, b = 1, lty = 2, col = "gray")
  }
  
  # 恢复图形参数
  par(mfrow = c(1, 1))
}

# 执行绘图
plot_roc_curves()
ggsave("roc_curves_comparison.png", width = 10, height = 8)

plot_roc_curves_separate()
ggsave("roc_curves_separate.png", width = 12, height = 10)

# ========== 第八步：构建最终预测模型 ==========
# 选择最佳模型（假设随机森林表现最好）
final_learner <- lrn("classif.ranger", 
                    predict_type = "prob",
                    num.trees = best_params$random_forest$num.trees,
                    mtry = best_params$random_forest$mtry,
                    min.node.size = best_params$random_forest$min.node.size)

# 使用最佳特征子集（取前80%高频特征）
final_features <- best_features$random_forest[1:floor(length(best_features$random_forest)*0.8)]

# 创建最终任务
final_task <- TaskClassif$new(
  id = "final_model",
  backend = task_imputed$data()[, c("feature13", final_features), with = FALSE],
  target = "feature13",
  positive = "1"
)

# 训练最终模型
final_model <- final_learner$train(final_task)

# 保存模型和特征列表
saveRDS(final_model, "final_model.rds")
writeLines(final_features, "final_features.txt")

# ========== 第九步：新样本预测流程 ==========
#加载模型和特征
final_model <- readRDS("final_model.rds")
final_features <- readLines("final_features.txt")

#数据预处理管道
preprocess_newdata <- function(new_data) {
  # 1. 应用相同的特征筛选
  new_data <- new_data[, final_features, with = FALSE]
  
  # 2. 应用相同的缺失值处理
  new_data <- impute_continuous_new(new_data)
  new_data <- impute_categorical_new(new_data)
  
  # 3. 移除相同特征
  new_data <- new_data[, !grepl("feature24_", names(new_data))]
  
  return(new_data)
}

#进行预测
predict_new <- function(new_data) {
  # 预处理
  processed_data <- preprocess_newdata(new_data)
  
  # 创建预测任务
  pred_task <- TaskClassif$new(
    id = "prediction",
    backend = processed_data,
    target = "feature13",  # 如果新数据没有目标变量，需要特殊处理
    positive = "1"
  )
  
  # 进行预测
  prediction <- final_model$predict(pred_task)
  
  # 返回概率和类别
  return(data.table(
    prob = prediction$prob[, "1"],
    class = prediction$response
  ))
}

# 新增的函数
monitor_drift <- function(new_data) {
  # 计算特征分布差异
  train_stats <- readRDS("train_feature_stats.rds")
  new_stats <- lapply(new_data[, final_features, with = FALSE], function(x) {
    if(is.numeric(x)) c(mean = mean(x), sd = sd(x)) 
    else table(x)/length(x)
  })
  
  # 计算KL散度等分布差异指标
  drift_scores <- mapply(function(t, n) {
    if(is.numeric(t)) {
      abs(t["mean"] - n["mean"])/t["sd"]
    } else {
      sqrt(sum((t - n)^2))
    }
  }, train_stats, new_stats)
  
  return(drift_scores)
}
