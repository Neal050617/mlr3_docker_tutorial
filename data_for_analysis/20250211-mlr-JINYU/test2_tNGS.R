setwd("/Users/colinliu/Nutstore\ Files/我的坚果云/R/mlr3_docker_tutorial/data_for_analysis/20250211-mlr-JINYU/")
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
library(pROC)
library(data.table)
# 设置并行计算
future::plan("multisession", workers = 6) # 使用6个并行工作进程
set.seed(123)

# 加载数据
load("./step1.RData")

mytheme <- function() {
  theme_bw() +
    theme(
      plot.title = element_text(size = rel(1), hjust = 0.5),
      axis.title = element_text(size = rel(1)),
      axis.text.x = element_text(
        size = rel(1), angle = 90,
        vjust = 0.5, hjust = 0.5, color = "black"
      ),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white"),
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key.size = unit(.4, "cm")
    )
}

roc_pict <- function(pod, title = "", path = "./roc_plot") {
  # 确保输入格式正确
  if (!all(c("d", "m") %in% names(pod))) {
    pod <- data.frame(
      d = pod[, 1], # 真实标签
      m = pod[, 2] # 预测概率
    )
  }

  # 计算AUC及置信区间
  roc_obj <- roc(pod$d, pod$m)
  auc_value <- round(auc(roc_obj), 3)
  ci_obj <- ci.auc(roc_obj)

  # 创建标注文本
  auc_text <- paste0(
    "AUC = ", auc_value, "\n",
    "95% CI: ", round(ci_obj[1], 3), "-",
    round(ci_obj[3], 3)
  )

  # 生成ROC曲线
  p <- ggplot(pod, aes(d = d, m = m)) +
    geom_roc(n.cuts = 0, color = "steelblue") +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", color = "grey40"
    ) +
    mytheme() +
    ggtitle(title) +
    labs(
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)"
    ) +
    annotate("text",
      x = 0.65, y = 0.25,
      label = auc_text, size = 5
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

  # 保存图形
  ggsave(paste0(path, ".pdf"), p,
    width = 8, height = 6, units = "in", dpi = 300
  )

  return(p) # 仅返回单个图形对象
}

# 数据准备
data <- data14 |>
  mutate(!!sym(feature_names$group) := as.factor(!!sym(feature_names$group)))

# 创建分类任务
pbp_task <- TaskClassif$new(
  id = "lasso_logistic",
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

# 定义预处理管道
pbp_prep <- po("imputehist", affect_columns = selector_name(continuous_cols)) %>>%
  po("imputemode", affect_columns = selector_name(categorical_cols)) %>>%
  po("removeconstants") %>>%
  po("select",
    selector = selector_invert(
      selector_grep(paste0("^", feature_names$tNGS))
    )
  ) # %>>% # 对LASSO很重要：标准化特征
# po("scale",affect_columns = selector_name(continuous_cols))
task_prep <- pbp_prep$clone()$train(pbp_task)[[1]]
dim(task_prep$data())

# 数据划分
split_task <- partition(task = pbp_task, ratio = 0.9)
task_train <- pbp_task$clone()$filter(split_task$train)
task_test <- pbp_task$clone()$filter(split_task$test)

# 定义基础学习器 (逻辑回归 + LASSO)
learner_base_glmnet <- lrn("classif.glmnet", predict_type = "prob", lambda = 1)

# 定义参数搜索空间
glmnet_params <- ps(
  classif.glmnet.lambda = p_dbl(lower = 0.0001, upper = 1, logscale = TRUE), # lambda值，LASSO惩罚系数
  classif.glmnet.alpha = p_dbl(lower = 0, upper = 1), # 弹性网络混合参数：0=岭回归，0.5=弹性网络，1=LASSO
  classif.glmnet.standardize = p_lgl(), # 是否在模型内部标准化
  classif.glmnet.thresh = p_dbl(lower = 1e-7, upper = 1e-4, logscale = TRUE), # 收敛阈值
  classif.glmnet.maxit = p_int(lower = 1e3, upper = 1e4) # 最大迭代次数
)

# 定义终止条件
# 更优化的终止条件设置
terminator <- trm("combo",
  terminators = list(
    # 性能停滞终止
    trm("stagnation",
      iters = 10, # 观察最近15次迭代
      threshold = 0.005
    ), # 性能改善阈值
    # 评估次数终止
    trm("evals", n_evals = 50),
    # 运行时间终止
    trm("run_time", secs = 3600), # 1小时时间限制
    # 目标性能终止
    trm("perf_reached",
      level = 0.85
    ) # 达到85%准确率就停止
  )
)

# 定义调优器
tuner <- tnr("random_search", batch_size = 100)

# 定义重抽样方案
inner_resampling <- rsmp("cv", folds = 5)
outer_resampling <- rsmp("cv", folds = 5)

# 创建自动调优器
at <- auto_tuner(
  tuner = tuner,
  learner = as_learner(pbp_prep %>>% learner_base_glmnet),
  resampling = inner_resampling,
  measure = msr("classif.auc"), # "classif.ce"
  search_space = glmnet_params,
  terminator = terminator,
  store_tuning_instance = TRUE
)

# 使用嵌套重抽样评估
rr <- resample(
  task = task_train,
  learner = at,
  resampling = outer_resampling,
  store_models = TRUE
)

# 输出性能指标
print("嵌套重抽样性能评估结果:")
print(rr$score(msrs(c("classif.ce", "classif.acc", "classif.auc"))))
save.image("step2.elasticnet.RData")

# load("./step2.0.RData")
# 访问基准测试结果
# 1. 提取外部交叉验证结果绘制ROC（反映模型泛化能力）
library(mlr3viz)
q <- autoplot(rr, type = "roc") +
  labs(
    title = "Nested Cross-Validation ROC",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme(text = element_text(family = "Times New Roman")) +
  mytheme()
ggsave("step2.nested.elasticnet.roc.pdf", q, width = 12.5, height = 12, units = "in", dpi = 300)
Aggr_AUC <- rr$aggregate(msr("classif.auc"))
print("Aggregated AUC Performance:")
print(Aggr_AUC)

best_run_AUC <- rr$score(msr("classif.auc")) %>%
  arrange(desc(classif.auc)) %>%
  .[1] %>%
  .$classif.auc
print("\nBest Run (Highest AUC):")
print(best_run_AUC)

# 2. 查看最优模型的超参数：
tuned_learner <- rr$learners[[1]] # 获取第一个重抽样迭代的 AutoTuner 模型
tuning_instance <- tuned_learner$tuning_instance
best_params <- tuning_instance$result_learner_param_vals
print("\nBest Hyperparameters:")
print(best_params)

# 3. 查看最优模型使用了哪些特征，以及特征系数：
final_model <- at$train(task_train) # 使用自动调优器训练完整训练集

# 方法一：通过管道结构获取最终模型
# final_pipeop <- final_model$model$graph_model$pipeops$classif.glmnet
# final_glmnet <- final_pipeop$learner_model$model
# coef_matrix <- coef(final_glmnet, s = best_params$classif.glmnet.lambda)

# 方法二：直接访问AutoTuner的底层模型
final_glmnet <- final_model$learner$model$classif.glmnet$model
coef_matrix <- coef(final_glmnet, s = best_params$classif.glmnet.lambda)

coeffs_dt <- as.data.table(as.matrix(coef_matrix))
coeffs_dt[, feature := rownames(coef_matrix)]
setcolorder(coeffs_dt, c("feature", "s1"))

# 转换为数据框并处理结果
coeffs_dt <- as.data.table(as.matrix(coef_matrix), keep.rownames = "feature") %>%
  data.table::setnames("s1", "coefficient") %>%
  .[abs(coefficient) > 1e-8 & feature != "(Intercept)"] %>% # 过滤有效特征
  .[order(-abs(coefficient))] # 按系数绝对值排序

print("\n=== 最终保留的特征（非零系数）===")
print(coeffs_dt[, .(feature, coefficient)])

print("\n=== 被LASSO消除的特征 ===")
zero_features <- setdiff(task_train$feature_names, coeffs_dt$feature)
print(zero_features)

# 4. 在独立测试集上评估并绘制ROC（反映最终模型真实性能）
test_pred <- final_model$predict(task_test)
roc_pict(test_pred, title = "独立测试集ROC曲线", path = "./independent_test_roc")

# 输出测试集性能
print("测试集性能评估结果:")
print(test_pred$score(msrs(c("classif.ce", "classif.acc", "classif.auc"))))

# 快速绘制测试集ROC（不使用roc_pict）
library(ggplot2)
library(plotROC)

# 生成测试集ROC数据
# 更稳健的转换方式（无需手动-1）
test_roc <- data.frame(
  truth = as.integer(test_pred$truth == task_train$positive), # 自动生成0/1
  prob = test_pred$prob[, task_train$positive]
)

# 验证目标变量的因子水平顺序
cat("目标变量因子水平顺序：")
print(task_train$col_info[id == "feature13", levels])

# 显式设置因子水平顺序（如需调整）
# task_train$col_info[id == "feature13", levels := list(c("0", "1"))]

# 2. 绘制最佳模型在测试集上的ROC曲线
# 使用之前定义的roc_pict函数绘制ROC曲线
roc_pict(pod = test_roc, title = "Independent_test_ROC_curve")

# 2. 绘制各项指标的箱线图
performance_data <- rr$score(msrs(c("classif.ce", "classif.acc", "classif.auc", "classif.sensitivity", "classif.specificity"))) |>
  tidyr::pivot_longer(
    cols = starts_with("classif."),
    names_to = "metric",
    values_to = "value"
  )

q <- ggplot(performance_data, aes(x = metric, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Performance Metrics Distribution",
    x = "Evaluation Metric",
    y = "Value"
  )
ggsave("step2.nested.elasticnet.performance.pdf", q, width = 12.5, height = 12, units = "in", dpi = 300)

# 保存最终模型和特征
# saveRDS(final_model, "final_model.rds")
# 创建模型信息汇总表
model_summary <- data.table(
  项目 = c(
    "交叉验证AUC", "最佳单次AUC", "正则化参数λ", "弹性网络α",
    "保留特征数", "零系数特征数", "训练样本量", "测试样本量"
  ),
  值 = c(
    round(rr$aggregate(msr("classif.auc")), 4),
    round(max(rr$score(msr("classif.auc"))$classif.auc), 4),
    signif(best_params$classif.glmnet.lambda, 3),
    signif(best_params$classif.glmnet.alpha, 2),
    nrow(coeffs_dt),
    length(zero_features),
    task_train$nrow,
    task_test$nrow
  )
)

# 创建特征系数表
feature_table <- coeffs_dt[, .(
  特征名称 = feature,
  系数值 = round(coefficient, 4),
  重要性 = scales::percent(abs(coefficient) / max(abs(coefficient)))
)][order(-abs(系数值))]

# 创建超参数表
param_table <- data.table(
  参数 = names(best_params),
  值 = vapply(best_params, function(x) {
    if (is.function(x)) {
      "function"
    } else if (is.list(x)) {
      paste(x, collapse = ",")
    } else {
      as.character(x)
    }
  }, character(1))
)[, 描述 := fcase(
  参数 == "classif.glmnet.lambda", "Regularization strength (higher = stronger feature selection)",
  参数 == "classif.glmnet.alpha", "Elastic net mixing parameter (0=Ridge, 1=LASSO)",
  参数 == "classif.glmnet.standardize", "Standardize features internally",
  参数 == "classif.glmnet.thresh", "Convergence threshold",
  参数 == "classif.glmnet.maxit", "Maximum iterations"
)][, 值 := as.character(值)] # 确保所有值转换为字符型

# 3. 输出每个fold的最佳参数（安全版本）
print("\nBest parameters for each fold:")
fold_params <- map_dfr(seq_len(rr$iters), function(i) { # 使用$iters替代$n_resample_results
  # 安全访问参数
  params <- tryCatch(
    rr$learners[[i]]$tuning_instance$result_learner_param_vals,
    error = function(e) NULL
  )

  # 构建安全数据框
  if (!is.null(params)) {
    data.frame(
      Fold = i,
      Lambda = params$classif.glmnet.lambda %||% NA_real_,
      Alpha = params$classif.glmnet.alpha %||% NA_real_,
      Standardize = params$classif.glmnet.standardize %||% NA,
      Thresh = params$classif.glmnet.thresh %||% NA_real_,
      Maxit = params$classif.glmnet.maxit %||% NA_integer_,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Fold = i,
      Lambda = NA_real_,
      Alpha = NA_real_,
      Standardize = NA,
      Thresh = NA_real_,
      Maxit = NA_integer_
    )
  }
})

# 添加检查逻辑
if (nrow(fold_params) == 0) {
  stop("No tuning parameters found. Check if tuning was properly executed.")
}

# 4. 计算并输出聚合指标
print("\n平均性能指标:")
aggregate_scores <- performance_data |>
  group_by(metric) |>
  summarise(
    mean = mean(value),
    sd = sd(value),
    median = median(value),
    min = min(value),
    max = max(value)
  )
print(aggregate_scores)

# 输出美观表格
# library(kableExtra)
# kable(model_summary, caption = "模型性能总览") %>%
#  kable_styling(bootstrap_options = "striped", full_width = F)
#
# kable(feature_table[1:10], caption = "Top 10重要特征") %>%
#  kable_styling(bootstrap_options = "striped") %>%
#  column_spec(3, color = "white", background = "#E41A1C")
#
# kable(param_table, caption = "最优超参数配置") %>%
#  kable_styling(bootstrap_options = "striped") %>%
#  column_spec(2, bold = T)

# 保存为Excel工作簿
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "模型总览")
writeData(wb, "模型总览", model_summary)
addWorksheet(wb, "特征分析")
writeData(wb, "特征分析", feature_table)
addWorksheet(wb, "参数配置")
writeData(wb, "参数配置", param_table)
addWorksheet(wb, "每个fold的最佳参数")
writeData(wb, "每个fold的最佳参数", fold_params)
addWorksheet(wb, "平均性能指标")
writeData(wb, "平均性能指标", aggregate_scores)
saveWorkbook(wb, "model_report.xlsx", overwrite = TRUE)
