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
# library(lightgbm)
library(pROC)
library(data.table)
library(showtext)


# 安装并加载Boruta包(mlr3filters的Boruta功能依赖此包)
library(Boruta)

# 设置并行计算
future::plan("multisession", workers = 12) # 使用6个并行工作进程
set.seed(123)
mlr3verse_info()

# 加载数据
load("./step1.RData")
# write_tsv
data14 %>% rownames_to_column("SampleID") %>% write_tsv("step1_data14.xls")

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
  # pod = test_pred; path = "./independent_test_roc"
  # 确保输入格式正确
  if (!all(c("d", "m") %in% names(pod))) {
    pod <- data.frame(
      d = pod[,1],  # 真实标签
      m = pod[,2]   # 预测概率
    )
  }
  
  # 计算AUC及置信区间
  roc_obj <- roc(pod$d, pod$m)
  auc_value <- round(auc(roc_obj), 3)
  ci_obj <- ci.auc(roc_obj)
  
  # 创建标注文本
  auc_text <- paste0("AUC = ", auc_value, "\n",
                    "95% CI: ", round(ci_obj[1],3), "-", 
                    round(ci_obj[3],3))
  
  # 生成ROC曲线
  p <- ggplot(pod, aes(d = d, m = m)) + 
    geom_roc(n.cuts = 0, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, 
                linetype = "dashed", color = "grey40") +
    mytheme() +
    ggtitle(title) +
    labs(x = "False Positive Rate (1 - Specificity)",
         y = "True Positive Rate (Sensitivity)") +
    annotate("text", x = 0.65, y = 0.25, 
             label = auc_text, size = 5) +
    coord_cartesian(xlim = c(0,1), ylim = c(0,1))

  # 保存图形
  ggsave(paste0(path, ".pdf"), p, 
         width = 8, height = 6, units = "in", dpi = 300)
  
  return(p)  # 仅返回单个图形对象
}

# 数据准备
data <- data14 |>
  mutate(!!sym(feature_names$group) := as.factor(!!sym(feature_names$group)))

# 创建分类任务
pbp_task <- TaskClassif$new(
  id = "jinyu_task", # 任务ID
  backend = data, # 数据集
  target = "feature13", # 目标变量
  positive = "1" # 正类标签
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
pbp_prep = po("imputehist", affect_columns = selector_name(continuous_cols)) %>>% 
  po("imputemode", affect_columns = selector_name(categorical_cols)) %>>% 
  po("removeconstants") %>>%
#  po("filter", filter = flt("variance"), param_vals = list(frac = 0.2)) %>>%  # 保留方差前20%特征
  po("select", selector = selector_invert(selector_grep(paste0("^", feature_names$clinic)))
  ) #%>>% # 对LASSO很重要：标准化特征 
  #po("scale",affect_columns = selector_name(continuous_cols))
task_prep <- pbp_prep$clone()$train(pbp_task)[[1]]
dim(task_prep$data())

task_prep$data() %>%
    rownames_to_column("SampleID") %>%
    write_tsv("step2_data14.xls")

# 数据划分 - 使用预处理后的任务
split_task <- partition(task = task_prep, ratio = 0.9)
task_train <- task_prep$clone()$filter(split_task$train)
task_test <- task_prep$clone()$filter(split_task$test)

# 定义嵌套交叉验证的内外层重采样策略 --------------------------------------
inner_resampling <- rsmp("cv", folds = 5)  # 内层5折交叉验证用于超参数调优
outer_resampling <- rsmp("cv", folds = 5) # 外层10折交叉验证用于性能评估

# 定义特征筛选方法 -----------------------------------------------------
# 方法1: Boruta特征选择 (按照官方文档修正参数)
boruta_filter <- flt("boruta", maxRuns = 100)

# 方法2: RFE递归特征消除
# 在mlr3pipelines中，RFE操作器(po("rfe"))允许两种模式：
# 1. 带有自己的resampling：
# 2. 使用外部提供的resampling：不指定resampling参数时，它会使用上层提供的resampling
rfe_filter <- fs("rfe",
    #learner = lrn("classif.ranger", importance = "permutation"),
    n_features = to_tune(5, 20),
    feature_fraction = to_tune(0.6, 0.9)#,
    #resampling = rsmp("cv", folds = 3)
)
ranger_learner <- lrn("classif.ranger", importance = "permutation")

# 方法3: 基于相关性的特征选择
# correlation_filter <- flt("jmim")  # 最大互信息最小化联合互信息

# 创建基础学习器 -----------------------------------------------------
learner_base_glmnet <- lrn("classif.glmnet", lambda = 0.1, alpha = 0.5, 
                         predict_type = "prob")
learner_base_xgb <- lrn("classif.xgboost", predict_type = "prob",
                       nrounds = 100, eta = 0.1, max_depth = 6)
learner_base_rf <- lrn("classif.ranger", predict_type = "prob",
                      num.trees = 500, importance = "impurity")
learner_base_svm <- lrn("classif.svm", predict_type = "prob",
                      type = "C-classification",  # 必须指定分类类型
                      kernel = "radial", cost = 1)

# 定义参数空间 ------------------------------------------------------------
model_params <- list(
    # Elastic Net参数
    glmnet = ps(
        # 核心正则化参数
        classif.glmnet.lambda = p_dbl(lower = 1e-4, upper = 1, logscale = TRUE),  # 正则化强度
        classif.glmnet.alpha = p_dbl(lower = 0, upper = 1),  # L1/L2混合比例
        # 预处理相关参数
        classif.glmnet.standardize = p_lgl(),  # 是否标准化特征
        classif.glmnet.thresh = p_dbl(lower = 1e-7, upper = 1e-4, logscale = TRUE),  # 收敛阈值
        classif.glmnet.maxit = p_int(lower = 1e3, upper = 1e4),  # 最大迭代次数
        # 特征筛选参数
        boruta.filter.frac = p_dbl(lower = 0.5, upper = 0.9)  # 保留特征比例
    ),
    
    # XGBoost参数
    xgboost = ps(
        classif.xgboost.eta = p_dbl(0.01, 0.3), # 学习率
        classif.xgboost.max_depth = p_int(1, 10), # 树的最大深度
        classif.xgboost.nrounds = p_int(50, 500), # 迭代次数
        classif.xgboost.min_child_weight = p_dbl(1, 10), # 最小子节点权重
        classif.xgboost.subsample = p_dbl(0.5, 1), # 样本采样比例
        classif.xgboost.colsample_bytree = p_dbl(0.5, 1), # 特征采样比例
        boruta.filter.frac = p_dbl(lower = 0.5, upper = 0.9)  # 保留特征比例
    ),
    
    # 随机森林参数
    rf = ps(
        classif.ranger.num.trees = p_int(lower = 500, upper = 1000),
        classif.ranger.max.depth =  p_int(lower = 3, upper = 15),
        classif.ranger.min.node.size = p_int(lower = 1, upper = 10),
        classif.ranger.mtry = p_int(1, 20), # 特征采样数量
        boruta.filter.frac = p_dbl(lower = 0.5, upper = 0.9)  # 保留特征比例
    ),
    
    # SVM参数
    svm = ps(
        classif.svm.cost = p_dbl(lower = -2, upper = 2, trafo = function(x) 10^x),
        classif.svm.kernel = p_fct(c("polynomial", "radial")),
        classif.svm.degree = p_int(1, 3, depends = classif.svm.kernel == "polynomial"),
        boruta.filter.frac = p_dbl(lower = 0.5, upper = 0.9)  # 保留特征比例
    )
)
# 查看boruta的参数
#mlr_filters$get("boruta")$param_set$ids()

# 创建终止条件 ----------------------------------------------------------
terminator <- trm("combo",
    terminators = list(        # 性能停滞终止
        trm("stagnation", iters = 10, threshold = 0.005), # 观察最近15次
        trm("evals", n_evals = 100), # 评估次数终止
        trm("run_time", secs = 3600), # 1小时时间限制
        trm("perf_reached", level = 0.85) # 目标性能终止
    ) # 达到85%准确率就停止
)
# 定义调优器
tuner <- tnr("random_search", batch_size = 100)

# 创建自动调优器 ----------------------------------------------------------
# 这里的关键是将特征选择作为管道的一部分,同时将筛选比例作为超参数进行调优
auto_tuners <- list(
    glmnet_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter, param_vals = list(maxRuns = 100)) %>>% 
            po("select") %>>% learner_base_glmnet
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$glmnet,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "glmnet_boruta"
    ),
    xgboost_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter) %>>% 
            po("select") %>>% learner_base_xgb
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$xgboost,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "xgboost_boruta"
    ),
    rf_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter) %>>% 
            po("select") %>>% learner_base_rf
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$rf,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "rf_boruta"
    ),
    svm_boruta = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("filter", filter = boruta_filter) %>>% 
            po("select") %>>% learner_base_svm
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$svm,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "svm_boruta"
    ),
    glmnet_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("learner_cv", ranger_learner) %>>% 
            po("fselect", fselector = rfe_filter) %>>% 
            po("select") %>>% learner_base_glmnet
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$glmnet,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "glmnet_rfe"
    ),
    xgboost_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner( # 直接进行特征选择 (数据已预处理)
            po("rfe", learner = rfe_pipeline) %>>% 
            po("select") %>>% learner_base_xgb
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$xgboost,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "xgboost_rfe"
    ),
    rf_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("rfe", learner = rfe_pipeline) %>>% 
            po("select") %>>% learner_base_rf
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$rf,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "rf_rfe"
    ),
    svm_rfe = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("rfe", learner = rfe_pipeline) %>>% 
            po("select") %>>% learner_base_svm
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$svm,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "svm_rfe"
    ),
    glmnet_boruta_scale = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("scale", affect_columns = selector_name(continuous_cols)) %>>% 
            po("filter", filter = boruta_filter, param_vals = list(maxRuns = 100)) %>>% 
            po("select") %>>% learner_base_glmnet
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$glmnet,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "glmnet_boruta_scale"
    ),
    glmnet_rfe_scale = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("scale", affect_columns = selector_name(continuous_cols)) %>>% 
            po("rfe", learner = rfe_pipeline) %>>% 
            po("select") %>>% learner_base_glmnet
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$glmnet,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "glmnet_rfe_scale"
    ),
    svm_boruta_scale = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("scale", affect_columns = selector_name(continuous_cols)) %>>% 
            po("filter", filter = boruta_filter) %>>% 
            po("select") %>>% learner_base_svm
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$svm,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "svm_boruta_scale"
    ),
    svm_rfe_scale = auto_tuner(
        tuner = tuner,
        learner = as_learner(# 直接进行特征选择 (数据已预处理)
            po("scale", affect_columns = selector_name(continuous_cols)) %>>% 
            po("rfe", learner = rfe_pipeline) %>>% 
            po("select") %>>% learner_base_svm
        ),
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = model_params$svm,
        terminator = terminator,
        store_tuning_instance = TRUE, id = "svm_rfe_scale"
    )   
)

# 整合所有学习器用于基准测试
learners_for_benchmark <- list(
    auto_tuners$glmnet_boruta,
    auto_tuners$xgboost_boruta,
    auto_tuners$rf_boruta,
    auto_tuners$svm_boruta,
    auto_tuners$glmnet_rfe,
    auto_tuners$xgboost_rfe,
    auto_tuners$rf_rfe,
    auto_tuners$svm_rfe,
    auto_tuners$glmnet_boruta_scale,
    auto_tuners$glmnet_rfe_scale,
    auto_tuners$svm_boruta_scale,
    auto_tuners$svm_rfe_scale
)

# 创建benchmark网格
benchmark_grid <- benchmark_grid(
    tasks = task_train, # 使用预处理后的任务train
    learners = learners_for_benchmark,
    resamplings = outer_resampling # 外层交叉验证
)

# 执行基准测试
bmr <- benchmark(benchmark_grid, store_models = TRUE)
#saveRDS(bmr, "benchmark_results.rds")
save.image("test2.1.RData")
# load("test2.1.RData")


# 聚合结果并展示性能
performance_results <- bmr$aggregate(msrs(c("classif.auc", "classif.acc", "classif.ce")))
print("模型性能比较:")
print(performance_results)
write_tsv(performance_results, "performance_results.xls")


# 提取内部调优结果
tuning_results <- extract_inner_tuning_results(bmr)
write_tsv(tuning_results, "tuning_results.xls")
# 提取内部调优过程
tuning_archives <- extract_inner_tuning_archives(bmr)
write_tsv(tuning_archives, "tuning_archives.xls")

# 2. 可视化比较
library(mlr3viz)
q <- autoplot(bmr, measure = msr("classif.auc")) +
    mytheme() +
    labs(title = "模型性能对比 - AUC")
ggsave("step2.nested.auc.pdf",q,width = 12.5,height = 12,units = 'in',dpi = 300)

p <- autoplot(bmr, type = "roc")
ggsave("step2.nested.roc.pdf",p,width = 12.5,height = 12,units = 'in',dpi = 300)












# 3. 选择最佳模型
best_results <- bmr$aggregate(msr("classif.auc"))
best_results$task_id <- c("glmnet", "xgboost", "rf", "svm")
best_model_id <- best_results[which.max(classif.auc), "learner_id"]
print(paste("最佳模型:", best_model_id))

# 获取最佳模型对象
best_model_parts <- strsplit(best_model_id, "\\.")[[1]]
best_model_type <- best_model_parts[length(best_model_parts)]
best_learner <- auto_tuners$rf[[best_model_type]]

# 4. 在完整训练集上训练最佳模型
best_learner$train(task_train)
final_model <- best_learner$model

# 5. 在测试集上验证
test_pred <- best_learner$predict(task_test)
test_performance <- test_pred$score(msrs(c("classif.auc", "classif.acc")))
print("测试集性能:")
print(test_performance)

# 绘制ROC曲线
test_roc_data <- data.frame(
    truth = as.integer(test_pred$truth == task_train$positive),
    prob = test_pred$prob[, task_train$positive]
)
roc_plot <- roc_pict(test_roc_data, title = paste("最佳模型:", best_model_type), 
                    path = "./final_model_roc")

# 6. 特征重要性分析
# 提取最佳模型使用的特征
get_selected_features <- function(learner) {
    # 从学习器管道中提取特征筛选结果
    if ("classif.ranger" %in% best_model_id) {
        # 对于随机森林,可直接获取特征重要性
        imp <- learner$model$classif.ranger$importance()
        if (is.null(imp)) return(NULL)
        
        return(data.table(
            Feature = names(imp),
            Importance = as.numeric(imp),
            Selected = TRUE
        ))
    } else if ("classif.xgboost" %in% best_model_id) {
        # 对于XGBoost模型
        imp <- xgb.importance(model = learner$model$classif.xgboost)
        if (is.null(imp)) return(NULL)
        
        return(data.table(
            Feature = imp$Feature,
            Importance = imp$Gain,
            Selected = TRUE
        ))
    } else if ("classif.glmnet" %in% best_model_id) {
        # 对于Elastic Net模型
        coefs <- coef(learner$model$classif.glmnet)
        nonzero_idx <- which(coefs[-1] != 0)
        feature_names <- rownames(coefs)[-1][nonzero_idx]
        
        return(data.table(
            Feature = feature_names,
            Importance = abs(coefs[-1][nonzero_idx]),
            Selected = TRUE
        ))
    }
    
    # 如果无法提取特征重要性,返回NULL
    return(NULL)
}

# 获取最佳模型的特征重要性
feature_importance <- get_selected_features(best_learner)

# 可视化重要特征
if (!is.null(feature_importance) && nrow(feature_importance) > 0) {
    top_n <- min(20, nrow(feature_importance))
    top_features <- feature_importance[order(-Importance)][1:top_n]
    
    p_importance <- ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        mytheme() +
        labs(x = "特征", y = "重要性分数", 
            title = paste0("Top ", top_n, " 重要特征"))
    
    ggsave("feature_importance.pdf", p_importance, width = 10, height = 8)
}

# 保存结果
saveRDS(bmr, "benchmark_results.rds")
save(best_learner, test_pred, performance_results, feature_importance, 
     file = "final_model.RData")

# 保存筛选后的特征列表
if (!is.null(feature_importance)) {
    selected_features <- feature_importance$Feature
    writeLines(selected_features, "selected_features.txt")
}
