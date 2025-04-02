setwd("/Users/colinliu/Nutstore\ Files/我的坚果云/R/mlr3_docker_tutorial/data_for_analysis/20250211-mlr-JINYU/")
# setwd("~/analysis/data_for_analysis/20250211-mlr-JINYU/")
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
# lgr::get_logger("mlr3")$set_threshold("warn")
# lgr::get_logger("bbotk")$set_threshold("warn")
# 恢复默认屏幕输出 (显示 info 级别及以上的消息)
# lgr::get_logger("mlr3")$set_threshold("info")
# lgr::get_logger("bbotk")$set_threshold("info")

# ========== 第二步：数据处理 ==========
# 准备数据
data <- data14 |>
    mutate(!!sym(feature_names$group) := as.factor(!!sym(feature_names$group))) # |>
#  select(-starts_with("feature24_")) # -starts_with("feature17_"),

# 创建分类任务
pbp_task <- TaskClassif$new(
    id = "model_comparison",
    backend = data,
    target = "feature13",
    positive = "1"
)

pbp_prep <- po("imputehist", affect_columns = selector_name(continuous_cols)) %>>%
    po("imputemode", affect_columns = selector_name(categorical_cols)) %>>%
    po("removeconstants")

task_prep <- pbp_prep$clone()$train(pbp_task)[[1]]
dim(task_prep$data())

# 数据划分
split_task <- partition(task = pbp_task, ratio = 0.9)

task_train <- pbp_task$clone()$filter(split_task$train)
task_test <- pbp_task$clone()$filter(split_task$test)

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
# 首先注册回调
mlr3misc::dictionary_callbacks$add("custom_log_reg_lasso_callback", R6::R6Class("CallbackRFELogRegLasso",
    inherit = mlr3::CallbackRFE,
    public = list(
        callback_rfe_before_select = function(callback, instance, learner, feature_set, resampling, measure, fselector, ...) {
            if (!inherits(learner$model, "glm")) {
                stop("log_reg_lasso_callback 回调函数仅适用于线性模型 (classif.log_reg).")
            }

            model <- learner$model
            coefficients <- coef(model)
            feature_names <- names(coefficients)[!names(coefficients) == "(Intercept)"]
            importance_values <- coefficients[feature_names]
            selected_features <- feature_names[abs(importance_values) > 1e-8]

            cat(paste0(
                "[Lasso Feature Selection] Iteration ", instance$iteration, ": Selected features: ",
                paste(selected_features, collapse = ", "), "\n"
            ))

            private$.selected_features[[instance$iteration]] <- selected_features
            instance$feature_importance <- data.table(
                feature = feature_names,
                importance = as.numeric(importance_values)
            )
        }
    ),
    private = list(
        .selected_features = list()
    )
))

# 然后创建回调实例
log_reg_lasso_callback <- clbk("custom_log_reg_lasso_callback")

#learners <- list(
#    lrn("classif.ranger", predict_type = "prob", importance = "impurity"),
#    lrn("classif.xgboost",
#        id = "xgb", nrounds = max_nrounds,
#        early_stopping_rounds = 20, validate = "test"
#    ),
#    lrn("classif.svm", id = "svm", type = "C-classification", kernel = "linear")
#    # lrn("classif.glmnet",predict_type = "prob", lambda = 1)
#)
# 定义基础学习器和它们的参数搜索空间
max_nrounds <- 500
learners <- list(
    # 随机森林参数空间
    rf = list(
        learner = lrn("classif.ranger",
            predict_type = "prob",
            importance = "impurity"
        ),
        search_space = ps(
            mtry = p_int(lower = 2, upper = 20),
            min.node.size = p_int(lower = 1, upper = 10),
            num.trees = p_int(lower = 100, upper = 1000)
        )
    ),
    # XGBoost参数空间
    xgb = list(
        learner = lrn("classif.xgboost",
            nrounds = max_nrounds,
            early_stopping_rounds = 20,
            validate = "test"
        ),
        search_space = ps(
            eta = p_dbl(lower = 0.01, upper = 0.3),
            max_depth = p_int(lower = 1, upper = 10),
            subsample = p_dbl(lower = 0.5, upper = 1),
            colsample_bytree = p_dbl(lower = 0.5, upper = 1)
        )
    ),
    # SVM参数空间
    svm = list(
        learner = lrn("classif.svm",
            type = "C-classification",
            kernel = "linear"
        ),
        search_space = ps(
            cost = p_dbl(lower = 0.1, upper = 10, logscale = TRUE)#,
            #gamma = p_dbl(lower = 0.1, upper = 10, logscale = TRUE)
        )
    ),
    # glmnet_lasso 参数空间  <--  添加了 glmnet_lasso 的定义
    glmnet_lasso = list(
       learner = lrn("classif.glmnet", predict_type = "prob", lambda = 1), # 使用 learner_base_glmnet
       search_space = ps( # 使用 glmnet_params
           classif.glmnet.lambda = p_dbl(lower = 0.0001, upper = 1, logscale = TRUE),
           #  alpha = p_fct(levels = c("0.5", "1")),
           classif.glmnet.standardize = p_lgl(),
           classif.glmnet.thresh = p_dbl(lower = 1e-7, upper = 1e-4, logscale = TRUE),
           classif.glmnet.maxit = p_int(lower = 1e3, upper = 1e4)
       )
   )
)

# 定义调优方法
tuner <- tnr("grid_search", resolution = 5)
# 存储 AutoTuner 对象的列表
auto_tuners <- list()

# 循环遍历 learners 列表
for (learner_name in names(learners)) {
    # 从 learners 列表中提取当前模型的 learner 和 search_space
    current_learner <- learners[[learner_name]]$learner
    current_search_space <- learners[[learner_name]]$search_space

    # 创建 AutoTuner 对象
    at <- AutoTuner$new(
        learner = current_learner, #  使用当前模型的学习器
        resampling = rsmp("cv", folds = 5), #  重抽样策略 (所有模型通用)
        measure = msr("classif.acc"), #  性能度量 (所有模型通用)
        search_space = current_search_space, #  使用当前模型的参数空间
        terminator = trm("evals", n_evals = 25), #  终止条件 (所有模型通用)
        tuner = tuner #  调优器 (所有模型通用)
    )

    # 设置 AutoTuner 对象的 ID，方便后续识别
    at$id <- paste0("at_", learner_name) # 例如 "at_rf", "at_xgb", "at_svm", "at_glmnet_lasso"

    # 将创建的 AutoTuner 对象存储到列表中
    auto_tuners[[learner_name]] <- at
}

# 现在 auto_tuners 列表中就包含了所有模型的 AutoTuner 对象
print(auto_tuners)
# 由于 SVM 本身不支持 importance 分数，我们通过回调将训练好的线性 SVM 模型系数转换为重要性分数
svm_rfe <- clbk("mlr3fselect.svm_rfe")
# 由于 XGBoost 学习器通过提前停止来执行内部调整，而内部交叉验证重采样方案中的测试折叠作为验证集，因此我们需要定义以下回调
internal_ss <- ps(
    nrounds = p_int(upper = max_nrounds, aggr = function(x) as.integer(mean(unlist(x))))
)
xgb_clbk <- clbk("mlr3fselect.internal_tuning", internal_search_space = internal_ss)
one_se_clbk <- clbk("mlr3fselect.one_se_rule")

efs <- ensemble_fselect(
    fselector = rfe,
    task = task,
    learners = learners,
    init_resampling = rsmp("subsampling", repeats = 50, ratio = 0.8),
    inner_resampling = rsmp("cv", folds = 5),
    inner_measure = msr("classif.ce"),
    measure = msr("classif.acc"),
    terminator = trm("none"),
    # following list must be named with the learners' ids
    callbacks = list(
        xgb  = list(one_se_clbk, xgb_clbk),
        rf   = list(one_se_clbk),
        svm  = list(one_se_clbk, svm_rfe),
        log_reg_lasso = list(log_reg_lasso_callback)
    ),
    store_benchmark_result = FALSE
)
