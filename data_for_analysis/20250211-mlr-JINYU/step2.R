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

roc_pict <- function(pod, title = "") {
    # pod = pod_test ; path = './训练集roc曲线'
    rocobj <- roc(pod[, 2], pod[, 4], # cases=pod[,4][pod[,2]=='OR'],
        # levels=c('OR','HC'),controls=pod[,4][pod[,2]=='HC'],  # 可以设置实验组或对照组
        smooth = F
    ) # 曲线是否光滑，当光滑时，无法计算置信区间

    # 计算临界点/阈值
    cutOffPoint <- coords(rocobj, "best")
    cutOffPointText <- paste0(round(cutOffPoint[1, 1], 3), "\n(", round(cutOffPoint[1, 2], 3), ",", round(cutOffPoint[1, 3], 3), ")")

    # 计算AUC值
    auc <- auc(rocobj)[1]
    # AUC的置信区间
    auc_low <- ci(rocobj, of = "auc")[1]
    auc_high <- ci(rocobj, of = "auc")[3]

    # 计算置信区间
    ciobj <- ci.se(rocobj, specificities = seq(0, 1, 0.01))
    data_ci <- ciobj[1:101, 1:3]
    data_ci <- as.data.frame(data_ci)
    x <- as.numeric(rownames(data_ci))
    data_ci <- data.frame(x, data_ci)

    # 绘图
    p <- ggroc(rocobj, color = "black", size = 0.5, legacy.axes = F) + # FALSE时 横坐标为1-0 specificity；TRUE时 横坐标为0-1 1-specificity
        geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), # 绘制对角线
            colour = "grey", linetype = "longdash"
        ) +
        geom_ribbon(data = data_ci, aes(x = x, ymin = X2.5., ymax = X97.5.), fill = "lightblue", alpha = 0.5) + # 绘制置信区间,当legacy.axes=TRUE时， 把x=x改为x=1-x
        geom_point(aes(x = cutOffPoint[1, 2], y = cutOffPoint[1, 3]), color = "red") + # 绘制临界点/阈值
        # geom_text(aes(x = cutOffPoint[[2]],y = cutOffPoint[[3]],label=cutOffPointText,fontface = "plain"),size=4,vjust=-1)+# 添加临界点/阈值文字标
        ggtitle(title) + mytheme() + # theme_bw()+
        annotate("text",
            x = cutOffPoint[1, 2] * .9, y = cutOffPoint[1, 3] * .9,
            label = cutOffPointText, hjust = 0.5, vjust = 0, size = 4
        ) +
        annotate("text",
            x = 0.2, y = 0.1, hjust = 0.6, vjust = 0.2, size = 4,
            label = paste0("AUC: ", round(auc, 4), "\n", "95% CI: ", round(auc_low, 4), "-", round(auc_high, 4))
        )

    plot_bx <- pod[, c(2, 5)] %>% rename_all(~ c("group", "prob"))
    q <- ggplot(plot_bx) +
        stat_boxplot(aes(x = group, y = prob), geom = "errorbar", linetype = 1, width = 0.5) + # whiskers
        geom_boxplot(aes(x = group, y = prob, fill = group), show.legend = FALSE) +
        scale_fill_manual(values = sc$V2) +
        ggtitle(title) +
        mytheme()

    # ggsave(paste0(path,'.pdf'),p,width = 12.5,height = 12,units = 'in',dpi = 300)
    # ggsave(paste0(path,'.png'),p,width = 12,height = 12,units = 'in',dpi = 600 )
    return(list(p, q))
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
    po("removeconstants") # %>>% po("scale")  # 对LASSO很重要：标准化特征
task_prep <- pbp_prep$clone()$train(pbp_task)[[1]]
dim(task_prep$data())

# 数据划分
split_task <- partition(task = pbp_task, ratio = 0.9)
task_train <- pbp_task$clone()$filter(split_task$train)
task_test <- pbp_task$clone()$filter(split_task$test)

# 创建基准测试设计
learners <- list(
    # Elastic Net
    elasticnet = as_learner(pbp_prep %>>% lrn("classif.glmnet",
        predict_type = "prob",
        lambda = 1
    )),
    # XGBoost
    xgboost = as_learner(pbp_prep %>>% lrn("classif.xgboost",
        predict_type = "prob",
        nrounds = 100,
        eta = 0.1,
        objective = "binary:logistic"
    )),
    # LightGBM
    lightgbm = as_learner(pbp_prep %>>% lrn("classif.lightgbm",
        predict_type = "prob",
        num_iterations = 100,
        learning_rate = 0.1,
        objective = "binary"
    ))
)

# 为每个学习器设置搜索空间
search_spaces = list(
  # elastic net搜索空间
  elasticnet = ps(
    classif.glmnet.lambda = p_dbl(lower = 0.0001, upper = 1, logscale = TRUE), # lambda值，LASSO惩罚系数
    classif.glmnet.alpha = p_dbl(lower = 0, upper = 1), # 弹性网络混合参数：0=岭回归，0.5=弹性网络，1=LASSO
    classif.glmnet.standardize = p_lgl(), # 是否在模型内部标准化
    classif.glmnet.thresh = p_dbl(lower = 1e-7, upper = 1e-4, logscale = TRUE), # 收敛阈值
    classif.glmnet.maxit = p_int(lower = 1e3, upper = 1e4) # 最大迭代次数
  ),
  # XGBoost搜索空间
  xgboost = ps(
    classif.xgboost.eta = p_dbl(lower = 0.01, upper = 0.3, logscale = TRUE),
    classif.xgboost.max_depth = p_int(lower = 3, upper = 10),
    classif.xgboost.min_child_weight = p_dbl(lower = 1, upper = 10),
    classif.xgboost.subsample = p_dbl(lower = 0.5, upper = 1),
    classif.xgboost.colsample_bytree = p_dbl(lower = 0.5, upper = 1),
    classif.xgboost.nrounds = p_int(lower = 50, upper = 500)
  ),
  # LightGBM搜索空间
  lightgbm = ps(
    classif.lightgbm.learning_rate = p_dbl(lower = 0.01, upper = 0.3, logscale = TRUE),
    classif.lightgbm.max_depth = p_int(lower = 3, upper = 10),
    classif.lightgbm.min_data_in_leaf = p_int(lower = 10, upper = 100),
    classif.lightgbm.feature_fraction = p_dbl(lower = 0.5, upper = 1),
    classif.lightgbm.bagging_fraction = p_dbl(lower = 0.5, upper = 1),
    classif.lightgbm.num_iterations = p_int(lower = 50, upper = 500)
  )
)

# 定义终止条件
# 更优化的终止条件设置
terminator <- trm("combo",
    terminators = list(
        # 性能停滞终止
        trm("stagnation",
            iters = 10, # 观察最近10次迭代
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
tuner <- tnr("random_search", batch_size = 50)

# 定义重抽样方案
inner_resampling <- rsmp("cv", folds = 5)
outer_resampling <- rsmp("cv", folds = 5)

# 创建自动调优器列表
auto_tuners <- map2(learners, search_spaces, function(learner, search_space) {
    auto_tuner(
        tuner = tuner,
        learner = learner,
        resampling = inner_resampling,
        measure = msr("classif.auc"),
        search_space = search_space,
        terminator = terminator,
        store_tuning_instance = TRUE
    )
})

# 创建基准测试设计
design <- benchmark_grid(
    tasks = task_train,
    learners = auto_tuners,
    resamplings = outer_resampling
)

# 执行基准测试
bmr <- benchmark(design, store_models = TRUE)
# 聚合结果
measures <- msrs(c("classif.auc", "classif.acc", "classif.bacc", "time_train", "time_predict"))
tab <- bmr$aggregate(measures)
print(tab)

# 可视化比较结果
autoplot(bmr, measure = msr("classif.auc")) +
    theme_minimal() +
    labs(title = "模型AUC比较")

# 创建详细的性能比较图
performance_long <- bmr$score(measures) %>%
    as.data.table() %>%
    select(learner_id, contains("classif.")) %>%
    melt(
        id.vars = "learner_id",
        variable.name = "metric",
        value.name = "value"
    )

ggplot(performance_long, aes(x = learner_id, y = value, fill = metric)) +
    geom_boxplot() +
    theme_minimal() +
    labs(
        title = "模型性能比较",
        x = "模型",
        y = "性能指标值"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 提取特征重要性
get_feature_importance <- function(model) {
    if (inherits(model, "AutoTuner")) {
        model <- model$learner$model
    }

    if (inherits(model, "glmnet")) {
        coef_matrix <- coef(model)
        importance <- data.frame(
            Feature = rownames(coef_matrix)[-1],
            Importance = abs(as.vector(coef_matrix)[-1])
        )
    } else if (inherits(model, "xgb.Booster")) {
        importance <- xgboost::xgb.importance(model = model)
        importance <- data.frame(
            Feature = importance$Feature,
            Importance = importance$Gain
        )
    } else if (inherits(model, "lgb.Booster")) {
        importance <- lightgbm::lgb.importance(model)
        importance <- data.frame(
            Feature = importance$Feature,
            Importance = importance$Gain
        )
    } else {
        return(NULL)
    }
    return(importance)
}

# 获取所有模型的特征重要性
importance_list <- lapply(auto_tuners, function(at) {
    imp <- get_feature_importance(at$model)
    if (!is.null(imp)) {
        imp$Model <- at$id
    }
    return(imp)
})

# 合并所有特征重要性结果
all_importance <- do.call(rbind, importance_list)

# 绘制特征重要性对比图
ggplot(all_importance, aes(x = reorder(Feature, Importance), y = Importance, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_minimal() +
    labs(
        title = "特征重要性比较",
        x = "特征",
        y = "重要性得分"
    ) +
    theme(legend.position = "bottom")

# 在测试集上评估最终模型
test_predictions <- lapply(auto_tuners, function(at) {
    at$train(task_train)
    at$predict(task_test)
})

# 计算测试集性能
test_performance <- lapply(test_predictions, function(pred) {
    pred$score(measures)
})

test_performance_df <- do.call(rbind, test_performance)
print("测试集性能：")
print(test_performance_df)

# 保存结果
save(bmr, test_predictions, test_performance_df, all_importance,
    file = "benchmark_results.RData"
)

# 生成ROC曲线比较图
library(pROC)
roc_curves <- lapply(test_predictions, function(pred) {
    roc(pred$truth, pred$prob[, "1"])
})

# 绘制所有ROC曲线
colors <- c("red", "blue", "green")
plot(roc_curves[[1]], col = colors[1])
for (i in 2:length(roc_curves)) {
    plot(roc_curves[[i]], add = TRUE, col = colors[i])
}
legend("bottomright",
    legend = names(roc_curves),
    col = colors,
    lwd = 2
)
