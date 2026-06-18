#setwd("/Users/colinliu/Nutstore\ Files/我的坚果云/R/mlr3_docker_tutorial/data_for_analysis/20250211-mlr-JINYU/")
# RFE for Random Forest 示例 (需要安装 mlr3fselect 和 mlr3learners)
getOption("repos")
options(repos = structure(c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
options(BioC_mirror = "https://mirrors.tuna.tsinghua.edu.cn/bioconductor")
## install.packages("remotes")
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager") # ;BiocManager::install(version = "3.18")
if (!("mlr3extralearners" %in% installed.packages())) {
    remotes::install_github("mlr-org/mlr3extralearners@*release")
}
if (!("aplot" %in% installed.packages())) {
    remotes::install_github("YuLab-SMU/aplot")
}
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(
    optparse, tidyverse, openxlsx, mlr3verse, data.table, mltools, mlr3tuningspaces, future, readxl,
    treeshap, kernelshap, shapviz, mlr3extralearners, ranger, randomForest, pROC, patchwork, Boruta,
    showtext, xgboost, mlr3learners, mlr3tuning, paradox, future.apply, ggplot2, glmnet, e1071, nnet, 
    lightgbm
)

if (TRUE) {
    option_list <- list(
        make_option(c("-o", "--outdir"), type = "character", default = "", help = "输出文件夹默认为当前目录")
    )
    opts <- parse_args(OptionParser(option_list = option_list))
}

if (opts$outdir == "") {
    opts$outdir <- getwd()
}
# 设置并行计算
future::plan("multisession", workers = 6) # 使用6个并行工作进程
set.seed(123)
mlr3verse_info()

# 加载数据
load("./step1.RData")

# 1. function -----------------------------------------------
extract_roc_data_from_bmr <- function(bmr_object) { # bmr_object <- bmr
    # 获取所有resample结果
    resample_results <- bmr_object$resample_results

    # 创建一个空列表来存储每个模型的ROC数据
    roc_data_list <- list()

    # 遍历每个模型的结果
    for (i in 1:nrow(resample_results)) { # i <- 1
        # 获取当前模型的信息 - 正确访问resample_result列
        current_result <- resample_results$resample_result[[i]]

        # 获取模型名称
        model_name <- current_result$learner$id
        model_name <- str_replace(model_name, "classif\\.(.*?)\\.tuned", "\\1")

        # 获取预测结果
        predictions <- current_result$predictions()

        # 合并所有折叠的预测结果
        all_preds <- data.frame()

        for (fold in seq_along(predictions)) { # fold <- 2
            pred <- predictions[[fold]] # 获取当前折叠的预测
            truth_values <- pred$truth # 提取真实标签和预测概率
            pos_class <- current_result$task$positive # 获取正类名称
            prob_values <- pred$prob[, pos_class]
            # 安全计算truth整数值
            truth_int <- as.integer(truth_values == pos_class)

            # 创建一致长度的向量
            n_rows <- length(truth_int)
            dataset_vec <- rep("CV", n_rows)
            model_vec <- rep(model_name, n_rows)

            # 安全创建数据框
            fold_data <- data.frame(
                truth = truth_int,
                prob = prob_values,
                dataset = dataset_vec,
                model = model_vec,
                stringsAsFactors = FALSE
            )

            all_preds <- rbind(all_preds, fold_data)
        }

        # 将该模型的数据添加到列表中
        roc_data_list[[model_name]] <- all_preds
    }

    return(roc_data_list)
}

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

roc_pict <- function(pod_list, title = "", path = "./train_roc_plot",
                     colors = NULL, labels = NULL) {
  # 创建一个空的数据框来存储所有ROC数据
  all_roc_data <- c() # bind_rows(pod_list)
  auc_texts <- c()

  # 如果没有提供颜色，使用默认颜色方案
  if (is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(min(9, length(pod_list)), "Set1")
    if (length(pod_list) > 9) {
      colors <- colorRampPalette(colors)(length(pod_list))
    }
  }

  # 处理每个模型的数据
  for (i in seq_along(pod_list)) { # i <- 2
    pod <- pod_list[[i]]

    # 确保数据格式正确
    if (!all(c("d", "m") %in% names(pod))) {
      pod <- data.frame(
        d = pod[, 1], # 真实标签
        m = pod[, 2] # 预测概率
      )
    }

    # 计算ROC数据
    roc_obj <- roc(pod$d, pod$m, quiet = TRUE)
    auc_value <- round(auc(roc_obj), 3)
    ci_obj <- ci.auc(roc_obj)

    # 存储ROC数据
    roc_df <- data.frame(
      specificity = roc_obj$specificities,
      sensitivity = roc_obj$sensitivities,
      model = labels[i]
    )
    all_roc_data <- rbind(all_roc_data, roc_df)

    # 创建AUC文本
    auc_texts[i] <- sprintf(
      "%s: AUC = %.3f (%.3f-%.3f)",
      labels[i], auc_value,
      round(ci_obj[1], 3), round(ci_obj[3], 3)
    )
  }
  all_roc_data$model <- factor(all_roc_data$model, levels = names(pod_list))
  # 生成ROC曲线
  p <- ggplot(all_roc_data, aes(
    x = 1 - specificity, y = sensitivity,
    color = model
  )) +
    geom_path(size = 1) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", color = "grey40"
    ) +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")) +
    mytheme() +
    ggtitle(title) +
    labs(
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      color = "Models"
    ) +
    annotate("text",
      x = 0.55, y = 0.25 - 0.05 * seq_along(auc_texts),
      label = auc_texts, size = 4, hjust = 0
    ) +
    coord_equal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)
    )

  # 保存图形
  ggsave(paste0(path, ".pdf"), p,
    width = 10, height = 8, units = "in", dpi = 300
  )

  return(list(plot = p, data = all_roc_data))
}

# 创建一个函数来提取模型信息
get_model_info <- function(model_name, current_learner, task) {
    model_info <- list(
        model_name = model_name,
        features = task$feature_names, # 获取特征名称
        n_features = length(task$feature_names),
        hyperparameters = list(),
        performance = list(),
        feature_importance = NULL
    )

    # 获取超参数
    if (inherits(current_learner, "AutoTuner")) {
        model_info$hyperparameters <- current_learner$tuning_instance$result_learner_param_vals
        model_info$tuning_results <- current_learner$tuning_instance$archive$data
    }

    # 获取特征重要性
    if (model_name == "rf") {
        imp <- current_learner$model$learner$model$variable.importance
        if (!is.null(imp)) {
            model_info$feature_importance <- data.frame(
                Feature = names(imp),
                Importance = as.numeric(imp)
            )
        }
    } else if (model_name == "xgboost") {
        model <- current_learner$model$learner$model
        if (inherits(model, "xgb.Booster")) {
            imp <- xgb.importance(model = model)
            if (!is.null(imp)) {
                model_info$feature_importance <- data.frame(
                    Feature = imp$Feature,
                    Importance = imp$Gain
                )
            }
        }
    } else if (model_name == "glmnet") {
        coefs <- coef(current_learner$model$learner$model)
        if (!is.null(coefs)) {
            nonzero_idx <- which(coefs[-1] != 0)
            model_info$feature_importance <- data.frame(
                Feature = rownames(coefs)[-1][nonzero_idx],
                Importance = abs(coefs[-1][nonzero_idx])
            )
        }
    } else if (model_name == "lightgbm") {
        model <- current_learner$model$learner$model
        if (inherits(model, "lgb.Booster")) {
            imp <- lightgbm::lgb.importance(model)
            if (!is.null(imp)) {
                model_info$feature_importance <- data.frame(
                    Feature = imp$Feature,
                    Importance = imp$Gain
                )
            }
        }
    } else if (model_name == "svm") {
        model <- current_learner$model$learner$model
        sv_importance <- colMeans(abs(model$SV))
        if (!is.null(sv_importance)) {
            model_info$feature_importance <- data.frame(
                Feature = names(sv_importance),
                Importance = sv_importance
            )
        }
    }

    return(model_info)
}

roc_box_plot <- function(pod_list, title = "", label = NULL) {
  # 确保数据格式正确
  pod <- data.frame(
    d = pod_list[, 1], # 真实标签
    m = pod_list[, 2] # 预测概率
  )

  # 计算ROC数据
  roc_obj <- roc(pod$d, pod$m, quiet = TRUE)
  # 计算临界点/阈值
  cutOffPoint <- coords(roc_obj, "best")
  cutOffPointText <- paste0(round(cutOffPoint[1, 1], 3), "\n(", round(cutOffPoint[1, 2], 3), ",", round(cutOffPoint[1, 3], 3), ")")

  # 计算AUC值
  auc_value <- auc(roc_obj)[1]
  # AUC的置信区间 # ci_obj <- ci.auc(roc_obj)
  auc_low <- ci(roc_obj, of = "auc")[1]
  auc_high <- ci(roc_obj, of = "auc")[3]

  # 计算置信区间
  ciobj <- ci.se(roc_obj, specificities = seq(0, 1, 0.01))
  data_ci <- ciobj[1:101, 1:3]
  data_ci <- as.data.frame(data_ci)
  x <- as.numeric(rownames(data_ci))
  data_ci <- data.frame(x, data_ci)

  # 存储ROC数据
  roc_df <- data.frame(
    specificity = roc_obj$specificities,
    sensitivity = roc_obj$sensitivities
  )

  # 计算pvalue
  U <- wilcox.test(
    pod_list[pod_list$truth == 1, "prob"],
    pod_list[pod_list$truth == 0, "prob"]
  )
  # 生成ROC曲线
  p <- ggroc(roc_obj, color = "black", size = 0.5, legacy.axes = F) + # FALSE时 横坐标为1-0 specificity；TRUE时 横坐标为0-1 1-specificity
    geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), # 绘制对角线
      colour = "grey", linetype = "longdash"
    ) +
    geom_ribbon(data = data_ci, aes(x = x, ymin = X2.5., ymax = X97.5.), fill = "lightblue", alpha = 0.5) + # 绘制置信区间,当legacy.axes=TRUE时， 把x=x改为x=1-x
    geom_point(aes(x = cutOffPoint[1, 2], y = cutOffPoint[1, 3]), color = "red") + # 绘制临界点/阈值
    annotate("text",
      x = (cutOffPoint[1, 2]) * .9, y = cutOffPoint[1, 3] * .9,
      label = cutOffPointText, hjust = 0.5, vjust = 0, size = 3
    ) +
    annotate("text",
      x = 0.2, y = 0.1, hjust = 0.6, vjust = 0.2, size = 3,
      label = paste0(
        label, " AUC: ", round(auc_value, 4), "\n", "95% CI: ",
        round(auc_low, 4), "-", round(auc_high, 4),
        "\n", "pvalue: ", Minus(U$p.value, 4)
      )
    ) +
    mytheme() +
    ggtitle(title) +
    coord_equal() +
    theme(legend.position = "bottom", legend.box = "horizontal") # ,legend.margin = margin(t = 10, r = 0, b = 0, l = 0)

  mapcol <- c("#61d04f", "#df536b") # , "#377EB8", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF") # ,"#4DAF4A","#E41A1C" RColorBrewer::brewer.pal(n = 8,name = "Set1")
  q <- ggplot(pod_list %>% mutate(truth = factor(truth, levels = c("0", "1")))) +
    stat_boxplot(aes(x = truth, y = prob, group = truth), geom = "errorbar", linetype = 1, width = 0.5) + # whiskers
    geom_boxplot(aes(x = truth, y = prob, group = truth, fill = truth), show.legend = FALSE) +
    scale_fill_manual(values = mapcol) +
    scale_x_discrete(labels = c("0" = "Ctrl", "1" = "Case")) +
    ggtitle(title) +
    mytheme() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # 增加横轴文字旋转90度
  # coord_equal() +
  return(list(p, q, U))
}

# 太长的话加换行符，以第一个分隔符为界
Add_breaks <- function(x, sep = "_") { # x <- Plot_data4[[1]]$Description
    # 查找第一个出现的分隔符及其位置
    sep_positions <- str_locate(x, sep)[1, 1] # 将字符串拆分为单个字母
    letters <- strsplit(x, "")[[1]]
    # 将字母用换行符连接
    paste0(
        paste0(letters[1:sep_positions - 1], collapse = ""),
        "\n",
        paste0(letters[(sep_positions + 1):length(letters)], collapse = "")
    )
}

# importance plot
plot_p_feat <- function(var_imp, out = opts$outdir, plot = T) { # var_imp <- Plot_data4[[1]]
    p_feat <- ggplot(var_imp, aes(x = Description_fold, y = Importance, fill = Importance)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        ylab("Variable Importance") +
        xlab("") + # ggtitle("Information Value Summary") +
        guides(fill = "none") +
        scale_fill_gradient(low = "#327eba", high = "#e06663") +
        theme_bw() +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )
    if (isTRUE(plot)) {
        ggsave(file.path(out, "00.MeanDecreaseAccuracy.pdf"), p_feat, width = 10, height = 6)
    }
    return(p_feat)
}

ComBn_plot <- function(P, w, out_parm = opts$outdir, biomaker_num_fix_parm = "") {
  # P <- roc_plot
  if (length(P) == 1) {
    layout <- "ABC"
  } else if (length(P) == 3) {
    layout <- "
  ABGG
  CDGG
  EFGG"
  } else if (length(P) == 2) {
    layout <- "
  ABGG
  CDGG"
  }

  Q <- P[[1]][[1]] + P[[1]][[2]]
  if (length(P) > 1) {
    for (i in 2:length(P)) {
      Q <- Q + P[[i]][[1]] + P[[i]][[2]]
    }
  }
  Q <- Q + w + plot_layout(design = layout, guides = "collect") +
    plot_annotation(tag_levels = c("A", "1")) #+ # ncol = 2, byrow = TRUE
  # gridExtra::tableGrob(t(as.data.frame(parm)))
  ggsave(file.path(out_parm, paste0("step3.", biomaker_num_fix_parm, ".patchwork.pdf")), Q,
    dpi = 300, device = cairo_pdf,
    width = 14,
    height = 4 * length(P)
  )
}

Minus <- function(x, n) {
    d1 <- 10^(-n)
    ifelse(x >= d1, round(x, 4), paste0("< ", d1))
}

# 2. 数据准备 -----------------------------------------------
data <- data14 |>
  mutate(!!sym(feature_names$group) := as.factor(!!sym(feature_names$group))) |>
  filter(feature14_0 == 1) |> dplyr::select(!starts_with("feature_14_"))

# 创建分类任务
pbp_task <- TaskClassif$new(
  id = "jinyu_model",
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

# 定义要排除的特征
tNGS_pattern <- paste0("^", feature_names$tNGS)
excluded_feature_names <- c("feature58", "feature27", "feature26")

# 创建排除选择器
exclude_selector <- selector_union(
  selector_grep(tNGS_pattern),
  selector_name(excluded_feature_names)
)

# 使用排除选择器
pbp_prep <- po("imputehist", affect_columns = selector_name(continuous_cols)) %>>%
  po("imputemode", affect_columns = selector_name(categorical_cols)) %>>%
  po("removeconstants") %>>%
  po("select", selector = selector_invert(exclude_selector))
task_prep <- pbp_prep$clone()$train(pbp_task)[[1]]
dim(task_prep$data())

# 数据划分 - 使用预处理后的任务
split_task <- partition(task = task_prep, ratio = 0.9)
task_train <- task_prep$clone()$filter(split_task$train)
task_test <- task_prep$clone()$filter(split_task$test)

# 定义重抽样方案
inner_resampling <- rsmp("cv", folds = 5)
outer_resampling <- rsmp("cv", folds = 5)

rfe <- fs("rfe", n_features = 20, feature_fraction = 0.85)

max_nrounds <- 500

learners <- list(
  lrn("classif.xgboost",
    id = "xgb", nrounds = max_nrounds,
    early_stopping_rounds = 20, validate = "test"
  ),
  lrn("classif.ranger", id = "rf", importance = "permutation"),
  lrn("classif.svm", id = "svm", type = "C-classification", kernel = "linear"),
  lrn("classif.lightgbm",
    id = "lgbm",
    num_iterations = max_nrounds,
    learning_rate = 0.05,
    num_leaves = 31,
    objective = "binary",
    min_data_in_leaf = 20,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq = 5
  )
)

svm_rfe <- clbk("mlr3fselect.svm_rfe")
internal_ss <- ps(
  nrounds = p_int(upper = max_nrounds, aggr = function(x) as.integer(mean(unlist(x))))
)
xgb_clbk <- clbk("mlr3fselect.internal_tuning", internal_search_space = internal_ss)
one_se_clbk <- clbk("mlr3fselect.one_se_rule")

callbacks <- list(
    xgb  = list(one_se_clbk, xgb_clbk),
    rf   = list(one_se_clbk),
    svm  = list(one_se_clbk, svm_rfe),
    lgbm = list(one_se_clbk)
)

efs <- ensemble_fselect(
  fselector = rfe,
  task = task_train,
  learners = learners,
  init_resampling = rsmp("subsampling", repeats = 100, ratio = 0.8),
  inner_resampling = inner_resampling,
  inner_measure = msr("classif.ce"),
  measure = msr("classif.acc"),
  terminator = trm("none"),
  callbacks = callbacks,
  store_benchmark_result = TRUE
)

print(efs)
efs$result
efs$active_measure
efs$measure
# 我们可以查看在集合特征选择过程中使用的不同学习器的性能得分。每个方框表示特定学习器在不同重采样迭代中的得分分布。
p1 <- autoplot(efs, type = "performance", theme = theme_minimal(base_size = 14)) +
  scale_fill_brewer(palette = "Set1")
ggsave(file.path(opts$outdir, "step2.2.1.efs.performance.pdf"), p1, width = 12.5, height = 12, units = "in", dpi = 300)

# 可以绘制出每个学习器在不同的重采样迭代中选择的特征数量：
p2 <- autoplot(efs, type = "n_features", theme = theme_minimal(base_size = 14)) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(0, 60, 10))
ggsave(file.path(opts$outdir, "step2.2.2.efs.n_features.pdf"), p2, width = 12.5, height = 12, units = "in", dpi = 300)

# 帕累托前沿，代表特征数量和性能之间权衡的点集
p3 <- autoplot(efs, type = "pareto", theme = theme_minimal(base_size = 14)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Empirical Pareto front")
ggsave(file.path(opts$outdir, "step2.2.3.efs.pareto.pdf"), p3, width = 12.5, height = 12, units = "in", dpi = 300)
# 拟合一个线性模型，以经验帕累托前沿的所选特征数量（ 1/x ）的倒数作为输入，以相关的性能得分作为输出，绘制出一条估计的帕累托前沿曲线
p4 <- autoplot(efs,
  type = "pareto", pareto_front = "estimated",
  theme = theme_minimal(base_size = 14)
) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Estimated Pareto front")
ggsave(file.path(opts$outdir, "step2.2.4.efs.pareto_estimated.pdf"), p4, width = 12.5, height = 12, units = "in", dpi = 300)

p5 <- autoplot(efs, type = "stability", theme = theme_minimal(base_size = 14)) +
  scale_fill_brewer(palette = "Set1")
ggsave(file.path(opts$outdir, "step2.2.5.efs.stability.pdf"), p5, width = 12.5, height = 12, units = "in", dpi = 300)

# stability
efs$stability(stability_measure = "jaccard", global = TRUE)
efs$stability(stability_measure = "jaccard", global = FALSE)

# 默认使用基于集合的法线边界交点法（Normal Boundary Intersection），
# 计算每个点与帕累托前沿第一个点（性能最差特征最少）和最后一个点（性能最好特征最多）连线的垂直距离，找到knee point
efs$knee_points(type = "estimated")
efs$knee_points(type = "empirical")

# EFS-based Feature Selection
n_features <- efs$knee_points()$n_features
res <- efs$feature_ranking(method = "sav", committee_size = n_features)
# res根据score绘制翻转xy轴的条形图
p6 <- ggplot(res, aes(x = reorder(feature, score), y = score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  mytheme() +
  labs(x = "feature", y = "score", title = "feature importance score")
ggsave(file.path(opts$outdir, "step2.2.6.efs.feature_importance.pdf"), p6, width = 12.5, height = 12, units = "in", dpi = 300)

# res$feature包含了EFS选择的特征子集
selected_features <- res$feature

# 3. 自动调优器 -----------------------------------------------
## 3.1. 创建新的训练和测试任务
task_train_selected <- task_train$clone()$select(selected_features)
task_test_selected <- task_test$clone()$select(selected_features)

## 3.2. 创建自动调优器
model_params <- list(
  # 原有模型参数保持不变
  glmnet = ps(
    lambda = p_dbl(lower = 1e-4, upper = 1, logscale = TRUE),
    alpha = p_dbl(lower = 0, upper = 1),
    standardize = p_lgl(),
    thresh = p_dbl(lower = 1e-7, upper = 1e-4, logscale = TRUE),
    maxit = p_int(lower = 1e3, upper = 1e4)
  ),
  xgboost = ps(
    eta = p_dbl(0.01, 0.3),
    max_depth = p_int(1, 10),
    nrounds = p_int(50, 500),
    min_child_weight = p_dbl(1, 10),
    subsample = p_dbl(0.5, 1),
    colsample_bytree = p_dbl(0.5, 1)
  ),
  rf = ps(
    num.trees = p_int(lower = 500, upper = 1000),
    max.depth = p_int(lower = 3, upper = 15),
    min.node.size = p_int(lower = 1, upper = 10),
    mtry = p_int(1, 20)
  ),
  svm = ps(
    cost = p_dbl(lower = -2, upper = 2, trafo = function(x) 10^x),
    kernel = p_fct(c("polynomial", "radial")),
    degree = p_int(1, 3, depends = kernel == "polynomial")
  ),
  lightgbm = ps(
    learning_rate = p_dbl(0.01, 0.3),
    num_iterations = p_int(50, 500),
    max_depth = p_int(3, 12),
    min_data_in_leaf = p_int(10, 100),
    feature_fraction = p_dbl(0.5, 1.0),
    bagging_fraction = p_dbl(0.5, 1.0),
    bagging_freq = p_int(0, 10),
    min_gain_to_split = p_dbl(0.0, 0.5),
    num_leaves = p_int(10, 255)
  )
)

## 3.3. 创建LightGBM基础学习器
learner_base_glmnet <- lrn("classif.glmnet",
  lambda = 0.1, alpha = 0.5,
  predict_type = "prob"
)
learner_base_xgb <- lrn("classif.xgboost",
  predict_type = "prob",
  nrounds = 100, eta = 0.1, max_depth = 6
)
learner_base_rf <- lrn("classif.ranger",
  predict_type = "prob",
  num.trees = 500, importance = "impurity"
)
learner_base_svm <- lrn("classif.svm",
  predict_type = "prob",
  type = "C-classification", # 必须指定分类类型
  kernel = "radial", cost = 1
)
learner_base_lightgbm <- lrn("classif.lightgbm")

terminator <- trm("combo",
  terminators = list( # 性能停滞终止
    trm("stagnation", iters = 10, threshold = 0.005), # 观察最近15次
    trm("evals", n_evals = 100), # 评估次数终止
    trm("run_time", secs = 3600), # 1小时时间限制
    trm("perf_reached", level = 0.85) # 目标性能终止
  ) # 达到85%准确率就停止
)
tuner <- tnr("random_search", batch_size = 100)
## 3.4. 自动调优器列表
auto_tuners <- list(
  glmnet = auto_tuner(
    tuner = tuner,
    learner = learner_base_glmnet,
    resampling = inner_resampling,
    measure = msr("classif.auc"),
    search_space = model_params$glmnet,
    terminator = terminator,
    store_tuning_instance = TRUE
  ),
  xgboost = auto_tuner(
    tuner = tuner,
    learner = learner_base_xgb,
    resampling = inner_resampling,
    measure = msr("classif.auc"),
    search_space = model_params$xgboost,
    terminator = terminator,
    store_tuning_instance = TRUE
  ),
  rf = auto_tuner(
    tuner = tuner,
    learner = learner_base_rf,
    resampling = inner_resampling,
    measure = msr("classif.auc"),
    search_space = model_params$rf,
    terminator = terminator,
    store_tuning_instance = TRUE
  ),
  svm = auto_tuner(
    tuner = tuner,
    learner = learner_base_svm,
    resampling = inner_resampling,
    measure = msr("classif.auc"),
    search_space = model_params$svm,
    terminator = terminator,
    store_tuning_instance = TRUE
  ),
  # 新增LightGBM自动调优器
  lightgbm = auto_tuner(
    tuner = tuner,
    learner = learner_base_lightgbm,
    resampling = inner_resampling,
    measure = msr("classif.auc"),
    search_space = model_params$lightgbm,
    terminator = terminator,
    store_tuning_instance = TRUE
  )
)

## 3.5. 创建benchmark网格（包含LightGBM）
benchmark_grid <- benchmark_grid(
  tasks = task_train_selected,
  learners = auto_tuners,
  resamplings = outer_resampling
)

## 3.6. 执行基准测试
bmr <- benchmark(benchmark_grid, store_models = TRUE)

## 3.7. 可视化比较
p7 <- autoplot(bmr, measure = msr("classif.auc")) +
  mytheme() +
  labs(title = "model comparison - AUC")
ggsave(file.path(opts$outdir, "step2.2.7.model_comparison.auc.pdf"), p7, width = 12.5, height = 12, units = "in", dpi = 300)

p8 <- autoplot(bmr, type = "roc")
ggsave(file.path(opts$outdir, "step2.2.8.model_comparison.roc.pdf"), p8, width = 12.5, height = 12, units = "in", dpi = 300)

# 4. 分析结果 -----------------------------------------------
performance_results <- bmr$aggregate(msrs(c("classif.auc", "classif.acc", "classif.ce")))
# 使用model_mapping处理cv_results
cv_results <- performance_results[, c("learner_id", "classif.auc", "classif.acc", "classif.ce")] %>%
    mutate(learner_id = case_when(
        learner_id == "classif.ranger.tuned" ~ "rf",
        TRUE ~ str_replace(learner_id, "classif\\.(.*?)\\.tuned", "\\1")
    )) %>%
    rename(Model = learner_id, CV_AUC = classif.auc, CV_ACC = classif.acc, CV_CE = classif.ce)

cv_roc_all <- extract_roc_data_from_bmr(bmr)

# 9. 选择最佳模型并在测试集上评估
best_learner_id <- performance_results[which.max(performance_results$classif.auc), "learner_id"]

# 创建模型名称映射
model_mapping <- c(
    "classif.ranger.tuned" = "rf",
    "classif.xgboost.tuned" = "xgboost",
    "classif.svm.tuned" = "svm",
    "classif.lightgbm.tuned" = "lightgbm",
    "classif.glmnet.tuned" = "glmnet"
)
# 创建颜色映射
model_colors <- c(
  "glmnet"  = "#E41A1C", # 红色
  "xgboost" = "#377EB8", # 蓝色
  "ranger"  = "#4DAF4A", # 绿色
  "svm"     = "#984EA3", # 紫色
  "lightgbm"  = "#FF7F00"  # 橙色
)

# 获取对应的模型名称
best_learner_name <- model_mapping[best_learner_id$learner_id]
best_learner <- auto_tuners[[best_learner_name]]
print(paste("选择的最佳模型:", best_learner_name))
# 将benchmark结果转换为数据表
results_dt <- as.data.table(bmr)

# 创建一个列表来存储所有模型的结果
all_models_results <- list()
for (i in 1:nrow(results_dt)) { # i<- 1
  task_id <- results_dt$iteration[i]
  learner_id <- results_dt$learner[[1]]$learner$id

  # 为每个模型创建一个结果容器
  current_model <- list(
    task_id = task_id,
    learner_id = learner_id,
    model_type = "standard",
    parameters = list(),
    performance = list()
  )

  # 获取学习器对象
  learner <- results_dt$learner[[i]]

  # 检查是否为AutoTuner对象
  if (inherits(learner, "AutoTuner")) {
    current_model$model_type <- "autotuner"

    if (!is.null(learner$tuning_instance)) {
      current_model$best_params <- learner$tuning_instance$result_learner_param_vals
      current_model$tuning_performance <- learner$tuning_instance$result_y
      current_model$tuning_history <- learner$tuning_instance$archive$data
    }

    base_learner <- learner$learner
    current_model$base_learner_class <- class(base_learner)

    if (inherits(base_learner, "GraphLearner")) {
      current_model$pipeline_components <- base_learner$graph$ids()
      current_model$component_params <- list()

      for (node_id in base_learner$graph$ids()) {
        node <- base_learner$graph$pipeops[[node_id]]
        current_model$component_params[[node_id]] <- node$param_set$values
      }
    }
  } else {
    current_model$parameters <- learner$param_set$values
  }

  # 存储当前模型的结果
  all_models_results[[paste(task_id, learner_id, sep = "_")]] <- current_model
}

# 对每个模型进行训练和测试
model_results <- list()
for (learner_name in names(auto_tuners)) {
  cat("\n处理模型:", learner_name, "\n")
  current_learner <- auto_tuners[[learner_name]]

  # 训练模型 - 修正为使用选定特征的任务
  current_learner$train(task_train_selected)

  # 在训练集上预测 - 也使用选定特征的任务
  train_pred <- current_learner$predict(task_train_selected)
  train_performance <- train_pred$score(msrs(c("classif.auc", "classif.acc", "classif.ce")))

  # 在测试集上预测 - 使用选定特征的测试任务
  test_pred <- current_learner$predict(task_test_selected)
  test_performance <- test_pred$score(msrs(c("classif.auc", "classif.acc", "classif.ce")))

  # 准备ROC数据
  train_roc_data <- data.frame(
    truth = as.integer(train_pred$truth == task_train_selected$positive),
    prob = train_pred$prob[, task_train_selected$positive],
    dataset = "Training"
  )

  test_roc_data <- data.frame(
    truth = as.integer(test_pred$truth == task_train_selected$positive),
    prob = test_pred$prob[, task_train_selected$positive],
    dataset = "Testing"
  )

  # 获取特征重要性（如果可用）
  feature_importance <- NULL
  if (learner_name == "rf") {
    imp <- current_learner$model$learner$model$variable.importance
    if (!is.null(imp)) {
      feature_importance <- data.frame(
        Feature = names(imp),
        Importance = as.numeric(imp)
      )
    }
  } else if (learner_name == "xgboost") {
    model <- current_learner$model$learner$model
    if (inherits(model, "xgb.Booster")) {
      imp <- xgb.importance(model = model)
      if (!is.null(imp)) {
        feature_importance <- data.frame(
          Feature = imp$Feature,
          Importance = imp$Gain
        )
      }
    }
  } else if (learner_name == "glmnet") {
    coefs <- coef(current_learner$model$learner$model)
    if (!is.null(coefs)) {
      nonzero_idx <- which(coefs[-1] != 0)
      feature_importance <- data.frame(
        Feature = rownames(coefs)[-1][nonzero_idx],
        Importance = abs(coefs[-1][nonzero_idx])
      )
    }
  } else if (learner_name == "lightgbm") {
    # 添加LightGBM特征重要性获取
    model <- current_learner$model$learner$model
    if (inherits(model, "lgb.Booster")) {
      # 获取特征重要性（默认使用'split'作为重要性度量）
      imp <- lightgbm::lgb.importance(model)
      if (!is.null(imp)) {
        feature_importance <- data.frame(
          Feature = imp$Feature,
          Importance = imp$Gain  # 使用Gain作为重要性度量
        )
      }
    }
  }

  # 如果有特征重要性，绘制特征重要性图
  if (!is.null(feature_importance) && nrow(feature_importance) > 0) {
    # 按重要性排序
    feature_importance <- feature_importance[order(-feature_importance$Importance), ]
    # 选择top 20特征
    top_n <- min(20, nrow(feature_importance))
    top_features <- feature_importance[order(-feature_importance$Importance), ][1:top_n, ]

    p_importance <- ggplot(
      top_features,
      aes(x = reorder(Feature, Importance), y = Importance)
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      mytheme() +
      labs(
        x = "特征",
        y = "重要性分数",
        title = paste(learner_name, "模型 Top", top_n, "重要特征")
      )
    showtext_auto() # 自动启用中文支持
    ggsave(file.path(opts$outdir, paste0(learner_name, "_feature_importance.pdf")),
      p_importance,
      width = 10, height = 8
    )
  }

  # 存储结果
  model_results[[learner_name]] <- list(
    model = current_learner,
    train_performance = train_performance,
    test_performance = test_performance,
    feature_importance = feature_importance,
    train_roc_data = train_roc_data,
    test_roc_data = test_roc_data
  )
}

save.image(file.path(opts$outdir, "test2.2.1.efs.clinic.RData"))

# 5. 从原始数据中获取特征描述 -----------------------------------------------
feature_descriptions <- list()

## 基础特征映射
feature_descriptions <- c(
    feature04 = "样本ID",
    feature09 = "性别",
    feature10 = "年龄",
    feature13 = "分组",
    feature14 = "组织",
    feature17 = "tNGS",
    feature24 = "临床表现"
)

## 从select.list.tsv中读取完整的特征描述
feature_info <- read_tsv("select.list.tsv") %>%
    filter(YN == "Yes") %>%
    select(feature_name, chinese_name) %>%
    # 将特征名和中文描述合并，用下划线连接
    mutate(combined_name = paste(feature_name, chinese_name, sep = "_")) %>%
    # 使用合并后的名称作为值，原特征名作为名称
    select(feature_name, combined_name) %>%
    deframe() # 转换为命名向量

## 直接使用原始名称作为描述
# tNGS列的描述 - 直接使用old_name作为描述
tNGS_descriptions <- setNames(
    tNGS_colnames$old_name,
    tNGS_colnames$new_name
)

## tissue列的描述 - 使用原始列名
tissue_descriptions <- data_tissue %>%
    select(-SampleID) %>%
    colnames() %>%
    setNames(., .)

## clinic列的描述 - 使用原始列名
clinic_descriptions <- setNames(
    clinic_colnames$old_name,
    clinic_colnames$new_name
)

## 合并所有描述
feature_descriptions <- c(
    feature_info,
    tissue_descriptions,
    tNGS_descriptions,
    clinic_descriptions
)

# 6. 性能比较-----------------------------------------------
performance_comparison <- data.frame(
  Model = character(),
  Train_AUC = numeric(),
  Train_ACC = numeric(),
  Train_CE = numeric(),
  Test_AUC = numeric(),
  Test_ACC = numeric(),
  Test_CE = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in names(model_results)) {
  performance_comparison <- rbind(
    performance_comparison,
    data.frame(
      Model = model_name,
      Train_AUC = model_results[[model_name]]$train_performance["classif.auc"],
      Train_ACC = model_results[[model_name]]$train_performance["classif.acc"],
      Train_CE = model_results[[model_name]]$train_performance["classif.ce"],
      Test_AUC = model_results[[model_name]]$test_performance["classif.auc"],
      Test_ACC = model_results[[model_name]]$test_performance["classif.acc"],
      Test_CE = model_results[[model_name]]$test_performance["classif.ce"]
    )
  )
}
performance_comparison <- left_join(cv_results, performance_comparison)
print(performance_comparison)
# write_tsv(performance_comparison, "model_performance_comparison.xls")

train_roc_all <- list()
test_roc_all <- list()
for (model_name in names(model_results)) {
  train_roc_all[[model_name]] <- model_results[[model_name]]$train_roc_data %>% mutate(model = model_name)
  test_roc_all[[model_name]] <- model_results[[model_name]]$test_roc_data %>% mutate(model = model_name)
}

train_roc_plot <- roc_pict(train_roc_all,
  title = "训练集ROC曲线", path = file.path(opts$outdir, "train_roc_plot"),
  colors = model_colors, labels = names(train_roc_all)
)

test_roc_plot <- roc_pict(test_roc_all,
  title = "测试集ROC曲线", path = file.path(opts$outdir, "test_roc_plot"),
  colors = model_colors, labels = names(test_roc_all)
)

cv_roc_plot <- roc_pict(cv_roc_all,
  title = "交叉验证ROC曲线", path = file.path(opts$outdir, "cv_roc_plot"),
  colors = model_colors, labels = names(cv_roc_all)
)

# 对每个模型收集信息
model_details <- list()
for (model_name in names(model_results)) {
  model_details[[model_name]] <- get_model_info(
    model_name,
    model_results[[model_name]]$model,
    task_train_selected
  )
}

### 画图 -----------------------------------------------
FS <- as.data.frame(feature_descriptions) %>% 
    rownames_to_column() %>% 
    rename_all(~c("Feature","Description"))

### ROC曲线数据
names(train_roc_all)
names(test_roc_all)
names(cv_roc_all)
names(cv_roc_all) <- gsub("ranger", "rf", names(train_roc_all))
nn <- names(test_roc_all)
### 特征重要性数据
Plot_data4 <- lapply(model_details, function(x) # x <- model_details[[1]]
    left_join(x$feature_importance, FS, by = c("Feature" = "Feature")) %>%
        arrange(Importance) %>% 
        mutate(Description_fold = map_chr(Description, Add_breaks)) %>%
        mutate(Description_fold = fct_inorder(Description_fold))
)

for (i in 1:length(nn)) { # i <- 1
  #    p1 <- roc_box_plot(train_roc_all[[nn[i]]], title = "train", label = nn[i])
  p2 <- roc_box_plot(test_roc_all[[nn[i]]], title = "test", label = nn[i])
  #    p3 <- roc_box_plot(cv_roc_all[[nn[i]]], title = "cv", label = nn[i])
  p4 <- plot_p_feat(Plot_data4[[nn[i]]], out = opts$outdir, plot = F)

  # bind_rows(list(
  #    p1[[3]] %>% broom::tidy() %>% mutate(data = "train"),
  #    p2[[3]] %>% broom::tidy() %>% mutate(data = "test"),
  #    p3[[3]] %>% broom::tidy() %>% mutate(data = "cv")
  # )) %>%
  p2[[3]] %>%
    broom::tidy() %>%
    mutate(data = "test") %>%
    write_tsv(file.path(opts$outdir, paste0("step3.", nn[i], "_pod_wilcox_test.tsv")))

  ComBn_plot(list(p2[1:2]), # , p1[1:2], p3[1:2]
    w = p4, out_parm = opts$outdir, biomaker_num_fix_parm = nn[i]
  )
}

# 6. 生成模型分析结果-----------------------------------------------

# 创建新的Excel工作簿
wb <- createWorkbook()

# 对每个模型创建详细信息sheet
for (model_name in names(model_details)) {# model_name <- names(model_details)[1]
    info <- model_details[[model_name]]
    
    # 创建模型信息sheet
    addWorksheet(wb, model_name)
    
    # 1. 添加模型性能信息 - 增加CE指标
    performance_data <- performance_comparison %>% 
    filter(Model == model_name) %>%
    pivot_longer(
        cols = starts_with(c("CV_", "Train_", "Test_")), # 选择以 "CV_", "Train_", "Test_" 开头的列
        names_to = "Metric_Split", # 新列名，存储原始列名
        values_to = "Value" # 新列名，存储性能值
    ) %>%
    separate(
        col = "Metric_Split", # 要拆分的列
        into = c("Data_Split", "Metric"), # 拆分后的新列名
        sep = "_" # 分隔符
    ) %>%
     pivot_wider(
         names_from = "Data_Split", # 使用 Data_Split 列的值作为新列名
         values_from = "Value" # 使用 Value 列的值填充新列
    ) %>% select(-Model)
    
    writeData(wb, model_name, "模型性能", startRow = 1, startCol = 1)
    writeData(wb, model_name, performance_data, startRow = 2, startCol = 1)
    
    # 添加足够的空行分隔不同部分
    # 2. 添加超参数信息 - 调整起始行，确保与性能指标有足够的间隔
    writeData(wb, model_name, "最优超参数", startRow = 7, startCol = 1)
    hyperparams_df <- data.frame(
        Parameter = names(info$hyperparameters),
        Value = as.character(info$hyperparameters)
    )
    writeData(wb, model_name, hyperparams_df, startRow = 8, startCol = 1)
    
    # 3. 添加特征重要性信息（如果有）- 也调整起始行
    if (!is.null(info$feature_importance)) {
        writeData(wb, model_name, "特征重要性", startRow = 10 + nrow(hyperparams_df), startCol = 1)
        
        feature_importance <- info$feature_importance
        # 按重要性排序
        feature_importance <- feature_importance[order(-feature_importance$Importance), ]
        
        # 创建一个新的列来存储特征名称原始名称
        feature_importance$Original_Name <- feature_importance$Feature
        
        # 获取特征描述
        for(i in 1:nrow(feature_importance)) {
            feature_name <- feature_importance$Feature[i]
            # 尝试从feature_descriptions中获取描述
            if(feature_name %in% names(feature_descriptions)) {
                feature_importance$Description[i] <- feature_descriptions[[feature_name]]
            } else {
                # 如果没有描述，保留原始名称作为描述
                feature_importance$Description[i] <- feature_name
            }
        }
        
        # 重新排列列顺序
        feature_importance <- feature_importance[, c("Original_Name", "Description", "Importance")]
        colnames(feature_importance) <- c("原始特征名", "特征描述", "重要性得分")
        
        writeData(wb, model_name, feature_importance, 
                 startRow = 11 + nrow(hyperparams_df), startCol = 1)
    }
    
    # 设置列宽
    setColWidths(wb, model_name, cols = 1:3, widths = "auto")
    
    # 添加样式，加粗标题
    hs1 <- createStyle(textDecoration = "bold", fontSize = 12)
    addStyle(wb, model_name, hs1, rows = c(1, 7, 10 + nrow(hyperparams_df)), cols = 1)
    
    # 添加边框，更清晰地分隔不同部分
    borderStyle <- createStyle(border = "TopBottom", borderColour = "#4F81BD", borderStyle = "medium")
    addStyle(wb, model_name, borderStyle, rows = c(6, 9 + nrow(hyperparams_df)), cols = 1:3, gridExpand = TRUE)
}

# 添加总体比较sheet
addWorksheet(wb, "模型比较")

writeData(wb, "模型比较", performance_comparison, startRow = 1)
setColWidths(wb, "模型比较", cols = 1:7, widths = "auto")

# 美化模型比较表
headerStyle <- createStyle(fgFill = "#4F81BD", halign = "center", textDecoration = "bold", 
                        fontColour = "white", fontSize = 12)
addStyle(wb, "模型比较", headerStyle, rows = 1, cols = 1:7, gridExpand = TRUE)

# 添加表格边框
tableStyle <- createStyle(border = "TopBottomLeftRight", borderColour = "#4F81BD", 
                        borderStyle = "thin")
addStyle(wb, "模型比较", tableStyle, rows = 1:(nrow(performance_comparison)+1), 
        cols = 1:7, gridExpand = TRUE)

# 为第二列到第十列设置红绿色阶
for (col in 2:min(10, ncol(performance_comparison))) {
  conditionalFormatting(wb, "模型比较",
    cols = col,
    rows = 2:(nrow(performance_comparison) + 1),
    type = "colorScale",
    style = c("#4F81BD", "#FFFFFF", "#E41A1C") # 红-白-蓝色阶
  )
}
# 保存Excel文件
saveWorkbook(wb, file.path(opts$outdir, "model_performance_comparison.xlsx"), overwrite = TRUE)

# 打印确认信息
print("已生成model_performance_comparison.xlsx文件，包含以下sheet:")
print(paste("- 模型sheet:", paste(names(model_details), collapse = ", ")))
print("- 模型比较sheet")
