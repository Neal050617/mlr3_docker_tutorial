model_params <- list(
    # Elastic Net参数 - 增加正则化强度
    glmnet = ps(
        # 增大lambda范围的下限，加强正则化
        classif.glmnet.lambda = p_dbl(lower = 0.01, upper = 1, logscale = TRUE),
        # 倾向于使用更多的L2正则化
        classif.glmnet.alpha = p_dbl(lower = 0, upper = 0.5), # 降低L1比例
        classif.glmnet.standardize = p_lgl(),
        classif.glmnet.thresh = p_dbl(lower = 1e-7, upper = 1e-4, logscale = TRUE),
        classif.glmnet.maxit = p_int(lower = 1e3, upper = 1e4),
        # 增加特征筛选强度
        boruta.filter.frac = p_dbl(lower = 0.3, upper = 0.7) # 减少保留特征数量
    ),

    # XGBoost参数 - 限制模型复杂度
    xgboost = ps(
        # 降低学习率
        classif.xgboost.eta = p_dbl(0.01, 0.1),
        # 限制树的深度
        classif.xgboost.max_depth = p_int(1, 6),
        classif.xgboost.nrounds = p_int(50, 300),
        # 增加最小子节点权重
        classif.xgboost.min_child_weight = p_dbl(3, 10),
        # 增加随机性
        classif.xgboost.subsample = p_dbl(0.5, 0.8),
        classif.xgboost.colsample_bytree = p_dbl(0.5, 0.8),
        # 添加正则化参数
        classif.xgboost.lambda = p_dbl(1, 10), # L2正则化
        classif.xgboost.alpha = p_dbl(0, 5), # L1正则化
        boruta.filter.frac = p_dbl(lower = 0.3, upper = 0.7)
    ),

    # 随机森林参数 - 增加随机性
    rf = ps(
        # 增加树的数量
        classif.ranger.num.trees = p_int(lower = 1000, upper = 2000),
        # 限制树的深度
        classif.ranger.max.depth = p_int(lower = 3, upper = 8),
        # 增加最小节点样本数
        classif.ranger.min.node.size = p_int(lower = 5, upper = 20),
        # 减少每棵树的特征数量
        classif.ranger.mtry = p_int(1, 10),
        # 启用样本外估计
        classif.ranger.oob.error = p_lgl(default = TRUE),
        boruta.filter.frac = p_dbl(lower = 0.3, upper = 0.7)
    ),

    # SVM参数 - 简化模型
    svm = ps(
        # 增加正则化（减小cost）
        classif.svm.cost = p_dbl(lower = -3, upper = 1, trafo = function(x) 10^x),
        # 优先使用简单的核函数
        classif.svm.kernel = p_fct(c("linear", "radial")), # 移除polynomial
        # 增加gamma参数范围（对radial核）
        classif.svm.gamma = p_dbl(
            lower = -5, upper = -1,
            trafo = function(x) 10^x,
            depends = classif.svm.kernel == "radial"
        ),
        boruta.filter.frac = p_dbl(lower = 0.3, upper = 0.7)
    )
)

# 将benchmark结果转换为数据表
results_dt <- as.data.table(bmr)

# 查看可用的列
print(names(results_dt))
# 创建性能指标表格
performance_table <- data.frame(
    Model = unique(roc_data$learner),
    AUC = sapply(unique(roc_data$learner), function(l) unique(roc_data$AUC[roc_data$learner == l]))
)

# 打印性能表格
print(performance_table)


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

# 创建颜色映射
model_colors <- c(
    "boruta.select.classif.glmnet.tuned"  = "#E41A1C",    # 红色
    "boruta.select.classif.xgboost.tuned" = "#377EB8",    # 蓝色
    "boruta.select.classif.ranger.tuned"  = "#4DAF4A",    # 绿色
    "boruta.select.classif.svm.tuned"     = "#984EA3"     # 紫色
)

# 保存性能指标
write.csv(performance_table, "model_performance_metrics.csv", row.names = FALSE)

# 对每个模型进行训练和测试
model_results <- list()
# learner_name <- "glmnet"  "xgboost" "rf"      "svm"
#future::plan(future::sequential) # 改用串行计算模式

for (learner_name in names(auto_tuners)) {
    cat("\n处理模型:", learner_name, "\n")
    current_learner <- auto_tuners[[learner_name]]
    
    # 训练模型
    current_learner$train(task_train)
    
    # 在训练集上预测
    train_pred <- current_learner$predict(task_train)
    train_performance <- train_pred$score(msrs(c("classif.auc", "classif.acc","classif.ce")))
    
    # 在测试集上预测
    test_pred <- current_learner$predict(task_test)
    test_performance <- test_pred$score(msrs(c("classif.auc", "classif.acc","classif.ce")))
    
    # 准备ROC数据
    train_roc_data <- data.frame(
        truth = as.integer(train_pred$truth == task_train$positive),
        prob = train_pred$prob[, task_train$positive],
        dataset = "Training"
    )
    
    test_roc_data <- data.frame(
        truth = as.integer(test_pred$truth == task_train$positive),
        prob = test_pred$prob[, task_train$positive],
        dataset = "Testing"
    )

    # 获取特征重要性（如果可用）
    feature_importance <- NULL
    if (learner_name == "rf") {
        imp <- current_learner$model$learner$model$classif.ranger$model$variable.importance
        if (!is.null(imp)) {
            feature_importance <- data.frame(
                Feature = names(imp),
                Importance = as.numeric(imp)
            )
        }
    } else if (learner_name == "xgboost") {
        imp <- xgb.importance(model = current_learner$model$learner$model$classif.xgboost$model)
        if (!is.null(imp)) {
            feature_importance <- data.frame(
                Feature = imp$Feature,
                Importance = imp$Gain
            )
        }
    } else if (learner_name == "glmnet") {
        coefs <- coef(current_learner$model$learner$model$classif.glmnet$model)
        if (!is.null(coefs)) {
            nonzero_idx <- which(coefs[-1] != 0)
            feature_importance <- data.frame(
                Feature = rownames(coefs)[-1][nonzero_idx],
                Importance = abs(coefs[-1][nonzero_idx])
            )
        }
    }


    # 如果有特征重要性，绘制特征重要性图
    if (!is.null(feature_importance) && nrow(feature_importance) > 0) {
        # 按重要性排序
        feature_importance <- feature_importance[order(-feature_importance$Importance), ]
        # 选择top 20特征
        top_n <- min(20, nrow(feature_importance))
        top_features <- feature_importance[order(-feature_importance$Importance),][1:top_n,]
        
        p_importance <- ggplot(top_features, 
                             aes(x = reorder(Feature, Importance), y = Importance)) +
            geom_col(fill = "steelblue") +
            coord_flip() +
            mytheme() +
            labs(x = "特征", 
                 y = "重要性分数",
                 title = paste(learner_name, "模型 Top", top_n, "重要特征"))
        showtext_auto() # 自动启用中文支持
        ggsave(paste0(learner_name, "_feature_importance.pdf"), 
               p_importance, width = 10, height = 8)
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

# 创建性能比较表格
performance_comparison <- data.frame(
    Model = character(),
    Train_AUC = numeric(),
    Train_ACC = numeric(),
    Train_CE  = numeric(),
    Test_AUC = numeric(),
    Test_ACC = numeric(),
    Test_CE  = numeric(),
    stringsAsFactors = FALSE
)

for (model_name in names(model_results)) {
    performance_comparison <- rbind(
        performance_comparison,
        data.frame(
            Model = model_name,
            Train_AUC = model_results[[model_name]]$train_performance["classif.auc"],
            Train_ACC = model_results[[model_name]]$train_performance["classif.acc"],
            Train_CE  = model_results[[model_name]]$train_performance["classif.ce"],
            Test_AUC  = model_results[[model_name]]$test_performance["classif.auc"],
            Test_ACC  = model_results[[model_name]]$test_performance["classif.acc"],
            Test_CE  = model_results[[model_name]]$test_performance["classif.ce"]
        )
    )
}
write_tsv(performance_comparison, "model_performance_comparison.xls")

train_roc_all <- list()
test_roc_all <- list()
for (model_name in names(model_results)) {
    train_roc_all[[model_name]] <- model_results[[model_name]]$train_roc_data %>% mutate(model = model_name)
    test_roc_all[[model_name]] <- model_results[[model_name]]$test_roc_data %>% mutate(model = model_name)
}

roc_pict(train_roc_all, title = "训练集ROC曲线", path = "./train_roc_plot")
roc_pict(test_roc_all, title = "测试集ROC曲线", path = "./test_roc_plot")

pod_list <- train_roc_all
colors <- model_colors
labels <- names(pod_list)
title <- "训练集ROC曲线"
path <- "./train_roc_plot"
roc_pict(pod_list, title = "训练集ROC曲线", path = "./train_roc_plot",
                     colors = colors, labels = labels)

roc_pict <- function(pod_list, title = "", path = "./train_roc_plot",
                     colors = NULL, labels = NULL) {
    # pod_list: 一个列表，每个元素包含一个数据框，有d和m两列
    # colors: 自定义颜色向量
    # labels: 每条曲线的标签

    # 创建一个空的数据框来存储所有ROC数据
    all_roc_data <- c()#bind_rows(pod_list)
    auc_texts <- c()

    # 如果没有提供颜色，使用默认颜色方案
    if (is.null(colors)) {
        colors <- RColorBrewer::brewer.pal(min(9, length(pod_list)), "Set1")
        if (length(pod_list) > 9) {
            colors <- colorRampPalette(colors)(length(pod_list))
        }
    }

    # 处理每个模型的数据
    for (i in seq_along(pod_list)) {# i <- 2
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
        scale_color_manual(values = c("#E41A1C","#377EB8","#4DAF4A","#984EA3" )) +
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
            legend.margin = margin(t = 10, r = 0, b = 0, l = 0)
        )

    # 保存图形
    ggsave(paste0(path, ".pdf"), p,
        width = 10, height = 8, units = "in", dpi = 300
    )

    return(p)
}

roc_pict(train_roc_all,
    title = "训练集ROC曲线", path = "./train_roc_plot",
    colors = colors, labels = names(train_roc_all)
)

roc_pict(test_roc_all,
    title = "测试集ROC曲线", path = "./test_roc_plot",
    colors = colors, labels = names(test_roc_all)
)

# 打印性能比较表格
print("模型性能比较：")
print(performance_comparison)

# 保存性能比较表格
write.csv(performance_comparison, "model_performance_comparison.csv", row.names = FALSE)



