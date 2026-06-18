# 加载必要的包
library(fastshap)
library(shapviz)
library(glmnet)
library(dplyr)
library(ggplot2)
library(stringr)
library(ranger)
library(e1071)
library(writexl)

# 定义预测函数包装器
get_pred_wrapper <- function(mmodel) {
    # 按模型类型返回对应预测逻辑
    switch(mmodel,
        # 1. glmnet：用lambda.min返回概率
        glmnet = function(object, newdata) {
            as.numeric(predict(
                object,
                newx = as.matrix(newdata),
                type = "response", s = "lambda.min"
            ))
        },
        # 2. XGBoost：直接返回预测结果（分类概率/回归值）
        xgboost = function(object, newdata) {
            predict(object, newdata = as.matrix(newdata), type = "response")
        },
        # 3. 随机森林：适配概率估计/分类/回归
        rf = function(object, newdata) {
            newdata_df <- as.data.frame(newdata, check.names = FALSE)
            if (is.null(colnames(newdata_df))) colnames(newdata_df) <- colnames(X_train)

            pred_type <- if (object$treetype == "Probability estimation") "response" else "response"
            pred <- predict(object, data = newdata_df, type = pred_type)

            if (object$treetype == "Probability estimation") as.vector(pred$predictions[, 1]) else as.vector(pred$predictions)
        },
        # 4. SVM：提取最后一列概率作为目标类别
        svm = function(object, newdata) {
            pred <- predict(object, newdata = as.matrix(newdata), probability = TRUE)
            prob_matrix <- attr(pred, "probabilities")
            as.numeric(prob_matrix[, ncol(prob_matrix)])
        },
        # 5. LightGBM：按目标自动选type（分类prob/回归raw）
        lightgbm = function(object, newdata) {
            pred <- predict(
                object,
                newdata = as.matrix(newdata),
                type = ifelse(object$params$objective %in% c("binary", "multiclass"), "prob", "raw")
            )
            if (is.matrix(pred)) pred[, ncol(pred)] else as.vector(pred)
        },
        # 未知模型报错
        stop("不支持的模型类型：", mmodel)
    )
}

# 定义保存SHAP可视化图形的函数
save_shap_plots <- function(sv_obj, mmodel, data_type) {
    # 特征重要性条形图
    ggsave(str_c(mmodel, "_", data_type, "_SHAP_importance.pdf"),
        sv_importance(sv_obj) + ggtitle(str_c(mmodel, "-", data_type, " SHAP Importance")),
        width = 10, height = 6
    )
    # 蜂群图
    ggsave(str_c(mmodel, "_", data_type, "_SHAP_beeswarm.pdf"),
        sv_importance(sv_obj, kind = "beeswarm") + ggtitle(str_c(mmodel, "-", data_type, " SHAP Beeswarm")),
        width = 10, height = 8
    )
    # 力导向图
    ggsave(str_c(mmodel, "_", data_type, "_SHAP_force.pdf"),
        sv_force(sv_obj, row_id = 1) + ggtitle(str_c(mmodel, "-", data_type, " SHAP Force")),
        width = 12, height = 6
    )
}

# 定义SHAP分析主函数
shap_analysize <- function(model_results, mmodel) {
    # 1. 提取模型
    select_model_info <- model_results[[mmodel]]
    if (is.null(select_model_info)) stop("未找到模型：", mmodel)
    select_model <- select_model_info$model$model$learner$model
    cat("===== 处理模型：", mmodel, "（类型：", class(select_model)[1], "）=====\n")

    # 2. 准备训练集数据（XGBoost单独对齐特征名）
    train_ids <- rownames(Data)[split_gp$train]
    X_train <- as.matrix(Data[train_ids, selected_features, drop = FALSE])
    if (mmodel == "xgboost") {
        model_feats <- select_model$feature_names
        if (!identical(colnames(X_train), model_feats)) {
            X_train <- X_train[, model_feats, drop = FALSE]
            cat("已对齐XGBoost特征名\n")
        }
    }
    cat("训练集维度：", dim(X_train), "\n")

    # 3. 计算训练集SHAP值
    pred_wrapper <- get_pred_wrapper(mmodel) # 调用整合后的预测函数
    cat("计算训练集SHAP值...\n")
    shap_train <- fastshap::explain(
        object = select_model, X = as.data.frame(X_train),
        pred_wrapper = pred_wrapper, nsim = 100, adjust = TRUE
    )

    # 4. 训练集可视化+数据整理（含特征名映射）
    sv_train <- shapviz(shap_train, X = X_train)
    if (exists("names_values")) { # 特征名替换（若有映射表）
        names_values$new_colnames <- as.character(names_values$new_colnames)
        sv_x_mapping <- filter(names_values, new_colnames %in% colnames(sv_train$X))
        if (nrow(sv_x_mapping) > 0) {
            feature_map <- setNames(sv_x_mapping$old_colnames, sv_x_mapping$new_colnames)
            colnames(sv_train$X) <- feature_map[colnames(sv_train$X)]
            colnames(sv_train$S) <- feature_map[colnames(sv_train$S)]
        }
    }
    # 生成训练集可视化
    save_shap_plots(sv_train, mmodel, "train")

    # 5. 测试集处理（复用训练集逻辑）
    test_ids <- rownames(Data)[split_gp$test]
    X_test <- as.matrix(Data[test_ids, colnames(X_train), drop = FALSE])
    cat("计算测试集SHAP值...\n")
    shap_test <- fastshap::explain(
        object = select_model, X = as.data.frame(X_test),
        pred_wrapper = pred_wrapper, nsim = 50, adjust = TRUE
    )
    sv_test <- shapviz(shap_test, X = X_test)
    if (exists("sv_x_mapping") && nrow(sv_x_mapping) > 0) {
        colnames(sv_test$X) <- feature_map[colnames(sv_test$X)]
        colnames(sv_test$S) <- feature_map[colnames(sv_test$S)]
    }
    # 生成测试集可视化
    save_shap_plots(sv_test, mmodel, "test")

    # 6. 保存Excel结果（合并训练+测试）
    all_excel <- list(
        # 训练集数据
        train_shap = cbind(SampleID = train_ids, as.data.frame(sv_train$S)),
        train_sample_imp = cbind(SampleID = train_ids, as.data.frame(abs(sv_train$S))),
        train_imp = data.frame(Feature = colnames(sv_train$S), SHAP_Imp = colMeans(abs(sv_train$S))),
        # 测试集数据
        test_shap = cbind(SampleID = test_ids, as.data.frame(sv_test$S)),
        test_sample_imp = cbind(SampleID = test_ids, as.data.frame(abs(sv_test$S))),
        test_imp = data.frame(Feature = colnames(sv_test$S), SHAP_Imp = colMeans(abs(sv_test$S)))
    )
    write_xlsx(all_excel, str_c(mmodel, "_sv_SHAP.xlsx"))

    cat(mmodel, "处理完成！\n\n")
}


for (i in 1:length(names(model_results))) {
    shap_analysize(model_results, mmodel = names(model_results)[[i]])
}
