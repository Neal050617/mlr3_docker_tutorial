log_reg_lasso_callback <- clbk("custom_log_reg_lasso_callback",
    .help = "在 RFE 特征选择过程中，记录 Lasso 逻辑回归模型筛选出的非零系数特征。",
    callback_rfe_before_select = function(callback, instance, learner, feature_set, resampling, measure, fselector, ...) {
        # 检查学习器是否是逻辑回归 (classif.log_reg)
        if (!inherits(learner$model, "glm")) {
            stop("log_reg_lasso_callback 回调函数仅适用于线性模型 (classif.log_reg).")
        }

        # 1. 获取训练好的逻辑回归模型
        model <- learner$model

        # 2. 提取模型系数 (coefficients)
        coefficients <- coef(model)
        feature_names <- names(coefficients)[!names(coefficients) == "(Intercept)"] # 排除截距项
        importance_values <- coefficients[feature_names]

        # 3. 找出系数不为零的特征 (Lasso 选择的特征)
        selected_features <- feature_names[abs(importance_values) > 1e-6] #  使用一个小的阈值判断系数是否为零，避免浮点数精度问题

        # 4. 记录 Lasso 选择的特征名称 (例如，打印到控制台，或者存储到 instance 对象)
        cat(paste0("[Lasso Feature Selection] Iteration ", instance$iteration, ": Selected features: ", paste(selected_features, collapse = ", "), "\n"))

        # (可选) 可以将选择的特征名称存储到 instance 对象中，以便后续分析
        instance$log_reg_lasso_selected_features[[instance$iteration]] <- selected_features

        # (可选)  如果你还想记录特征重要性分数 (系数), 可以也存储到 instance 对象
        instance$feature_importance <- data.table(
            feature = feature_names,
            importance = as.numeric(importance_values)
        )
    },

    # 声明回调函数适用于 RFE 特征选择方法
    properties = "rfe",

    # 初始化 instance 对象中的存储列表 (用于存储每轮迭代选择的特征)
    setup = function(callback, instance) {
        instance$log_reg_lasso_selected_features <- list()
    }
)
