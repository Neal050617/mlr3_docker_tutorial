# rm(list=ls())
# =============================================================================
# MLR3 Ensemble Feature Selection (EFS) with External Validation
# 综合版本：整合Youden指数、PR曲线、混淆矩阵、外部验证等功能
# =============================================================================
# 环境配置 --------------------------------------------------------------
getOption("repos")
options(repos = structure(c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
options(BioC_mirror = "https://mirrors.tuna.tsinghua.edu.cn/bioconductor")

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
script_dir <- if (!is.na(script_file) && nzchar(script_file)) dirname(normalizePath(script_file)) else getwd()
local_lib_candidates <- unique(c(
  file.path(getwd(), ".r_libs"),
  file.path(script_dir, ".r_libs")
))
existing_local_libs <- local_lib_candidates[dir.exists(local_lib_candidates)]
if (length(existing_local_libs) > 0) {
  .libPaths(c(existing_local_libs, .libPaths()))
}

## 安装必要的包
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!("mlr3extralearners" %in% installed.packages())) {
  remotes::install_github("mlr-org/mlr3extralearners@*release")
}
if (!("aplot" %in% installed.packages())) {
  remotes::install_github("YuLab-SMU/aplot")
}

# 加载R包
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(
  optparse, tidyverse, openxlsx, mlr3verse, data.table, mltools, mlr3tuningspaces, future, readxl,
  treeshap, kernelshap, shapviz, mlr3extralearners, ranger, randomForest, pROC, patchwork, Boruta,
  showtext, xgboost, mlr3learners, mlr3tuning, paradox, future.apply, ggplot2, glmnet, e1071, nnet,
  mlr3measures, lightgbm, ROCR, naivebayes, kknn, rpart, RWeka
)

init_plot_font <- function() {
  font_candidates <- c(
    "/System/Library/Fonts/Hiragino Sans GB.ttc",
    "/System/Library/Fonts/STHeiti Medium.ttc",
    "/System/Library/Fonts/STHeiti Light.ttc",
    "/Library/Fonts/Arial Unicode.ttf"
  )
  font_path <- font_candidates[file.exists(font_candidates)][1]
  if (!is.na(font_path) && nzchar(font_path)) {
    sysfonts::font_add("plot_cn_font", regular = font_path)
    showtext::showtext_auto(enable = TRUE)
    showtext::showtext_opts(dpi = 300)
    update_geom_defaults("text", list(family = "plot_cn_font"))
    update_geom_defaults("label", list(family = "plot_cn_font"))
    return("plot_cn_font")
  }
  warning("未找到可用中文字体，图中中文可能无法正常显示。")
  return("")
}

plot_font_family <- init_plot_font()

if (nzchar(plot_font_family)) {
  ggplot2::theme_set(
    ggplot2::theme_get() +
      ggplot2::theme(
        text = ggplot2::element_text(family = plot_font_family),
        plot.title = ggplot2::element_text(family = plot_font_family),
        axis.title = ggplot2::element_text(family = plot_font_family),
        axis.text = ggplot2::element_text(family = plot_font_family),
        legend.text = ggplot2::element_text(family = plot_font_family),
        legend.title = ggplot2::element_text(family = plot_font_family),
        strip.text = ggplot2::element_text(family = plot_font_family)
      )
  )
}

save_pdf_plot <- function(filename, plot_obj, width, height, units = "in", dpi = 300, ...) {
  if (nzchar(plot_font_family) && capabilities("cairo")) {
    ggplot2::ggsave(
      filename = filename,
      plot = plot_obj,
      device = function(...) grDevices::cairo_pdf(..., family = plot_font_family),
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      ...
    )
  } else {
    ggplot2::ggsave(
      filename = filename,
      plot = plot_obj,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      ...
    )
  }
}

save_workspace_atomic <- function(outdir) {
  tmp_file <- file.path(outdir, "efs.RDataTmp")
  final_file <- file.path(outdir, "efs.RData")
  obj_names <- ls(envir = .GlobalEnv, all.names = TRUE)
  obj_names <- setdiff(obj_names, c("log_con", ".Last.value"))

  log_message("开始保存工作区到临时文件：", tmp_file)
  save(
    list = obj_names,
    file = tmp_file,
    envir = .GlobalEnv,
    compress = TRUE
  )
  log_message("临时工作区文件保存完成：", tmp_file)

  if (file.exists(final_file)) {
    file.remove(final_file)
  }

  renamed <- file.rename(tmp_file, final_file)
  if (!isTRUE(renamed)) {
    copied <- file.copy(tmp_file, final_file, overwrite = TRUE)
    if (!isTRUE(copied)) {
      stop("无法将临时工作区文件移动到最终位置：", final_file)
    }
    file.remove(tmp_file)
  }

  log_message("所有分析结果已保存到：", final_file)
}

finalize_workspace_save <- function(outdir) {
  final_file <- file.path(outdir, "efs.RData")

  if (exists("save_workspace_atomic", mode = "function", inherits = TRUE)) {
    get("save_workspace_atomic", mode = "function", inherits = TRUE)(outdir)
    return(invisible(final_file))
  }

  if (file.exists(final_file)) {
    log_message("未找到 save_workspace_atomic()，但检测到已有efs.RData，跳过重复保存", level = "WARNING")
    return(invisible(final_file))
  }

  log_message("未找到 save_workspace_atomic()，回退到 save.image()", level = "WARNING")
  save.image(final_file)
  log_message("所有分析结果已保存到：", final_file)
  invisible(final_file)
}

# 命令行参数设置 --------------------------------------------------------------
if (TRUE) {
  option_list <- list(
    # 输入文件
    make_option(c("-i", "--input"), type = "character", default = "OTU_tax.xls", help = "输入的特征表格"),
    make_option(c("-g", "--map"), type = "character", default = "map-group.txt", help = "分组文件"),
    make_option(c("-c", "--color"), type = "character", default = "color.txt", help = "指定颜色：color.txt"),

    # 数据划分
    make_option(c("--part"), type = "character", default = "none", help = "训练集比例，2/3或3/4或0.8"),
    make_option(c("--split"), type = "character", default = "split.map-group.txt", help = "客户指定train和test的split文件"),

    # 交叉验证和重采样
    make_option(c("--inner_cv"), type = "numeric", default = 5, help = "内层交叉验证的层数"),
    make_option(c("--outer_cv"), type = "numeric", default = 5, help = "外层交叉验证的层数"),
    make_option(c("--resample"), type = "numeric", default = 5, help = "重抽样次数，默认5次"),

    # 特征选择
    make_option(c("--n_features"), type = "numeric", default = 20, help = "RFE算法选择的特征数量"),
    make_option(c("--feature_fraction"), type = "double", default = 0.85, help = "RFE算法每次迭代保留的特征比例"),

    # 数据预处理
    make_option(c("-u", "--unif"), type = "logical", default = FALSE, help = "是否归一化"),
    make_option(c("--gp"), type = "character", default = "none", help = "control-test顺序指定"),
    make_option(c("--select"), type = "character", default = "none", help = "select.list"),
    make_option(c("--delete"), type = "character", default = "delect.txt", help = "delete.list"),

    # 外部验证
    make_option(c("--trainOnly"), type = "logical", default = TRUE, help = "TRUE表示只用train排序，FALSE表示训练集和内部验证集一起排序"),
    make_option(c("--valid"), type = "character", default = "OTU_tax.xls", help = "外部验证数据文件"),
    make_option(c("--map2"), type = "character", default = "map-group2.txt", help = "外部验证分组文件"),
    make_option(c("--fill_missing"), type = "logical", default = TRUE, help = "外部验证缺失的选中特征是否用0补全（默认TRUE）"),
    make_option(c("--load_rdata"), type = "character", default = "none", help = "加载已有的RData文件路径，跳过训练直接进行外部验证"),
    make_option(c("--redraw_only"), type = "logical", default = FALSE, help = "基于已有efs.RData仅重绘图表和报告，不重新训练/验证"),

    # 预处理数据加载（新增）
    make_option(c("--load_preprocessed"), type = "character", default = "none", help = "加载预处理数据RData路径（如果提供，跳过数据加载和预处理步骤）"),

    # 阈值和性能指标
    make_option(c("--threshold"), type = "character", default = "youden", help = "分类阈值：0~1数值或'youden'"),
    make_option(c("--pr_ci_boot"), type = "integer", default = 200, help = "PR曲线置信区间的自助法重复次数"),

    # 种子和计算资源
    make_option(c("-s", "--seed"), type = "numeric", default = 115702, help = "设置种子，默认123"),
    make_option(c("--cores"), type = "double", default = 2, help = "使用的线程数"),
    make_option(c("-o", "--outdir"), type = "character", default = "", help = "输出文件夹"),

    # 类别不平衡处理
    make_option(c("--use_class_weight"), type = "logical", default = FALSE, help = "是否使用类别权重处理不平衡（默认FALSE）"),
    make_option(c("--class_weight_method"), type = "character", default = "balanced", help = "权重计算方法：balanced（自动平衡）或manual（手动指定）"),
    make_option(c("--manual_weights"), type = "character", default = "none", help = "手动指定权重，格式：'1,3'表示正类权重1，负类权重3"),
    make_option(c("--use_smote"), type = "logical", default = FALSE, help = "是否在训练折内部启用SMOTE过采样（默认FALSE）"),
    make_option(c("--smote_k"), type = "integer", default = 5, help = "SMOTE近邻数K（默认5）"),
    make_option(c("--smote_dup_size"), type = "double", default = 1, help = "SMOTE dup_size参数（默认1，且必须>=0.5）"),

    # LOOCV控制参数
    make_option(c("--use_loocv"), type = "logical", default = FALSE, help = "是否使用LOOCV（默认FALSE）"),
    make_option(c("--auto_loocv"), type = "logical", default = FALSE, help = "根据样本量自动选择是否使用LOOCV（<100时启用）"),
    make_option(c("--loocv_scope"), type = "character", default = "outer", help = "LOOCV应用范围：'outer'（仅模型评估）、'inner'（含超参数调优）、'full'（全面）"),
    make_option(c("--loocv_threshold"), type = "numeric", default = 100, help = "auto_loocv的样本量阈值（默认100）"),

    # 调参控制
    make_option(c("--tune_evals"), type = "integer", default = 100, help = "每个模型/外层折的随机搜索评估次数（默认100）"),
    make_option(c("--tune_batch_size"), type = "integer", default = 100, help = "随机搜索每批评估数量（默认100）"),
    make_option(c("--mlr3_log_level"), type = "character", default = "info", help = "mlr3/bbotk控制台日志级别：trace,debug,info,warn,error,fatal（默认info）"),

    # 特征选择策略（新增）
    make_option(c("--feature_combine_method"),
      type = "character", default = "majority",
      help = "第二套方案的特征组合方式：intersection(交集),union(并集),majority(多数投票，默认)"
    ),
    make_option(c("--model_subset"), type = "character", default = "all", help = "参与建模的算法子集，逗号分隔；默认all。可选：glmnet,xgboost,rf,svm,naive_bayes,kknn,rpart,adaboost,nnet"),

    # 流程图生成
    make_option(c("--generate_flowchart"), type = "logical", default = TRUE, help = "是否生成分析流程图（默认TRUE）")
  )
  opts <- parse_args(OptionParser(option_list = option_list))
}

if (isTRUE(opts$use_smote) && isTRUE(opts$use_class_weight)) {
  stop("当前实现不支持同时开启 --use_smote 和 --use_class_weight。建议二选一后比较结果。")
}

if (isTRUE(opts$use_smote)) {
  if (!requireNamespace("smotefamily", quietly = TRUE)) {
    stop("开启SMOTE需要安装 R 包 smotefamily。请先安装后再运行。")
  }
  if (is.na(opts$smote_k) || opts$smote_k < 1) {
    stop("--smote_k 必须为大于等于1的整数")
  }
  if (is.na(opts$smote_dup_size) || opts$smote_dup_size < 0.5) {
    stop("--smote_dup_size 必须为大于等于0.5的数值")
  }
}

if (is.na(opts$tune_evals) || opts$tune_evals < 1) {
  stop("--tune_evals 必须为大于等于1的整数")
}
if (is.na(opts$tune_batch_size) || opts$tune_batch_size < 1) {
  stop("--tune_batch_size 必须为大于等于1的整数")
}
valid_mlr3_log_levels <- c("trace", "debug", "info", "warn", "error", "fatal")
opts$mlr3_log_level <- tolower(opts$mlr3_log_level)
if (!(opts$mlr3_log_level %in% valid_mlr3_log_levels)) {
  stop("--mlr3_log_level 必须为以下之一：", paste(valid_mlr3_log_levels, collapse = ", "))
}

set_mlr3_log_level <- function(level) {
  if (!requireNamespace("lgr", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  for (logger_name in c("mlr3", "bbotk", "mlr3fselect", "mlr3tuning")) {
    lgr::get_logger(logger_name)$set_threshold(level)
  }
  invisible(TRUE)
}
set_mlr3_log_level(opts$mlr3_log_level)

# 创建输出目录
if (opts$outdir == "") {
  opts$outdir <- file.path(getwd(), paste0("efs", opts$seed))
}
if (!isTRUE(dir.exists(opts$outdir))) {
  dir.create(opts$outdir)
}
runtime_outdir <- opts$outdir

# 设置日志系统
log_file <- file.path(opts$outdir, paste0("run_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
log_con <- file(log_file, open = "wt")

# 日志函数
log_message <- function(..., level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste0("[", timestamp, "] [", level, "] ", paste(..., collapse = " "))

  # 输出到控制台
  cat(msg, "\n")

  # 输出到日志文件
  tryCatch(
    {
      cat(msg, "\n", file = log_con)
      flush(log_con)
    },
    error = function(e) {
      # 如果日志文件写入失败，至少保证控制台输出
    }
  )
}

patch_mlr3learners_xgboost_compat <- function() {
  if (!requireNamespace("mlr3learners", quietly = TRUE) ||
    !requireNamespace("xgboost", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  xgb_ns <- asNamespace("xgboost")
  has_xgb_params <- "xgb.params" %in% getNamespaceExports("xgboost") ||
    exists("xgb.params", envir = xgb_ns, inherits = FALSE)
  if (isTRUE(has_xgb_params)) {
    return(invisible(FALSE))
  }

  mlr3learners_ns <- asNamespace("mlr3learners")
  as_numeric_matrix <- get("as_numeric_matrix", envir = mlr3learners_ns)
  get_weights <- get("get_weights", envir = mlr3learners_ns)
  `%||%` <- function(x, y) if (is.null(x)) y else x

  patched_train <- function(task) {
    pv <- self$param_set$get_values(tags = "train")
    lvls <- task$class_names
    nlvls <- length(lvls)

    if (isTRUE(pv$predcontrib) || isTRUE(pv$predinteraction) || isTRUE(pv$predleaf)) {
      warning("Predicting contributions, interactions, or leaf values with $predict() is not supported.")
    }
    if (is.null(pv$objective)) {
      pv$objective <- if (nlvls == 2L) "binary:logistic" else "multi:softprob"
    }
    if (self$predict_type == "prob" && identical(pv$objective, "multi:softmax")) {
      stop("objective = 'multi:softmax' does not work with predict_type = 'prob'")
    }
    if (pv$objective %in% c("multi:softmax", "multi:softprob")) {
      pv$num_class <- nlvls
    }

    data <- task$data(cols = task$feature_names)
    label <- nlvls - as.integer(task$truth())
    xgb_data <- xgboost::xgb.DMatrix(data = as_numeric_matrix(data), label = label)

    weights <- get_weights(task, private)
    if (!is.null(weights)) {
      xgboost::setinfo(xgb_data, "weight", weights)
    }

    if ("offset" %in% task$properties) {
      offset <- task$offset
      if (nlvls == 2L) {
        base_margin <- offset$offset
      } else {
        reordered_cols <- paste0("offset_", rev(levels(task$truth())))
        n_offsets <- ncol(offset) - 1
        if (length(reordered_cols) != n_offsets) {
          stop(sprintf(
            "Task has %i class labels, and only %i offset columns are provided",
            nlevels(task$truth()), n_offsets
          ))
        }
        base_margin <- as_numeric_matrix(offset)[, reordered_cols]
      }
      xgboost::setinfo(xgb_data, "base_margin", base_margin)
    }

    internal_valid_task <- task$internal_valid_task
    if (!is.null(pv$early_stopping_rounds) && is.null(internal_valid_task)) {
      stop(sprintf("Learner (%s): Configure field 'validate' to enable early stopping.", self$id))
    }
    if (!is.null(internal_valid_task)) {
      valid_data <- internal_valid_task$data(cols = internal_valid_task$feature_names)
      valid_label <- nlvls - as.integer(internal_valid_task$truth())
      xgb_valid_data <- xgboost::xgb.DMatrix(data = as_numeric_matrix(valid_data), label = valid_label)
      valid_weights <- get_weights(internal_valid_task, private)
      if (!is.null(valid_weights)) {
        xgboost::setinfo(xgb_valid_data, "weight", valid_weights)
      }
      if ("offset" %in% internal_valid_task$properties) {
        valid_offset <- internal_valid_task$offset
        if (nlvls == 2L) {
          base_margin <- valid_offset$offset
        } else {
          reordered_cols <- paste0("offset_", rev(levels(internal_valid_task$truth())))
          base_margin <- as_numeric_matrix(valid_offset)[, reordered_cols]
        }
        xgboost::setinfo(xgb_valid_data, "base_margin", base_margin)
      }
      pv$evals <- c(pv$evals, list(test = xgb_valid_data))
    }

    if (inherits(pv$custom_metric, "Measure")) {
      stop("custom_metric Measure is not supported by the local xgboost compatibility patch.")
    }

    xgb_train_args <- c(
      "nrounds", "evals", "custom_metric", "verbose", "print_every_n",
      "early_stopping_rounds", "maximize", "save_period", "save_name",
      "callbacks"
    )
    xgb_params <- pv[setdiff(names(pv), xgb_train_args)]
    train_args <- list(
      params = xgb_params,
      data = xgb_data,
      nrounds = pv$nrounds %||% 1L,
      evals = pv$evals %||% list(),
      custom_metric = pv$custom_metric,
      verbose = pv$verbose %||% 0,
      print_every_n = pv$print_every_n,
      early_stopping_rounds = pv$early_stopping_rounds,
      maximize = pv$maximize,
      save_period = pv$save_period,
      save_name = pv$save_name,
      callbacks = pv$callbacks %||% list()
    )
    train_args <- train_args[!vapply(train_args, is.null, logical(1))]
    model <- do.call(xgboost::xgb.train, train_args)

    list(structure("wrapper", model = model))
  }

  mlr3learners::LearnerClassifXgboost$set("private", ".train", patched_train, overwrite = TRUE)
  log_message(
    "已启用mlr3learners/xgboost兼容补丁：当前xgboost未提供xgb.params，改用脚本内参数过滤逻辑",
    level = "WARNING"
  )
  invisible(TRUE)
}

sanitize_feature_ids <- function(feature_names) {
  safe_names <- gsub("[^A-Za-z0-9._]", "_", feature_names)
  safe_names <- ifelse(
    grepl("^[A-Za-z.]", safe_names),
    safe_names,
    paste0("feature_", safe_names)
  )
  make.unique(safe_names, sep = "_dup_")
}

map_feature_labels <- function(features, mapping_df) {
  if (is.null(mapping_df) || length(features) == 0) {
    return(features)
  }

  feature_lookup <- stats::setNames(mapping_df$old_colnames, mapping_df$new_colnames)
  mapped <- unname(feature_lookup[features])
  mapped[is.na(mapped)] <- features[is.na(mapped)]
  mapped
}

# 记录开始时间
start_time <- Sys.time()
log_message(paste(rep("=", 80), collapse = ""))
log_message("EFS机器学习分析流程开始")
log_message(paste(rep("=", 80), collapse = ""))
log_message("开始时间：", start_time)
log_message("输出目录：", opts$outdir)
log_message("日志文件：", log_file)
log_message("随机种子：", opts$seed)
log_message(paste(rep("=", 80), collapse = ""))

# 记录参数
log_message("\n参数配置：")
log_message("  输入文件：", opts$input)
log_message("  分组文件：", opts$map)
log_message("  训练集比例：", opts$part)
log_message("  目标特征数：", opts$n_features)
log_message("  外层CV：", opts$outer_cv)
log_message("  内层CV：", opts$inner_cv)
log_message("  调参评估次数：", opts$tune_evals)
log_message("  调参批大小：", opts$tune_batch_size)
log_message("  mlr3日志级别：", opts$mlr3_log_level)
log_message("  并行核数：", opts$cores)
log_message("  类别权重：", opts$use_class_weight)
log_message("  SMOTE：", opts$use_smote)
if (isTRUE(opts$use_smote)) {
  log_message("  SMOTE参数：K=", opts$smote_k, ", dup_size=", opts$smote_dup_size)
}
log_message("  建模算法：", opts$model_subset)
log_message("  特征组合策略：", opts$feature_combine_method)
patch_mlr3learners_xgboost_compat()

# 捕获错误和警告
options(warn = 1) # 立即显示警告

# 在脚本结束时关闭日志
on.exit(
  {
    end_time <- Sys.time()
    elapsed_time <- difftime(end_time, start_time, units = "mins")

    log_message("\n", paste(rep("=", 80), collapse = ""))
    log_message("分析流程完成")
    log_message("结束时间：", end_time)
    log_message("总耗时：", round(elapsed_time, 2), "分钟")
    log_message(paste(rep("=", 80), collapse = ""))

    close(log_con)
    cat("\n日志已保存到：", log_file, "\n")
  },
  add = TRUE
)

# 检查是否加载已有的RData文件
if (opts$load_rdata != "none") {
  cat("\n=== 加载已有的RData文件 ===\n")
  cat("RData文件路径：", opts$load_rdata, "\n")

  if (!file.exists(opts$load_rdata)) {
    stop(paste0("指定的RData文件不存在：", opts$load_rdata))
  }

  # 保存当前命令行参数（使用低碰撞命名，避免被历史工作区对象覆盖）
  .codex_cli_opts_snapshot_20260331 <- opts
  load(opts$load_rdata, envir = .GlobalEnv)

  # 恢复当前命令行参数
  opts <- .codex_cli_opts_snapshot_20260331
  runtime_outdir <- .codex_cli_opts_snapshot_20260331$outdir
  rm(.codex_cli_opts_snapshot_20260331)
  selected_model_names <- parse_model_subset(opts$model_subset)

  cat("成功加载RData文件，跳过训练步骤，直接进行外部验证\n")
  cat("=========================\n\n")
}

redraw_only_mode <- isTRUE(opts$redraw_only)
if (redraw_only_mode && opts$load_rdata == "none") {
  stop("--redraw_only 需要同时提供 --load_rdata=已有efs.RData")
}

resume_training_mode <- FALSE
if (opts$load_rdata != "none" && !redraw_only_mode) {
  resume_objects <- c(
    "task_train", "task_test", "selected_features", "selected_model_names",
    "outer_resampling", "inner_resampling"
  )
  has_resume_objects <- all(vapply(resume_objects, exists, logical(1), inherits = TRUE))
  has_completed_benchmark <- exists("bmr", inherits = TRUE)
  has_final_report <- file.exists(file.path(runtime_outdir, "model_performance_comparison.xlsx"))

  if (has_resume_objects && !has_completed_benchmark && !has_final_report) {
    resume_training_mode <- TRUE
    log_message("检测到训练前检查点，将从已有efs.RData恢复模型训练和评估")
  }
}

# 设置计算环境
resolved_workers <- if (opts$cores != 0) floor(opts$cores) else ceiling(availableCores() / 6)
resolved_workers <- max(1, resolved_workers)
if (resolved_workers <= 1) {
  future::plan(sequential)
} else {
  future_backend <- if (identical(Sys.info()[["sysname"]], "Darwin")) multicore else multisession
  future::plan(future_backend, workers = resolved_workers)
}
set.seed(opts$seed, kind = "Mersenne-Twister")
options("encoding" = "UTF-8")
options(scipen = 5000)
options(dplyr.summarise.inform = FALSE)

# 启用进度条
library(progressr)
handlers(global = TRUE)
handlers("progress")

# 加载tidytree的内部函数
nodeid.tbl_tree <- utils::getFromNamespace("nodeid.tbl_tree", "tidytree")
rootnode.tbl_tree <- utils::getFromNamespace("rootnode.tbl_tree", "tidytree")
offspring.tbl_tree <- utils::getFromNamespace("offspring.tbl_tree", "tidytree")
offspring.tbl_tree_item <- utils::getFromNamespace(".offspring.tbl_tree_item", "tidytree")
child.tbl_tree <- utils::getFromNamespace("child.tbl_tree", "tidytree")
parent.tbl_tree <- utils::getFromNamespace("parent.tbl_tree", "tidytree")

# 01. 工具函数定义 --------------------------------------------------------------

# 改进的膝点检测函数（5层容错机制）
improved_knee_detection <- function(efs_result, default_n = 10) {
  tryCatch(
    {
      extract_knee_n <- function(knee_obj, label) {
        if (is.null(knee_obj)) {
          return(NULL)
        }
        if (!is.data.frame(knee_obj) || !"n_features" %in% names(knee_obj) || nrow(knee_obj) == 0) {
          return(NULL)
        }
        knee_n <- suppressWarnings(as.numeric(knee_obj$n_features[[1]]))
        if (!is.na(knee_n) && knee_n >= 1) {
          cat("✓ 使用", label, "膝点：", knee_n, "个特征\n", sep = "")
          return(list(n_features = knee_n, method = label))
        }
        NULL
      }

      empirical_result <- tryCatch(
        extract_knee_n(efs_result$knee_points(type = "empirical"), "empirical"),
        error = function(e) {
          log_message("⚠️  empirical膝点检测失败：", e$message, level = "WARNING")
          NULL
        }
      )
      if (!is.null(empirical_result)) {
        return(empirical_result)
      }

      estimated_result <- tryCatch(
        extract_knee_n(efs_result$knee_points(type = "estimated"), "estimated"),
        error = function(e) {
          log_message("⚠️  estimated膝点检测失败：", e$message, level = "WARNING")
          NULL
        }
      )
      if (!is.null(estimated_result)) {
        return(estimated_result)
      }

      archive <- tryCatch(as.data.frame(efs_result$archive$data), error = function(e) NULL)
      if (!is.null(archive) && nrow(archive) > 0 && all(c("n_features", "classif.ce") %in% names(archive))) {
        archive <- archive %>%
          mutate(
            n_features = suppressWarnings(as.numeric(n_features)),
            score = 1 - suppressWarnings(as.numeric(classif.ce))
          ) %>%
          filter(!is.na(n_features), !is.na(score)) %>%
          arrange(n_features) %>%
          distinct(n_features, .keep_all = TRUE)
      } else {
        archive <- NULL
      }

      if (!is.null(archive) && nrow(archive) > 3) {
        marginal_benefit <- diff(archive$score) / diff(archive$n_features)
        if (length(marginal_benefit) > 2 && any(is.finite(diff(marginal_benefit)))) {
          knee_idx <- which.min(diff(marginal_benefit)) + 1
          n_features <- archive$n_features[knee_idx]
          if (!is.na(n_features) && n_features >= 1) {
            cat("✓ 使用边际效益分析：", n_features, "个特征\n")
            return(list(n_features = n_features, method = "marginal_benefit"))
          }
        }
      }

      stability_scores <- tryCatch(
        efs_result$stability(stability_measure = "jaccard", global = FALSE),
        error = function(e) NULL
      )

      if (!is.null(archive) && !is.null(stability_scores) && nrow(stability_scores) > 0) {
        combined_data <- merge(archive, stability_scores, by = "n_features", all.x = TRUE)
        combined_data$stability[is.na(combined_data$stability)] <- 0.5
        combined_data$combined_score <- combined_data$score * combined_data$stability
        best_idx <- which.max(combined_data$combined_score)
        n_features <- combined_data$n_features[best_idx]
        if (!is.na(n_features) && n_features >= 1) {
          cat("✓ 使用稳定性-性能平衡：", n_features, "个特征\n")
          return(list(n_features = n_features, method = "stability_performance"))
        }
      }

      if (!is.null(archive) && nrow(archive) > 0) {
        performance_threshold <- quantile(archive$score, 0.95, na.rm = TRUE)
        candidates <- archive[archive$score >= performance_threshold, , drop = FALSE]
        if (nrow(candidates) > 0) {
          n_features <- min(candidates$n_features, na.rm = TRUE)
          if (!is.na(n_features) && n_features >= 1) {
            cat("✓ 使用性能阈值方法：", n_features, "个特征\n")
            return(list(n_features = n_features, method = "performance_threshold"))
          }
        }
      }

      log_message("⚠️  所有方法失败，使用默认值：", default_n, "个特征", level = "WARNING")
      return(list(n_features = default_n, method = "default"))
    },
    error = function(e) {
      log_message("❌ 膝点检测错误：", e$message, level = "ERROR")
      log_message("使用默认值：", default_n, "个特征", level = "WARNING")
      return(list(n_features = default_n, method = "error_fallback"))
    }
  )
}

build_numeric_design_matrix <- function(task) {
  raw_x <- as.data.frame(task$data(cols = task$feature_names))
  if (ncol(raw_x) == 0) {
    stop("无可用特征列")
  }

  raw_x[] <- lapply(raw_x, function(col) {
    if (is.character(col)) {
      col <- factor(col)
    }
    if (is.logical(col)) {
      col <- factor(col, levels = c(FALSE, TRUE))
    }
    col
  })

  design <- stats::model.matrix(~ . - 1, data = raw_x)
  if (is.null(dim(design))) {
    design <- matrix(design, ncol = 1)
    colnames(design) <- colnames(raw_x)[1]
  }

  storage.mode(design) <- "double"
  design
}

# 特征重要性聚合分析函数
aggregate_feature_importance <- function(efs_result, selected_features, outdir) {
  log_message("=== 特征重要性聚合分析 ===")

  # 检查是否有EFS结果
  if (is.null(efs_result)) {
    log_message("跳过特征重要性聚合分析（未使用EFS方法）", level = "WARNING")
    return(NULL)
  }

  tryCatch(
    {
      # 1. 从EFS获取排名
      efs_ranking <- efs_result$feature_ranking(
        method = "sav",
        committee_size = length(selected_features)
      )

      # 2. 计算稳定性得分
      stability_scores <- tryCatch(
        {
          efs_result$stability(stability_measure = "jaccard", global = FALSE)
        },
        error = function(e) NULL
      )

      # 3. 合并信息
      if (!is.null(stability_scores) && nrow(stability_scores) > 0) {
        feature_stats <- efs_ranking %>%
          left_join(stability_scores, by = c("feature" = "n_features")) %>%
          mutate(
            stability = ifelse(is.na(stability), 0.5, stability),
            combined_score = score * stability,
            rank = row_number(desc(combined_score))
          ) %>%
          arrange(rank)
      } else {
        feature_stats <- efs_ranking %>%
          mutate(
            stability = 0.5,
            combined_score = score,
            rank = row_number(desc(combined_score))
          ) %>%
          arrange(rank)
      }

      feature_stats <- feature_stats %>%
        mutate(feature_original = map_feature_labels(feature, names_values))

      # 4. 保存详细报告
      write_csv(feature_stats, file.path(outdir, "feature_importance_detailed.csv"))

      # 5. 绘制综合重要性图
      p_combined <- ggplot(
        feature_stats %>% head(min(20, nrow(feature_stats))),
        aes(x = reorder(feature_original, combined_score), y = combined_score)
      ) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        mytheme() +
        labs(
          x = "Feature", y = "Combined Score (Importance × Stability)",
          title = "Aggregated Feature Importance"
        )

      save_pdf_plot(file.path(outdir, "feature_importance_aggregated.pdf"), p_combined,
        width = 12, height = 10, units = "in", dpi = 300
      )

      log_message("✓ 特征重要性聚合分析完成")
      log_message("✓ 保存到：feature_importance_detailed.csv")
      log_message("✓ 图表保存到：feature_importance_aggregated.pdf")

      return(feature_stats)
    },
    error = function(e) {
      log_message("⚠️  特征重要性聚合分析失败：", e$message, level = "WARNING")
      return(NULL)
    }
  )
}

# 性能-复杂度权衡分析函数
performance_complexity_tradeoff <- function(efs_result, n_features_selected, outdir) {
  log_message("=== 性能-复杂度权衡分析 ===")

  # 检查是否有EFS结果
  if (is.null(efs_result)) {
    log_message("跳过性能-复杂度权衡分析（未使用EFS方法）", level = "WARNING")
    return(NULL)
  }

  tryCatch(
    {
      archive <- efs_result$archive$data

      # 检查archive是否为空
      if (is.null(archive) || nrow(archive) == 0) {
        log_message("⚠️  EFS archive为空，跳过权衡分析", level = "WARNING")
        return(NULL)
      }

      # 计算每个特征数量下的性能统计
      performance_stats <- archive %>%
        group_by(n_features) %>%
        summarise(
          mean_acc = mean(classif.acc, na.rm = TRUE),
          sd_acc = sd(classif.acc, na.rm = TRUE),
          min_acc = min(classif.acc, na.rm = TRUE),
          max_acc = max(classif.acc, na.rm = TRUE),
          n_runs = n(),
          .groups = "drop"
        ) %>%
        arrange(n_features)

      # 计算边际效益
      performance_stats <- performance_stats %>%
        mutate(
          marginal_benefit = c(NA, diff(mean_acc) / diff(n_features)),
          cumulative_benefit = mean_acc - first(mean_acc)
        )

      # 绘制权衡图
      p1 <- ggplot(performance_stats, aes(x = n_features, y = mean_acc)) +
        geom_line(color = "blue", size = 1) +
        geom_ribbon(aes(ymin = mean_acc - sd_acc, ymax = mean_acc + sd_acc),
          alpha = 0.3, fill = "blue"
        ) +
        geom_point(size = 2) +
        geom_vline(xintercept = n_features_selected, linetype = "dashed", color = "red") +
        annotate("text",
          x = n_features_selected, y = min(performance_stats$mean_acc),
          label = paste("Selected:", n_features_selected), hjust = -0.1
        ) +
        labs(
          title = "Performance vs. Number of Features",
          x = "Number of Features",
          y = "Mean Accuracy"
        ) +
        mytheme()

      p2 <- ggplot(
        performance_stats %>% filter(!is.na(marginal_benefit)),
        aes(x = n_features, y = marginal_benefit)
      ) +
        geom_line(color = "darkgreen", size = 1) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
        labs(
          title = "Marginal Benefit of Adding Features",
          x = "Number of Features",
          y = "Marginal Benefit (ΔAcc/ΔFeatures)"
        ) +
        mytheme()

      p_combined <- p1 / p2
      save_pdf_plot(file.path(outdir, "performance_complexity_tradeoff.pdf"), p_combined,
        width = 12, height = 10, units = "in", dpi = 300
      )

      # 保存统计数据
      write_csv(performance_stats, file.path(outdir, "performance_complexity_stats.csv"))

      log_message("✓ 权衡分析完成")
      log_message("✓ 图表保存到：performance_complexity_tradeoff.pdf")
      log_message("✓ 统计数据保存到：performance_complexity_stats.csv")

      return(performance_stats)
    },
    error = function(e) {
      log_message("⚠️  性能-复杂度权衡分析失败：", e$message, level = "WARNING")
      return(NULL)
    }
  )
}

# ROC数据提取函数
extract_roc_data_from_bmr <- function(bmr_object) {
  resample_results <- bmr_object$resample_results
  roc_data_list <- list()

  for (i in 1:nrow(resample_results)) {
    current_result <- resample_results$resample_result[[i]]
    model_name <- current_result$learner$id
    model_name <- str_replace(model_name, "classif\\.(.*?)\\.tuned", "\\1")
    predictions <- current_result$predictions()
    all_preds <- data.frame()

    for (fold in seq_along(predictions)) {
      pred <- predictions[[fold]]
      truth_values <- pred$truth
      pos_class <- current_result$task$positive

      # 安全提取预测概率
      prob_values <- tryCatch(
        {
          prob_col <- pred$prob[, pos_class]
          if (is.list(prob_col)) {
            as.numeric(unlist(prob_col))
          } else if (is.data.frame(prob_col)) {
            as.numeric(unlist(prob_col[, 1]))
          } else {
            as.numeric(prob_col)
          }
        },
        error = function(e) {
          rep(0.5, length(truth_values))
        }
      )

      truth_int <- as.integer(truth_values == pos_class)

      n_rows <- length(truth_int)
      dataset_vec <- rep("CV", n_rows)
      model_vec <- rep(model_name, n_rows)

      fold_data <- data.frame(
        truth = truth_int,
        prob = prob_values,
        dataset = dataset_vec,
        model = model_vec,
        stringsAsFactors = FALSE
      )

      all_preds <- rbind(all_preds, fold_data)
    }

    roc_data_list[[model_name]] <- all_preds
  }

  return(roc_data_list)
}

# 绘图主题
mytheme <- function() {
  theme_bw(base_family = plot_font_family) +
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

build_metric_annotation_df <- function(labels, metric_texts,
                                       text_x = 0.91,
                                       line_x_start = 0.935,
                                       line_x_end = 0.985,
                                       y_bottom = 0.06,
                                       step = 0.038) {
  n <- length(metric_texts)
  tibble::tibble(
    model = labels,
    label = metric_texts,
    text_x = rep(text_x, n),
    line_x_start = rep(line_x_start, n),
    line_x_end = rep(line_x_end, n),
    y = y_bottom + step * (seq_len(n) - 1)
  )
}

normalize_plot_model_id <- function(x) {
  x <- as.character(x)
  x <- sub("\\.tuned$", "", x)
  x <- sub("^classif\\.", "", x)
  x <- sub("^smote\\.", "", x)
  x[x == "ranger"] <- "rf"
  x[x == "AdaBoostM1"] <- "adaboost"
  x
}

prepare_curve_style <- function(pod_list, labels = NULL, colors = NULL) {
  if (is.null(labels)) {
    labels <- names(pod_list)
  }
  if (is.null(labels) || !length(labels) || any(!nzchar(labels))) {
    labels <- paste0("model_", seq_along(pod_list))
  }
  labels <- normalize_plot_model_id(labels)

  if (is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(min(9, length(labels)), "Set1")
    if (length(labels) > 9) {
      colors <- colorRampPalette(colors)(length(labels))
    }
    names(colors) <- labels
  } else if (is.null(names(colors))) {
    if (length(colors) < length(labels)) {
      colors <- rep(colors, length.out = length(labels))
    }
    colors <- colors[seq_along(labels)]
    names(colors) <- labels
  } else {
    color_names <- normalize_plot_model_id(names(colors))
    names(colors) <- color_names
    missing_labels <- setdiff(labels, names(colors))
    if (length(missing_labels)) {
      fallback_colors <- RColorBrewer::brewer.pal(min(9, length(labels)), "Set1")
      if (length(labels) > 9) {
        fallback_colors <- colorRampPalette(fallback_colors)(length(labels))
      }
      names(fallback_colors) <- labels
      colors <- c(colors, fallback_colors[missing_labels])
    }
    colors <- colors[labels]
  }

  list(labels = labels, colors = colors)
}

# ROC曲线绘制函数（多模型对比）
roc_pict <- function(pod_list, title = "", path = "./train_roc_plot",
                     colors = NULL, labels = NULL) {
  all_roc_data <- tibble::tibble()
  auc_texts <- character(0)
  plotted_labels <- character(0)

  curve_style <- prepare_curve_style(pod_list, labels = labels, colors = colors)
  labels <- curve_style$labels
  colors <- curve_style$colors

  for (i in seq_along(pod_list)) {
    pod <- pod_list[[i]]

    if (all(c("truth", "prob") %in% names(pod))) {
      pod <- data.frame(
        d = pod$truth,
        m = pod$prob
      )
    } else if (all(c("d", "m") %in% names(pod))) {
      pod <- data.frame(
        d = pod$d,
        m = pod$m
      )
    } else {
      pod <- data.frame(
        d = pod[, 1],
        m = pod[, 2]
      )
    }

    if (is.factor(pod$d)) {
      pod$d <- as.numeric(as.character(pod$d))
    } else if (is.logical(pod$d)) {
      pod$d <- as.integer(pod$d)
    } else {
      pod$d <- as.numeric(pod$d)
    }

    available_classes <- unique(na.omit(pod$d))
    if (length(available_classes) < 2) {
      auc_texts <- c(auc_texts, sprintf("%s: AUC = NA (仅单一分组)", labels[i]))
      next
    }

    roc_obj <- pROC::roc(pod$d, pod$m, direction = "<", quiet = TRUE)
    auc_value <- round(pROC::auc(roc_obj), 3)
    ci_obj <- pROC::ci.auc(roc_obj)

    roc_df <- tibble::tibble(
      model = labels[i],
      specificity = as.numeric(roc_obj$specificities),
      sensitivity = as.numeric(roc_obj$sensitivities)
    ) %>%
      mutate(
        fpr = 1 - specificity,
        tpr = sensitivity
      ) %>%
      filter(!is.na(fpr), !is.na(tpr)) %>%
      arrange(fpr, tpr) %>%
      distinct(fpr, .keep_all = TRUE) %>%
      dplyr::select(model, fpr, tpr)

    roc_df <- dplyr::bind_rows(
      tibble::tibble(model = labels[i], fpr = 0, tpr = 0),
      roc_df,
      tibble::tibble(model = labels[i], fpr = 1, tpr = 1)
    ) %>%
      arrange(fpr)

    all_roc_data <- dplyr::bind_rows(all_roc_data, roc_df)
    plotted_labels <- c(plotted_labels, labels[i])

    auc_texts <- c(auc_texts, sprintf(
      "%s: AUC = %.3f (%.3f-%.3f)",
      labels[i], auc_value,
      round(ci_obj[1], 3), round(ci_obj[3], 3)
    ))
  }

  if (nrow(all_roc_data) == 0) {
    warning(sprintf("无法绘制%s：所有模型仅包含单一分组。", title))
    placeholder <- ggplot() +
      theme_void() +
      labs(title = paste0(title, "（无法绘制）")) +
      annotate("text",
        x = 0.5, y = 0.5,
        label = "外部验证仅包含单一区组，无法计算ROC/PR。",
        size = 5
      )
    save_pdf_plot(paste0(path, ".pdf"), placeholder,
      width = 10, height = 8, units = "in", dpi = 300
    )
    return(list(plot = placeholder, data = NULL))
  }

  all_roc_data$model <- factor(all_roc_data$model, levels = plotted_labels)

  annotation_df <- build_metric_annotation_df(
    labels = plotted_labels,
    metric_texts = auc_texts,
    text_x = 0.91,
    line_x_start = 0.935,
    line_x_end = 0.985,
    y_bottom = 0.05,
    step = 0.037
  )

  p <- ggplot(all_roc_data, aes(
    x = fpr, y = tpr,
    color = model
  )) +
    geom_step(linewidth = 1, direction = "vh") +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", color = "grey40"
    ) +
    scale_color_manual(values = colors[plotted_labels]) +
    mytheme() +
    ggtitle(title) +
    labs(
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)"
    ) +
    geom_segment(
      data = annotation_df,
      aes(x = line_x_start, xend = line_x_end, y = y, yend = y, color = model),
      inherit.aes = FALSE,
      linewidth = 2.2,
      lineend = "round",
      show.legend = FALSE
    ) +
    geom_text(
      data = annotation_df,
      aes(x = text_x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      color = "black",
      size = 3.0,
      lineheight = 0.9,
      show.legend = FALSE
    ) +
    coord_equal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, margin = ggplot2::margin(b = 12)),
      plot.margin = ggplot2::margin(t = 15, r = 25, b = 15, l = 15)
    )

save_pdf_plot(paste0(path, ".pdf"), p,
    width = 10, height = 8, units = "in", dpi = 300
  )

  return(list(plot = p, data = all_roc_data))
}

normalize_model_id <- function(x) {
  x <- as.character(x)
  x <- sub("\\.tuned$", "", x)
  x <- sub("^classif\\.", "", x)
  x <- sub("^smote\\.", "", x)

  mapping <- c(
    ranger = "rf",
    rf = "rf",
    xgboost = "xgboost",
    svm = "svm",
    lightgbm = "lightgbm",
    glmnet = "glmnet",
    naive_bayes = "naive_bayes",
    nb = "naive_bayes",
    kknn = "kknn",
    knn = "kknn",
    rpart = "rpart",
    tree = "rpart",
    AdaBoostM1 = "adaboost",
    adaboost = "adaboost",
    nnet = "nnet"
  )

  out <- unname(mapping[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

parse_model_subset <- function(model_subset) {
  allowed_models <- c("glmnet", "xgboost", "rf", "svm", "naive_bayes", "kknn", "rpart", "adaboost", "nnet")
  if (is.null(model_subset) || !nzchar(model_subset) || identical(tolower(model_subset), "all")) {
    return(allowed_models)
  }

  requested <- strsplit(model_subset, ",", fixed = TRUE)[[1]]
  requested <- trimws(tolower(requested))
  requested <- requested[nzchar(requested)]
  invalid <- setdiff(requested, allowed_models)
  if (length(invalid) > 0) {
    stop("无效的 --model_subset 取值：", paste(invalid, collapse = ", "))
  }

  requested <- unique(requested)
  if (length(requested) == 0) {
    stop("--model_subset 不能为空")
  }
  requested
}

selected_model_names <- parse_model_subset(opts$model_subset)

wrap_learner_with_optional_smote <- function(learner, learner_id, opts) {
  learner$id <- learner_id
  if (!isTRUE(opts$use_smote)) {
    return(learner)
  }

  graph_learner <- as_learner(
    po("smote", K = opts$smote_k, dup_size = opts$smote_dup_size) %>>% learner
  )
  graph_learner$id <- learner_id
  graph_learner
}

wrap_efs_learner <- function(learner, learner_id, opts) {
  learner$id <- learner_id
  if (isTRUE(opts$use_smote)) {
    # RFE / svm_rfe 等EFS组件当前不兼容 GraphLearner。
    # 因此SMOTE仅用于后续模型训练，不进入EFS特征筛选层。
    return(learner)
  }
  learner
}

make_efs_learners <- function(max_nrounds, opts, class_weights = NULL) {
  weighted <- isTRUE(opts$use_class_weight) && !is.null(class_weights)
  scale_pos_weight <- NULL
  if (weighted && exists("gp", inherits = TRUE) && length(gp) >= 2) {
    scale_pos_weight <- class_weights[gp[2]] / class_weights[gp[1]]
  }

  xgb_values <- list(
    id = "xgb",
    nrounds = max_nrounds,
    verbose = 0
  )
  if (!is.null(scale_pos_weight) && !is.na(scale_pos_weight)) {
    xgb_values$scale_pos_weight <- scale_pos_weight
  }
  xgb_learner <- do.call(lrn, c(list("classif.xgboost"), xgb_values))

  rf_learner <- lrn("classif.ranger",
    id = "rf",
    importance = "permutation"
  )

  svm_learner <- lrn("classif.svm",
    id = "svm",
    type = "C-classification",
    kernel = "linear"
  )

  lgbm_values <- list(
    id = "lgbm",
    num_iterations = max_nrounds,
    learning_rate = 0.05,
    num_leaves = 31,
    objective = "binary",
    min_data_in_leaf = 20,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq = 5,
    verbose = -1
  )
  if (!is.null(scale_pos_weight) && !is.na(scale_pos_weight)) {
    lgbm_values$scale_pos_weight <- scale_pos_weight
  }
  lgbm_learner <- do.call(lrn, c(list("classif.lightgbm"), lgbm_values))

  if (weighted) {
    for (learner in list(rf_learner, svm_learner)) {
      learner$use_weights <- "ignore"
      learner$param_set$values$class.weights <- class_weights
    }
    xgb_learner$use_weights <- "ignore"
    lgbm_learner$use_weights <- "ignore"
  }

  list(
    wrap_efs_learner(xgb_learner, "xgb", opts),
    wrap_efs_learner(rf_learner, "rf", opts),
    wrap_efs_learner(svm_learner, "svm", opts),
    wrap_efs_learner(lgbm_learner, "lgbm", opts)
  )
}

extract_fitted_base_model <- function(current_learner) {
  trained_obj <- current_learner
  if (inherits(trained_obj, "AutoTuner")) {
    trained_obj <- trained_obj$model$learner
  }

  if (inherits(trained_obj, "GraphLearner")) {
    graph_model <- trained_obj$model
    if (is.null(graph_model) && !is.null(trained_obj$graph_model)) {
      graph_model <- trained_obj$graph_model
    }
    if (is.null(graph_model) || is.null(graph_model$pipeops) || length(graph_model$pipeops) == 0) {
      return(NULL)
    }
    target_id <- tail(names(graph_model$pipeops), 1)
    return(graph_model$pipeops[[target_id]]$model)
  }

  if (!is.null(trained_obj$model)) {
    return(trained_obj$model)
  }

  NULL
}

make_glmnet_search_space <- function(prefix = "") {
  do.call(ps, setNames(list(
    p_dbl(lower = 1e-4, upper = 1, logscale = TRUE),
    p_dbl(lower = 0, upper = 1),
    p_lgl(),
    p_dbl(lower = 1e-7, upper = 1e-4, logscale = TRUE),
    p_int(lower = 1e3, upper = 1e4)
  ), paste0(prefix, c("lambda", "alpha", "standardize", "thresh", "maxit"))))
}

make_xgboost_search_space <- function(prefix = "") {
  do.call(ps, setNames(list(
    p_dbl(0.01, 0.3),
    p_int(1, 10),
    p_int(50, 500),
    p_dbl(1, 10),
    p_dbl(0.5, 1),
    p_dbl(0.5, 1)
  ), paste0(prefix, c("eta", "max_depth", "nrounds", "min_child_weight", "subsample", "colsample_bytree"))))
}

make_rf_search_space <- function(prefix = "") {
  num_trees_id <- paste0(prefix, "num_trees")
  max_depth_id <- paste0(prefix, "max_depth")
  min_node_size_id <- paste0(prefix, "min_node_size")
  mtry_id <- paste0(prefix, "mtry")
  num_trees_out <- paste0(prefix, "num.trees")
  max_depth_out <- paste0(prefix, "max.depth")
  min_node_size_out <- paste0(prefix, "min.node.size")

  do.call(ps, c(
    setNames(list(
      p_int(lower = 500, upper = 1000),
      p_int(lower = 3, upper = 15),
      p_int(lower = 1, upper = 10),
      p_int(1, rf_mtry_upper)
    ), c(num_trees_id, max_depth_id, min_node_size_id, mtry_id)),
    list(.extra_trafo = function(x, param_set) {
      x[[num_trees_out]] <- x[[num_trees_id]]
      x[[max_depth_out]] <- x[[max_depth_id]]
      x[[min_node_size_out]] <- x[[min_node_size_id]]
      x[[num_trees_id]] <- NULL
      x[[max_depth_id]] <- NULL
      x[[min_node_size_id]] <- NULL
      x
    })
  ))
}

make_svm_search_space <- function(prefix = "") {
  do.call(ps, setNames(list(
    p_dbl(lower = -2, upper = 2, trafo = function(x) 10^x),
    p_dbl(lower = -6, upper = 1, trafo = function(x) 10^x)
  ), paste0(prefix, c("cost", "gamma"))))
}

make_naive_bayes_search_space <- function(prefix = "") {
  do.call(ps, setNames(list(p_dbl(0, 5)), paste0(prefix, "laplace")))
}

make_kknn_search_space <- function(prefix = "") {
  do.call(ps, setNames(list(
    p_int(3, 15),
    p_dbl(1, 3),
    p_fct(c("rectangular", "triangular", "epanechnikov", "gaussian"))
  ), paste0(prefix, c("k", "distance", "kernel"))))
}

make_rpart_search_space <- function(prefix = "") {
  do.call(ps, setNames(list(
    p_dbl(0.001, 0.1, logscale = TRUE),
    p_int(3, 20),
    p_int(1, 10)
  ), paste0(prefix, c("cp", "maxdepth", "minbucket"))))
}

make_adaboost_search_space <- function(prefix = "") {
  do.call(ps, setNames(list(p_int(50, 200)), paste0(prefix, "I")))
}

make_nnet_search_space <- function(prefix = "") {
  do.call(ps, setNames(list(
    p_int(5, 20),
    p_dbl(0.0001, 0.1, logscale = TRUE),
    p_int(100, 500)
  ), paste0(prefix, c("size", "decay", "maxit"))))
}

# 模型信息提取函数
get_model_info <- function(model_name, current_learner, task) {
  model_info <- list(
    model_name = model_name,
    features = task$feature_names,
    n_features = length(task$feature_names),
    hyperparameters = list(),
    performance = list(),
    feature_importance = NULL
  )

  if (inherits(current_learner, "AutoTuner")) {
    model_info$hyperparameters <- current_learner$tuning_instance$result_learner_param_vals
    model_info$tuning_results <- current_learner$tuning_instance$archive$data
  }

  fitted_model <- extract_fitted_base_model(current_learner)

  if (model_name == "rf" && !is.null(fitted_model)) {
    imp <- fitted_model$variable.importance
    if (!is.null(imp)) {
      model_info$feature_importance <- data.frame(
        Feature = names(imp),
        Importance = as.numeric(imp)
      )
    }
  } else if (model_name == "xgboost" && !is.null(fitted_model)) {
    if (inherits(fitted_model, "xgb.Booster")) {
      imp <- xgb.importance(model = fitted_model)
      if (!is.null(imp)) {
        model_info$feature_importance <- data.frame(
          Feature = imp$Feature,
          Importance = imp$Gain
        )
      }
    }
  } else if (model_name == "glmnet" && !is.null(fitted_model)) {
    coefs <- coef(fitted_model)
    if (!is.null(coefs)) {
      nonzero_idx <- which(coefs[-1] != 0)
      model_info$feature_importance <- data.frame(
        Feature = rownames(coefs)[-1][nonzero_idx],
        Importance = abs(coefs[-1][nonzero_idx])
      )
    }
  } else if (model_name == "lightgbm" && !is.null(fitted_model)) {
    if (inherits(fitted_model, "lgb.Booster")) {
      imp <- lightgbm::lgb.importance(fitted_model)
      if (!is.null(imp)) {
        model_info$feature_importance <- data.frame(
          Feature = imp$Feature,
          Importance = imp$Gain
        )
      }
    }
  } else if (model_name == "svm" && !is.null(fitted_model)) {
    sv_matrix <- fitted_model$SV
    if (!is.null(sv_matrix)) {
      sv_matrix <- as.matrix(sv_matrix)
    }
    if (!is.null(sv_matrix) && is.numeric(sv_matrix)) {
      sv_importance <- colMeans(abs(sv_matrix))
      model_info$feature_importance <- data.frame(
        Feature = names(sv_importance),
        Importance = sv_importance
      )
    }
  }

  return(model_info)
}

# 新增：自适应计算permutation重复次数
get_perm_repeats <- function(n_samples, n_features = NULL) {
  # 基于样本数的自适应规则
  if (n_samples < 100) {
    repeats <- 10 # 小样本：10次
  } else if (n_samples < 300) {
    repeats <- 5 # 中等样本：5次
  } else {
    repeats <- 3 # 大样本：3次
  }

  # 如果特征数很多，减少重复次数以控制计算时间
  if (!is.null(n_features) && n_features > 100) {
    repeats <- max(3, repeats - 2)
  }

  return(repeats)
}

# 新增：阈值计算函数（支持Youden指数）
get_threshold <- function(truth, prob, method = "0.5") {
  # 确保prob是numeric向量
  prob <- tryCatch(
    {
      if (is.list(prob)) {
        as.numeric(unlist(prob))
      } else if (is.data.frame(prob)) {
        as.numeric(unlist(prob[, 1]))
      } else {
        as.numeric(prob)
      }
    },
    error = function(e) {
      warning("概率转换失败，使用默认值0.5: ", e$message)
      rep(0.5, length(truth))
    }
  )

  method <- tolower(as.character(method))
  if (method == "youden") {
    tryCatch(
      {
        roc_obj <- pROC::roc(truth, prob, direction = "<", quiet = TRUE)
        th <- as.numeric(pROC::coords(roc_obj, "best", ret = "threshold"))
        if (length(th) == 0 || is.na(th)) th <- 0.5
        return(max(min(th, 1), 0))
      },
      error = function(e) {
        warning("Youden阈值计算失败，使用默认值0.5: ", e$message)
        return(0.5)
      }
    )
  }
  th <- suppressWarnings(as.numeric(method))
  if (is.na(th)) th <- 0.5
  return(max(min(th, 1), 0))
}

# 新增：基于阈值的性能计算函数
perf_with_threshold <- function(df, threshold) {
  truth <- as.integer(df$truth)
  pred <- as.integer(df$prob >= threshold)
  TP <- sum(pred == 1 & truth == 1, na.rm = TRUE)
  FP <- sum(pred == 1 & truth == 0, na.rm = TRUE)
  TN <- sum(pred == 0 & truth == 0, na.rm = TRUE)
  FN <- sum(pred == 0 & truth == 1, na.rm = TRUE)
  acc <- (TP + TN) / (TP + TN + FP + FN)
  ce <- 1 - acc
  precision <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
  recall <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
  f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_ else 2 * precision * recall / (precision + recall)
  list(
    acc = acc,
    ce = ce,
    precision = precision,
    recall = recall,
    f1 = f1,
    cm = c(TP = TP, FP = FP, TN = TN, FN = FN)
  )
}

# 新增：PR曲线置信区间计算函数
compute_pr_curve_ci <- function(truth_vals, prob_vals, n_boot = 200, conf_level = 0.95) {
  truth_vals <- as.numeric(truth_vals)
  prob_vals <- as.numeric(prob_vals)

  pred_obj <- ROCR::prediction(prob_vals, truth_vals)
  perf <- ROCR::performance(pred_obj, "prec", "rec")
  auprc_perf <- ROCR::performance(pred_obj, "aucpr")
  auprc_value <- auprc_perf@y.values[[1]]

  pr_df <- tibble(
    recall = as.numeric(perf@x.values[[1]]),
    precision = as.numeric(perf@y.values[[1]])
  ) %>%
    filter(!is.na(precision)) %>%
    group_by(recall) %>%
    summarise(precision = mean(precision, na.rm = TRUE), .groups = "drop") %>%
    arrange(recall)

  baseline <- mean(truth_vals, na.rm = TRUE)

  # 检查数据点是否足够
  if (nrow(pr_df) < 2) {
    log_message("⚠️  PR曲线数据点不足（<2），跳过置信区间计算", level = "WARNING")
    return(list(curve = pr_df, ci = NULL, baseline = baseline, auprc = auprc_value, valid_boot = 0))
  }

  recall_grid <- seq(0, 1, length.out = 101)

  # 安全的插值
  base_interp <- tryCatch(
    {
      approx(pr_df$recall, pr_df$precision, xout = recall_grid, method = "linear", rule = 2)$y
    },
    error = function(e) {
      log_message("⚠️  基础PR曲线插值失败，跳过置信区间计算", level = "WARNING")
      return(NULL)
    }
  )

  if (is.null(base_interp)) {
    return(list(curve = pr_df, ci = NULL, baseline = baseline, auprc = auprc_value, valid_boot = 0))
  }

  boot_mat <- matrix(NA_real_, nrow = length(recall_grid), ncol = n_boot)
  valid_boot <- 0

  for (b in seq_len(n_boot)) {
    idx <- sample(seq_along(truth_vals), replace = TRUE)
    boot_truth <- truth_vals[idx]
    if (length(unique(boot_truth)) < 2) next
    boot_prob <- prob_vals[idx]
    boot_pred <- ROCR::prediction(boot_prob, boot_truth)
    boot_perf <- ROCR::performance(boot_pred, "prec", "rec")
    boot_df <- tibble(
      recall = as.numeric(boot_perf@x.values[[1]]),
      precision = as.numeric(boot_perf@y.values[[1]])
    ) %>%
      filter(!is.na(precision)) %>%
      group_by(recall) %>%
      summarise(precision = mean(precision, na.rm = TRUE), .groups = "drop") %>%
      arrange(recall)

    if (nrow(boot_df) < 2) next # 至少需要2个点

    # 安全的插值
    boot_interp <- tryCatch(
      {
        approx(boot_df$recall, boot_df$precision, xout = recall_grid, method = "linear", rule = 2)$y
      },
      error = function(e) {
        NULL
      }
    )

    if (is.null(boot_interp) || all(is.na(boot_interp))) next
    valid_boot <- valid_boot + 1
    boot_mat[, valid_boot] <- boot_interp
  }

  ci_df <- NULL
  if (valid_boot > 0) {
    boot_mat <- boot_mat[, seq_len(valid_boot), drop = FALSE]
    alpha <- (1 - conf_level) / 2
    lower <- apply(boot_mat, 1, quantile, probs = alpha, na.rm = TRUE)
    upper <- apply(boot_mat, 1, quantile, probs = 1 - alpha, na.rm = TRUE)
    lower <- pmax(0, pmin(1, lower))
    upper <- pmax(0, pmin(1, upper))
    base_interp <- pmax(0, pmin(1, base_interp))
    ci_df <- tibble(
      recall = recall_grid,
      precision = base_interp,
      lower = lower,
      upper = upper
    )
  }

  list(
    curve = pr_df,
    ci = ci_df,
    baseline = baseline,
    auprc = auprc_value,
    valid_boot = valid_boot
  )
}

# 新增：单模型ROC曲线和箱线图（带阈值）
roc_box_plot <- function(pod_list, title = "", label = NULL, threshold = NA) {
  if (all(c("truth", "prob") %in% names(pod_list))) {
    truth_vals <- pod_list$truth
    prob_vals <- pod_list$prob
  } else if (all(c("d", "m") %in% names(pod_list))) {
    truth_vals <- pod_list$d
    prob_vals <- pod_list$m
  } else {
    truth_vals <- pod_list[[1]]
    prob_vals <- pod_list[[2]]
  }

  # 简化转换逻辑 - valid_roc_data中的truth已经是正确的0/1整数
  truth_vals <- as.numeric(truth_vals)
  prob_vals <- as.numeric(prob_vals)

  # 验证数据完整性
  if (!all(truth_vals %in% c(0, 1, NA))) {
    log_message("  ⚠️  Truth值不是0/1: ", paste(unique(truth_vals), collapse = ", "),
      level = "WARNING"
    )
  }

  if (length(unique(na.omit(truth_vals))) < 2) {
    empty_plot <- ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = "Insufficient class labels"), size = 5, hjust = 0.5, vjust = 0.5) +
      theme_void()
    return(list(empty_plot, empty_plot, NA))
  }

  # === 检查所有概率是否相同 ===
  unique_probs <- unique(na.omit(prob_vals))
  if (length(unique_probs) == 1) {
    log_message("  ⚠️  所有概率都相同 (", unique_probs[1], ")，无法绘制有效ROC曲线",
      level = "WARNING"
    )
    empty_plot <- ggplot() +
      geom_text(
        aes(
          x = 0.5, y = 0.5,
          label = paste0(
            "All probabilities are identical (", round(unique_probs[1], 3), ")\n",
            "Cannot draw meaningful ROC curve\n",
            "AUC will be 0.5 (random)"
          )
        ),
        size = 4, hjust = 0.5, vjust = 0.5
      ) +
      theme_void() +
      ggtitle(title)

    # 创建空的箱式图
    mapcol <- c("#61d04f", "#df536b")
    plot_df <- data.frame(truth = truth_vals, prob = prob_vals)
    truth_char <- as.character(plot_df$truth)
    truth_levels <- unique(truth_char)
    if (setequal(truth_levels, c("0", "1"))) {
      truth_levels <- c("0", "1")
    }
    plot_df <- plot_df %>% mutate(truth_label = factor(truth_char, levels = truth_levels))

    q <- ggplot(plot_df) +
      stat_boxplot(aes(x = truth_label, y = prob, group = truth_label),
        geom = "errorbar", linetype = 1, width = 0.5
      ) +
      geom_boxplot(aes(x = truth_label, y = prob, group = truth_label, fill = truth_label),
        show.legend = FALSE
      ) +
      scale_fill_manual(values = mapcol) +
      scale_x_discrete(labels = if (setequal(truth_levels, c("0", "1"))) {
        c("0" = "Ctrl", "1" = "Case")
      } else {
        waiver()
      }) +
      ggtitle(title) +
      mytheme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

    return(list(empty_plot, q, NA))
  }

  # === 检查概率范围是否太窄 ===
  prob_range <- max(prob_vals, na.rm = TRUE) - min(prob_vals, na.rm = TRUE)
  if (prob_range < 0.02) {
    log_message("  ⚠️  概率范围太窄 (", round(prob_range, 4), ")，ROC曲线可能不准确",
      level = "WARNING"
    )
  }

  # 强制指定direction = "<"，表示较低的预测值对应正类（truth=1）
  # 这确保所有模型的ROC曲线方向一致
  roc_obj <- pROC::roc(truth_vals, prob_vals, direction = "<", quiet = TRUE)

  get_coords_threshold <- function(th_val) {
    suppressWarnings({
      res <- try(pROC::coords(
        roc_obj,
        x = th_val,
        input = "threshold",
        ret = c("threshold", "specificity", "sensitivity"),
        transpose = FALSE
      ), silent = TRUE)
    })
    if (inherits(res, "try-error") || is.null(res)) {
      return(NULL)
    }
    as.data.frame(res)[1, , drop = FALSE]
  }

  # 检查AUC，如果接近0.5则不显示最优点
  auc_value_check <- pROC::auc(roc_obj)[1]

  cutoff_coords <- NULL
  cutoff_layer <- NULL
  point_label_layer <- NULL
  spec_sens_text <- "Spec/Sens: NA"

  # 如果AUC接近0.5（±0.05），不显示最优点
  if (abs(auc_value_check - 0.5) < 0.05) {
    log_message("  ⚠️  AUC接近0.5 (", round(auc_value_check, 3), ")，跳过最优点标注", level = "WARNING")
    spec_sens_text <- "Spec/Sens: NA (AUC~0.5)"
  } else {
    # 原来的逻辑：尝试获取最优点
    threshold_numeric <- suppressWarnings(as.numeric(threshold))
    if (!is.null(threshold) && !is.na(threshold_numeric)) {
      cutoff_coords <- get_coords_threshold(threshold_numeric)
      if (is.null(cutoff_coords) || any(is.na(c(cutoff_coords$specificity, cutoff_coords$sensitivity)))) {
        log_message(
          "  ⚠️  无法在当前数据集上定位指定阈值对应坐标，跳过最优点标注：",
          round(threshold_numeric, 4),
          level = "WARNING"
        )
        spec_sens_text <- "Spec/Sens: NA (train threshold unavailable)"
        cutoff_coords <- NULL
      }
    }
    if ((is.null(threshold) || is.na(threshold_numeric)) &&
      (is.null(cutoff_coords) || any(is.na(c(cutoff_coords$specificity, cutoff_coords$sensitivity))))) {
      best_coords <- pROC::coords(roc_obj, "best",
        ret = c("threshold", "specificity", "sensitivity"),
        transpose = FALSE
      )
      cutoff_coords <- as.data.frame(best_coords)[1, , drop = FALSE]
    }
  }

  # 计算cutoff_layer和point_label_layer（统一处理）
  if (!is.null(cutoff_coords) && all(!is.na(c(cutoff_coords$specificity, cutoff_coords$sensitivity)))) {
    cutoff_fpr <- 1 - cutoff_coords$specificity
    cutoff_layer <- geom_point(
      data = data.frame(fpr = cutoff_fpr, tpr = cutoff_coords$sensitivity),
      aes(x = fpr, y = tpr),
      color = "red"
    )
    spec_sens_text <- paste0(
      "Spec/Sens: ",
      round(cutoff_coords$specificity, 3), "/",
      round(cutoff_coords$sensitivity, 3)
    )
    point_label_layer <- annotate(
      "text",
      x = pmin(0.95, pmax(0.05, cutoff_fpr + 0.02)),
      y = pmin(0.95, pmax(0.05, cutoff_coords$sensitivity + 0.02)),
      label = paste0(
        round(cutoff_coords$threshold, 3), "\n(",
        round(cutoff_fpr, 3), ",",
        round(cutoff_coords$sensitivity, 3), ")"
      ),
      hjust = 0.5,
      vjust = 0,
      size = 3
    )
  }

  threshold_display <- if (!is.null(cutoff_coords)) cutoff_coords$threshold else NA_real_

  roc_curve <- tibble::tibble(
    specificity = as.numeric(roc_obj$specificities),
    sensitivity = as.numeric(roc_obj$sensitivities)
  ) %>%
    mutate(
      fpr = 1 - specificity,
      tpr = sensitivity
    ) %>%
    filter(!is.na(fpr), !is.na(tpr)) %>%
    arrange(fpr, tpr) %>%
    distinct(fpr, .keep_all = TRUE)

  if (nrow(roc_curve) == 0) {
    roc_curve <- tibble::tibble(fpr = c(0, 1), tpr = c(0, 1))
  } else {
    roc_curve <- dplyr::bind_rows(
      tibble::tibble(fpr = 0, tpr = 0),
      roc_curve %>% dplyr::select(fpr, tpr),
      tibble::tibble(fpr = 1, tpr = 1)
    ) %>%
      arrange(fpr)
  }

  ciobj <- pROC::ci.se(roc_obj, specificities = seq(0, 1, length.out = 101))
  if (is.null(ciobj)) {
    ci_df <- tibble::tibble()
  } else {
    ci_df <- tibble::tibble(
      specificity = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      median = ciobj[, 2],
      upper = ciobj[, 3]
    ) %>%
      mutate(fpr = 1 - specificity) %>%
      filter(!is.na(fpr), !is.na(lower), !is.na(upper)) %>%
      arrange(fpr)
  }

  # cutoff_layer和point_label_layer已经在上面定义，这里不需要重复

  auc_value <- pROC::auc(roc_obj)[1]
  auc_ci <- pROC::ci(roc_obj, of = "auc")
  auc_low <- auc_ci[1]
  auc_high <- auc_ci[3]
  U <- wilcox.test(prob_vals[truth_vals == 1], prob_vals[truth_vals == 0])

  threshold_text <- if (!is.na(threshold_display)) paste0("Threshold: ", round(threshold_display, 4)) else "Threshold: NA"
  annotation_text <- paste(
    paste0(label, " AUC: ", round(auc_value, 4)),
    paste0("95% CI: ", round(auc_low, 4), "-", round(auc_high, 4)),
    threshold_text,
    spec_sens_text,
    paste0("pvalue: ", Minus(U$p.value, 4)),
    sep = "\n"
  )

  p <- ggplot() +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour = "grey", linetype = "longdash", inherit.aes = FALSE) +
    {
      if (nrow(ci_df) > 0) {
        geom_ribbon(
          data = ci_df,
          aes(x = fpr, ymin = lower, ymax = upper),
          fill = "lightblue",
          alpha = 0.5,
          inherit.aes = FALSE
        )
      }
    } +
    geom_step(data = roc_curve, aes(x = fpr, y = tpr), color = "black", linewidth = 0.5, direction = "vh") +
    cutoff_layer +
    point_label_layer +
    annotate("text",
      x = 0.95, y = 0.15, hjust = 1, vjust = 0, size = 3,
      label = annotation_text
    ) +
    mytheme() +
    ggtitle(title) +
    labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_equal() +
    theme(legend.position = "bottom", legend.box = "horizontal")

  mapcol <- c("#61d04f", "#df536b")
  plot_df <- pod_list
  if (!"truth" %in% names(plot_df)) plot_df$truth <- truth_vals
  if (!"prob" %in% names(plot_df)) plot_df$prob <- prob_vals
  truth_char <- as.character(plot_df$truth)
  truth_levels <- unique(truth_char)
  if (setequal(truth_levels, c("0", "1"))) {
    truth_levels <- c("0", "1")
  }
  plot_df <- plot_df %>% mutate(truth_label = factor(truth_char, levels = truth_levels))

  q <- ggplot(plot_df) +
    stat_boxplot(aes(x = truth_label, y = prob, group = truth_label), geom = "errorbar", linetype = 1, width = 0.5) +
    geom_boxplot(aes(x = truth_label, y = prob, group = truth_label, fill = truth_label), show.legend = FALSE) +
    scale_fill_manual(values = mapcol) +
    scale_x_discrete(labels = if (setequal(truth_levels, c("0", "1"))) c("0" = "Ctrl", "1" = "Case") else waiver()) +
    ggtitle(title) +
    mytheme() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  return(list(p, q, U))
}

# 新增：单模型PR曲线和箱线图
pr_box_plot <- function(pod_list, title = "", label = NULL, threshold = NA) {
  if (all(c("truth", "prob") %in% names(pod_list))) {
    truth_vals <- pod_list$truth
    prob_vals <- pod_list$prob
  } else if (all(c("d", "m") %in% names(pod_list))) {
    truth_vals <- pod_list$d
    prob_vals <- pod_list$m
  } else {
    truth_vals <- pod_list[[1]]
    prob_vals <- pod_list[[2]]
  }

  if (is.factor(truth_vals)) {
    truth_vals <- as.character(truth_vals)
  }
  if (is.logical(truth_vals)) {
    truth_vals <- as.integer(truth_vals)
  }
  if (!is.numeric(truth_vals)) {
    truth_factor <- factor(truth_vals)
    if (length(levels(truth_factor)) < 2) {
      truth_vals <- as.numeric(truth_factor)
    } else {
      truth_vals <- as.integer(truth_factor == levels(truth_factor)[2])
    }
  }
  truth_vals <- as.numeric(truth_vals)
  prob_vals <- as.numeric(prob_vals)

  pod <- data.frame(d = truth_vals, m = prob_vals)

  if (length(unique(na.omit(pod$d))) < 2) {
    empty_plot <- ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = "Insufficient class labels"),
        size = 5, hjust = 0.5, vjust = 0.5
      ) +
      theme_void()
    return(list(empty_plot, empty_plot, NA))
  }

  pr_stats <- compute_pr_curve_ci(truth_vals, prob_vals, n_boot = opts$pr_ci_boot, conf_level = 0.95)
  pr_df <- pr_stats$curve
  ci_df <- pr_stats$ci
  baseline <- pr_stats$baseline
  auprc_value <- pr_stats$auprc
  threshold_input <- threshold
  if (!is.null(threshold_input) && !is.na(threshold_input)) {
    threshold_input <- as.numeric(threshold_input)
  }

  if (nrow(pr_df) == 0 || nrow(pr_df) < 2) {
    pr_plot <- ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = "Insufficient PR data\n(model may have failed)"),
        size = 5, hjust = 0.5, vjust = 0.5
      ) +
      theme_void() +
      ggtitle(title)
  } else {
    # 检查是否有足够的变化
    if (length(unique(prob_vals)) < 3) {
      pr_plot <- ggplot() +
        geom_text(
          aes(
            x = 0.5, y = 0.5,
            label = "Insufficient probability variation\n(constant predictions)"
          ),
          size = 5, hjust = 0.5, vjust = 0.5
        ) +
        theme_void() +
        ggtitle(title)
    } else {
      plot_data <- if (!is.null(ci_df) && nrow(ci_df) > 1) ci_df else pr_df

      # 确保有足够的数据点用于geom_path
      if (nrow(plot_data) < 2) {
        pr_plot <- ggplot() +
          geom_text(aes(x = 0.5, y = 0.5, label = "Insufficient data points for curve"),
            size = 5, hjust = 0.5, vjust = 0.5
          ) +
          theme_void() +
          ggtitle(title)
      } else {
        pr_plot <- ggplot(plot_data, aes(x = recall, y = precision))
        if (!is.null(ci_df) && nrow(ci_df) > 1 && any(!is.na(ci_df$lower)) && any(!is.na(ci_df$upper))) {
          pr_plot <- pr_plot +
            geom_ribbon(
              data = ci_df,
              aes(x = recall, ymin = lower, ymax = upper),
              fill = "lightblue",
              alpha = 0.5,
              inherit.aes = FALSE
            )
        }
        pr_plot <- pr_plot +
          geom_path(color = "black", size = 0.5) +
          geom_hline(yintercept = baseline, linetype = "longdash", color = "grey40") +
          annotate(
            "text",
            x = 0.95,
            y = 0.05,
            hjust = 1,
            vjust = 0,
            size = 3,
            label = paste(
              paste0(label, " AUPRC: ", round(auprc_value, 4)),
              paste0("Baseline: ", round(baseline, 4)),
              if (!is.null(threshold_input) && !is.na(threshold_input)) paste0("Threshold: ", round(threshold_input, 4)) else "Threshold: NA",
              sep = "\n"
            )
          ) +
          labs(x = "Recall", y = "Precision") +
          scale_x_continuous(limits = c(0, 1)) +
          scale_y_continuous(limits = c(0, 1)) +
          coord_equal() +
          mytheme() +
          theme(legend.position = "none") +
          ggtitle(title)
      }
    }
  }

  U <- wilcox.test(prob_vals[truth_vals == 1], prob_vals[truth_vals == 0])

  mapcol <- c("#61d04f", "#df536b")
  plot_df <- pod_list
  if (!"truth" %in% names(plot_df)) plot_df$truth <- truth_vals
  if (!"prob" %in% names(plot_df)) plot_df$prob <- prob_vals
  truth_char <- as.character(plot_df$truth)
  truth_levels <- unique(truth_char)
  if (setequal(truth_levels, c("0", "1"))) {
    truth_levels <- c("0", "1")
  }
  plot_df <- plot_df %>% mutate(truth_label = factor(truth_char, levels = truth_levels))

  q <- ggplot(plot_df) +
    stat_boxplot(aes(x = truth_label, y = prob, group = truth_label), geom = "errorbar", linetype = 1, width = 0.5) +
    geom_boxplot(aes(x = truth_label, y = prob, group = truth_label, fill = truth_label), show.legend = FALSE) +
    scale_fill_manual(values = mapcol) +
    scale_x_discrete(labels = if (setequal(truth_levels, c("0", "1"))) c("0" = "Ctrl", "1" = "Case") else waiver()) +
    ggtitle(title) +
    mytheme() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  return(list(pr_plot, q, U))
}

# 新增：多模型PR曲线对比
pr_pict <- function(pod_list, title = "", path = "./train_pr_plot",
                    colors = NULL, labels = NULL) {
  curve_style <- prepare_curve_style(pod_list, labels = labels, colors = colors)
  labels <- curve_style$labels
  colors <- curve_style$colors

  all_pr_data <- tibble::tibble()
  auprc_texts <- character(0)
  plotted_labels <- character(0)

  for (i in seq_along(pod_list)) {
    pod <- pod_list[[i]]
    if (!all(c("truth", "prob") %in% names(pod))) {
      if (all(c("d", "m") %in% names(pod))) {
        pod <- data.frame(truth = pod$d, prob = pod$m)
      } else {
        pod <- data.frame(truth = pod[, 1], prob = pod[, 2])
      }
    }

    if (is.factor(pod$truth)) {
      pod$truth <- as.numeric(as.character(pod$truth))
    } else if (is.logical(pod$truth)) {
      pod$truth <- as.integer(pod$truth)
    } else {
      pod$truth <- as.numeric(pod$truth)
    }

    available_classes <- unique(na.omit(pod$truth))
    if (length(available_classes) < 2) {
      auprc_texts <- c(auprc_texts, sprintf("%s: AUPRC = NA (仅单一分组)", labels[i]))
      next
    }

    pred_obj <- ROCR::prediction(pod$prob, pod$truth)
    perf <- ROCR::performance(pred_obj, "prec", "rec")

    auprc_perf <- ROCR::performance(pred_obj, "aucpr")
    auprc_value <- auprc_perf@y.values[[1]]

    pr_df <- tibble::tibble(
      recall = perf@x.values[[1]],
      precision = perf@y.values[[1]],
      model = labels[i]
    )
    all_pr_data <- dplyr::bind_rows(all_pr_data, pr_df)
    plotted_labels <- c(plotted_labels, labels[i])

    auprc_texts <- c(auprc_texts, sprintf("%s: AUPRC = %.3f", labels[i], auprc_value))
  }

  if (nrow(all_pr_data) == 0) {
    warning(sprintf("无法绘制%s：所有模型仅包含单一分组。", title))
    placeholder <- ggplot() +
      theme_void() +
      labs(title = paste0(title, "（无法绘制）")) +
      annotate("text",
        x = 0.5, y = 0.5,
        label = "外部验证仅包含单一区组，无法计算ROC/PR。",
        size = 5
      )
    save_pdf_plot(paste0(path, ".pdf"), placeholder,
      width = 10, height = 8, units = "in", dpi = 300
    )
    return(list(plot = placeholder, data = NULL))
  }

  all_pr_data$model <- factor(all_pr_data$model, levels = plotted_labels)

  annotation_df <- build_metric_annotation_df(
    labels = plotted_labels,
    metric_texts = auprc_texts,
    text_x = 0.91,
    line_x_start = 0.935,
    line_x_end = 0.985,
    y_bottom = 0.05,
    step = 0.037
  )

  p <- ggplot(all_pr_data, aes(x = recall, y = precision, color = model)) +
    geom_path(linewidth = 1) +
    scale_color_manual(values = colors[plotted_labels]) +
    mytheme() +
    ggtitle(title) +
    labs(x = "Recall (Sensitivity)", y = "Precision (PPV)") +
    geom_segment(
      data = annotation_df,
      aes(x = line_x_start, xend = line_x_end, y = y, yend = y, color = model),
      inherit.aes = FALSE,
      linewidth = 2.2,
      lineend = "round",
      show.legend = FALSE
    ) +
    geom_text(
      data = annotation_df,
      aes(x = text_x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      color = "black",
      size = 3.0,
      lineheight = 0.9,
      show.legend = FALSE
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, margin = ggplot2::margin(b = 12)),
      plot.margin = ggplot2::margin(t = 15, r = 25, b = 15, l = 15)
    )

  save_pdf_plot(paste0(path, ".pdf"), p, width = 10, height = 8, units = "in", dpi = 300)

  return(list(plot = p, data = all_pr_data))
}

redraw_saved_outputs <- function() {
  outdir <- if (exists("runtime_outdir", inherits = TRUE) && nzchar(runtime_outdir)) runtime_outdir else opts$outdir
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  required_objects <- c(
    "model_results", "performance_comparison", "model_details",
    "train_roc_all", "test_roc_all", "confusion_train_all", "confusion_test_all",
    "names_values", "model_colors"
  )
  missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
  if (length(missing_objects) > 0) {
    stop("重绘模式缺少必要对象：", paste(missing_objects, collapse = ", "))
  }

  log_message("=== 开始重绘图表和报告 ===")

  feature_descriptions <- names_values$old_colnames
  names(feature_descriptions) <- names_values$new_colnames

  if (exists("cv_roc_all") && length(cv_roc_all) > 0) {
    cv_labels <- names(cv_roc_all)
    cv_labels <- gsub("classif\\.", "", cv_labels)
    cv_labels <- gsub("\\.tuned", "", cv_labels)
    cv_labels[cv_labels == "ranger"] <- "rf"
    names(cv_roc_all) <- cv_labels
  }

  FS <- as.data.frame(feature_descriptions) %>%
    rownames_to_column() %>%
    rename_all(~ c("Feature", "Description"))

  Plot_data4 <- lapply(model_details, function(x) {
    if (is.null(x$feature_importance) || nrow(x$feature_importance) == 0) {
      return(data.frame(
        Feature = character(0),
        Importance = numeric(0),
        Description = character(0),
        Description_fold = character(0),
        stringsAsFactors = FALSE
      ))
    }

    x$feature_importance$Importance <- as.numeric(x$feature_importance$Importance)
    left_join(x$feature_importance, FS, by = c("Feature" = "Feature")) %>%
      arrange(Importance) %>%
      mutate(Description = ifelse(is.na(Description) | Description == "", Feature, Description)) %>%
      mutate(Description_fold = Add_breaks(Description)) %>%
      mutate(Description_fold = fct_inorder(Description_fold))
  })

  if (length(train_roc_all) > 0) {
    roc_pict(train_roc_all, title = "训练集ROC曲线", path = file.path(outdir, "train_roc_plot"), colors = model_colors, labels = names(train_roc_all))
    pr_pict(train_roc_all, title = "训练集PR曲线", path = file.path(outdir, "train_pr_plot"), colors = model_colors, labels = names(train_roc_all))
  }
  if (length(test_roc_all) > 0) {
    roc_pict(test_roc_all, title = "测试集ROC曲线", path = file.path(outdir, "test_roc_plot"), colors = model_colors, labels = names(test_roc_all))
    pr_pict(test_roc_all, title = "测试集PR曲线", path = file.path(outdir, "test_pr_plot"), colors = model_colors, labels = names(test_roc_all))
  }
  if (exists("cv_roc_all") && length(cv_roc_all) > 0) {
    roc_pict(cv_roc_all, title = "交叉验证ROC曲线", path = file.path(outdir, "cv_roc_plot"), colors = model_colors, labels = names(cv_roc_all))
  }

  nn <- intersect(names(train_roc_all), names(test_roc_all))
  for (model_name in nn) {
    threshold_used <- if (!is.null(model_results[[model_name]])) model_results[[model_name]]$threshold_used else NA
    p1 <- roc_box_plot(train_roc_all[[model_name]], title = "train", label = model_name, threshold = threshold_used)
    p2 <- roc_box_plot(test_roc_all[[model_name]], title = "test", label = model_name, threshold = threshold_used)
    pr1 <- pr_box_plot(train_roc_all[[model_name]], title = "train", label = model_name, threshold = threshold_used)
    pr2 <- pr_box_plot(test_roc_all[[model_name]], title = "test", label = model_name, threshold = threshold_used)

    p4 <- if (!is.null(Plot_data4[[model_name]]) && nrow(Plot_data4[[model_name]]) > 0) {
      plot_p_feat(Plot_data4[[model_name]], out = outdir, plot = FALSE)
    } else {
      ggplot() + geom_text(aes(x = 0.5, y = 0.5, label = "No feature importance data available"), size = 6, hjust = 0.5, vjust = 0.5) + theme_void()
    }

    bind_rows(list(
      p1[[3]] %>% broom::tidy() %>% mutate(data = "train"),
      p2[[3]] %>% broom::tidy() %>% mutate(data = "test")
    )) %>%
      write_tsv(file.path(outdir, paste0("step3.", model_name, "_pod_wilcox_test.tsv")))

    ComBn_plot(list(p1[1:2], p2[1:2]), w = p4, out_parm = outdir, biomaker_num_fix_parm = paste0(model_name, "_roc"))
    ComBn_plot(list(pr1[1:2], pr2[1:2]), w = p4, out_parm = outdir, biomaker_num_fix_parm = paste0(model_name, "_pr"))
  }

  if (exists("valid_results") && exists("valid_roc_all") && exists("valid_performance_comparison") && exists("confusion_valid_all")) {
    if (length(valid_roc_all) > 0) {
      roc_pict(valid_roc_all, title = "外部验证ROC曲线", path = file.path(outdir, "valid_roc_plot"), colors = model_colors, labels = names(valid_roc_all))
      pr_pict(valid_roc_all, title = "外部验证PR曲线", path = file.path(outdir, "valid_pr_plot"), colors = model_colors, labels = names(valid_roc_all))
    }
    for (model_name in names(valid_roc_all)) {
      threshold_used <- if (!is.null(model_results[[model_name]])) model_results[[model_name]]$threshold_used else NA
      p_valid <- roc_box_plot(valid_roc_all[[model_name]], title = "External Validation", label = model_name, threshold = threshold_used)
      pr_valid <- pr_box_plot(valid_roc_all[[model_name]], title = "External Validation", label = model_name, threshold = threshold_used)
      p4_valid <- if (!is.null(Plot_data4[[model_name]]) && nrow(Plot_data4[[model_name]]) > 0) plot_p_feat(Plot_data4[[model_name]], out = outdir, plot = FALSE) else ggplot() + theme_void()
      p_valid[[3]] %>% broom::tidy() %>% mutate(data = "external_validation") %>% write_tsv(file.path(outdir, paste0("step4.", model_name, "_external_validation_wilcox_test.tsv")))
      ComBn_plot(list(p_valid[1:2]), w = p4_valid, out_parm = outdir, biomaker_num_fix_parm = paste0(model_name, "_external_validation_roc"))
      ComBn_plot(list(pr_valid[1:2]), w = p4_valid, out_parm = outdir, biomaker_num_fix_parm = paste0(model_name, "_external_validation_pr"))
    }
  }

  log_message("✓ 重绘完成（仅图形文件）")
}

# 辅助函数
Add_breaks_by_sep <- function(x, sep = "_") {
  sep_positions <- str_locate(x, sep)[1, 1]
  letters <- strsplit(x, "")[[1]]
  paste0(
    paste0(letters[1:sep_positions - 1], collapse = ""),
    "\n",
    paste0(letters[(sep_positions + 1):length(letters)], collapse = "")
  )
}

knee_point <- function(y) {
  y <- as.numeric(y)
  y <- y[!is.na(y)]

  if (length(y) == 0) {
    return(0)
  }
  if (length(y) == 1) {
    return(y[1])
  }

  kk <- boxplot.stats(y)$conf[[2]]
  return(kk)
}

Add_breaks <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }

  x <- as.character(x)
  x[is.na(x)] <- "Unknown"

  xixi <- unique(x)
  char_lengths <- nchar(xixi)

  char_lengths <- char_lengths[!is.na(char_lengths)]
  if (length(char_lengths) == 0) {
    return(xixi)
  }

  kk <- knee_point(char_lengths)

  if (is.na(kk) || max(char_lengths) - kk < 5) {
    hehe <- xixi
  } else {
    haha <- floor(kk)
    hehe <- map_chr(xixi, function(x) {
      if (is.na(x) || x == "") {
        return("Unknown")
      }
      xx <- strsplit(x, "")[[1]]
      ifelse(length(xx) > haha, str_c(
        str_c(xx[1:haha], collapse = ""), "\n",
        str_c(xx[(haha + 1):length(xx)], collapse = "")
      ), x)
    })
  }
  return(hehe)
}

plot_p_feat <- function(var_imp, out = opts$outdir, plot = TRUE) {
  p_feat <- ggplot(var_imp, aes(x = Description_fold, y = Importance, fill = Importance)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    ylab("Variable Importance") +
    xlab("") +
    guides(fill = "none") +
    scale_fill_gradient(low = "#327eba", high = "#e06663") +
    theme_bw(base_family = plot_font_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  if (isTRUE(plot)) {
    save_pdf_plot(file.path(out, "00.MeanDecreaseAccuracy.pdf"), p_feat, width = 10, height = 6)
  }
  return(p_feat)
}

ComBn_plot <- function(P, w, out_parm = opts$outdir, biomaker_num_fix_parm = "") {
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
    plot_annotation(tag_levels = c("A", "1"))
  save_pdf_plot(file.path(out_parm, paste0("step3.", biomaker_num_fix_parm, ".patchwork.pdf")), Q,
    dpi = 300,
    width = 14,
    height = 4 * length(P)
  )
}

Minus <- function(x, n) {
  d1 <- 10^(-n)
  ifelse(x >= d1, round(x, 4), paste0("< ", d1))
}

is_binary <- function(x) {
  vals <- unique(x[!is.na(x)])
  length(vals) <= 2 && all(vals %in% c(0, 1))
}

# 类别权重计算函数
calculate_class_weights <- function(data, target_col, method = "balanced", manual_weights = NULL) {
  class_counts <- table(data[[target_col]])
  n_samples <- nrow(data)
  n_classes <- length(class_counts)

  if (method == "balanced") {
    # sklearn默认方法：weight = n_samples / (n_classes * n_samples_per_class)
    weights <- n_samples / (n_classes * class_counts)
    names(weights) <- names(class_counts)
  } else if (method == "manual" && !is.null(manual_weights)) {
    weights <- manual_weights
  } else {
    stop("Invalid class weight method or missing manual weights")
  }

  cat("\n计算的类别权重：\n")
  print(weights)

  return(weights)
}

# 流程图生成函数
generate_analysis_flowchart <- function(opts, analysis_info, outdir) {
  cat("\n=== 生成分析流程图 ===\n")

  tryCatch(
    {
      # 创建流程图数据
      flowchart_data <- list(
        # 基本信息
        analysis_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        seed = opts$seed,

        # 数据信息
        input_file = opts$input,
        map_file = opts$map,
        n_samples = analysis_info$n_samples,
        n_features_original = analysis_info$n_features_original,
        n_features_selected = analysis_info$n_features_selected,

        # 数据划分
        train_test_split = opts$part,
        train_samples = analysis_info$train_samples,
        test_samples = analysis_info$test_samples,

        # 特征选择
        feature_selection_method = analysis_info$feature_selection_method,
        knee_detection_method = analysis_info$knee_detection_method,

        # 重采样策略
        use_loocv = analysis_info$use_loocv,
        loocv_scope = analysis_info$loocv_scope,
        inner_cv = opts$inner_cv,
        outer_cv = opts$outer_cv,

        # 模型信息
        models = analysis_info$models,
        best_model = analysis_info$best_model,

        # 外部验证
        has_external_validation = opts$valid != "none",
        external_validation_file = opts$valid
      )

      # 生成流程图文本
      flowchart_text <- paste0(
        "# MLR3 EFS 分析流程图\n\n",
        "## 分析时间\n",
        flowchart_data$analysis_date, "\n\n",
        "## 数据输入\n",
        "```\n",
        "输入文件: ", flowchart_data$input_file, "\n",
        "分组文件: ", flowchart_data$map_file, "\n",
        "总样本数: ", flowchart_data$n_samples, "\n",
        "原始特征数: ", flowchart_data$n_features_original, "\n",
        "随机种子: ", flowchart_data$seed, "\n",
        "```\n\n",
        "## 数据划分\n",
        "```\n",
        "划分比例: ", flowchart_data$train_test_split, "\n",
        "训练集样本数: ", flowchart_data$train_samples, "\n",
        "测试集样本数: ", flowchart_data$test_samples, "\n",
        "```\n\n",
        "## 特征选择流程\n",
        "```\n",
        "方法: ", flowchart_data$feature_selection_method, "\n",
        "膝点检测方法: ", flowchart_data$knee_detection_method, "\n",
        "选择的特征数: ", flowchart_data$n_features_selected, "\n",
        "```\n\n",
        "## 重采样策略\n",
        "```\n",
        "使用LOOCV: ", flowchart_data$use_loocv, "\n",
        "LOOCV范围: ", flowchart_data$loocv_scope, "\n",
        "内层交叉验证: ", flowchart_data$inner_cv, "-fold\n",
        "外层交叉验证: ", flowchart_data$outer_cv, "-fold\n",
        "```\n\n",
        "## 模型训练\n",
        "```\n",
        "训练的模型: ", paste(flowchart_data$models, collapse = ", "), "\n",
        "最佳模型: ", flowchart_data$best_model, "\n",
        "```\n\n"
      )

      # 如果有外部验证，添加外部验证信息
      if (flowchart_data$has_external_validation) {
        flowchart_text <- paste0(
          flowchart_text,
          "## 外部验证\n",
          "```\n",
          "外部验证文件: ", flowchart_data$external_validation_file, "\n",
          "```\n\n"
        )
      }

      # 添加流程图
      # 清理特殊字符以避免Mermaid语法错误
      clean_text <- function(text) {
        text <- gsub("\\(", "&#40;", text) # 替换左括号
        text <- gsub("\\)", "&#41;", text) # 替换右括号
        text <- gsub("\\[", "&#91;", text) # 替换左方括号
        text <- gsub("\\]", "&#93;", text) # 替换右方括号
        text <- gsub("\\{", "&#123;", text) # 替换左花括号
        text <- gsub("\\}", "&#125;", text) # 替换右花括号
        return(text)
      }

      # 清理可能包含特殊字符的字段
      safe_method <- clean_text(as.character(flowchart_data$feature_selection_method))
      safe_knee <- clean_text(as.character(flowchart_data$knee_detection_method))
      safe_models <- clean_text(paste(flowchart_data$models, collapse = " "))
      safe_best <- clean_text(as.character(flowchart_data$best_model))

      flowchart_text <- paste0(
        flowchart_text,
        "## 分析流程图\n\n",
        "```mermaid\n",
        "graph TD\n",
        "    A[\"数据输入<br/>", flowchart_data$n_samples, "样本<br/>", flowchart_data$n_features_original, "特征\"] --> B[\"数据预处理<br/>去除常数特征<br/>缺失值填充\"]\n",
        "    B --> C[\"数据划分<br/>训练集: ", flowchart_data$train_samples, "<br/>测试集: ", flowchart_data$test_samples, "\"]\n",
        "    C --> D[\"特征选择<br/>方法: ", safe_method, "<br/>膝点检测: ", safe_knee, "\"]\n",
        "    D --> E[\"选择特征<br/>", flowchart_data$n_features_selected, "个特征\"]\n",
        "    E --> F[\"模型训练<br/>", safe_models, "\"]\n",
        "    F --> G[\"超参数调优<br/>", flowchart_data$inner_cv, "-fold CV\"]\n",
        "    G --> H[\"模型评估<br/>", flowchart_data$outer_cv, "-fold CV\"]\n",
        "    H --> I[\"最佳模型<br/>", safe_best, "\"]\n",
        "    I --> J[\"性能评估<br/>ROC/PR曲线<br/>混淆矩阵\"]\n"
      )

      # 如果有外部验证，添加外部验证节点
      if (flowchart_data$has_external_validation) {
        flowchart_text <- paste0(
          flowchart_text,
          "    J --> K[\"外部验证<br/>独立数据集\"]\n",
          "    K --> L[\"最终报告\"]\n"
        )
      } else {
        flowchart_text <- paste0(
          flowchart_text,
          "    J --> L[\"最终报告\"]\n"
        )
      }

      flowchart_text <- paste0(flowchart_text, "```\n\n")

      # 添加参数总结
      flowchart_text <- paste0(
        flowchart_text,
        "## 完整参数列表\n\n",
        "| 参数 | 值 |\n",
        "|------|----|\n"
      )

      # 添加所有参数
      for (param_name in names(opts)) {
        param_value <- opts[[param_name]]
        if (is.logical(param_value)) {
          param_value <- ifelse(param_value, "TRUE", "FALSE")
        }
        flowchart_text <- paste0(
          flowchart_text,
          "| ", param_name, " | ", param_value, " |\n"
        )
      }

      # 保存流程图
      flowchart_file <- file.path(outdir, "analysis_flowchart.md")
      writeLines(flowchart_text, flowchart_file)

      cat("✓ 流程图已生成：", flowchart_file, "\n")
      cat("✓ 可以使用支持Mermaid的Markdown查看器查看流程图\n")
      cat("===================\n\n")

      return(flowchart_file)
    },
    error = function(e) {
      log_message("⚠️  流程图生成失败：", e$message, level = "WARNING")
      return(NULL)
    }
  )
}

# 02. 多种特征选择方法实现 ------------------------------------------------------

# Boruta特征选择
boruta_feature_selection <- function(task, max_runs = 500) {
  cat("\n运行Boruta特征选择...\n")
  tryCatch(
    {
      data <- task$data()
      boruta_result <- Boruta(group ~ ., data = data, doTrace = 2, maxRuns = max_runs)

      # 获取确定重要的特征
      selected <- getSelectedAttributes(boruta_result, withTentative = FALSE)

      # 如果没有确定重要的特征，包含tentative特征
      if (length(selected) == 0) {
        log_message("⚠️  Boruta未找到确定重要特征，包含可能重要特征", level = "WARNING")
        selected <- getSelectedAttributes(boruta_result, withTentative = TRUE)
      }

      if (length(selected) > 0) {
        cat("✓ Boruta选择了", length(selected), "个特征\n")
        return(list(features = selected, result = boruta_result))
      } else {
        stop("Boruta未选出任何特征")
      }
    },
    error = function(e) {
      log_message("❌ Boruta失败：", e$message, level = "ERROR")
      return(NULL)
    }
  )
}

# RFE特征选择
rfe_feature_selection <- function(task, n_features = 20, learner = NULL) {
  cat("\n运行RFE特征选择...\n")
  tryCatch(
    {
      if (is.null(learner)) {
        learner <- lrn("classif.ranger", importance = "impurity")
      }

      rfe <- fs("rfe", n_features = n_features, feature_fraction = 0.85)

      instance <- fselect(
        fselector = rfe,
        task = task,
        learner = learner,
        resampling = rsmp("cv", folds = 5),
        measure = msr("classif.ce"),
        store_models = FALSE
      )

      selected <- instance$result_feature_set

      if (length(selected) > 0) {
        cat("✓ RFE选择了", length(selected), "个特征\n")
        return(list(features = selected, result = instance))
      } else {
        stop("RFE未选出任何特征")
      }
    },
    error = function(e) {
      log_message("❌ RFE失败：", e$message, level = "ERROR")
      return(NULL)
    }
  )
}

# LASSO特征选择
lasso_feature_selection <- function(task, lambda = "lambda.min") {
  cat("\n运行LASSO特征选择...\n")

  tryCatch(
    {
      X <- build_numeric_design_matrix(task)
      n_features <- ncol(X)
      if (n_features < 2) {
        log_message("⚠️  LASSO跳过：设计矩阵特征数量不足（需要至少2个特征，当前", n_features, "个）", level = "WARNING")
        return(NULL)
      }

      data <- task$data(cols = task$target_names)
      y <- as.factor(data[[task$target_names]])

      # 交叉验证选择lambda
      cv_fit <- cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = 5)

      # 提取非零系数
      coefs <- coef(cv_fit, s = lambda)
      selected <- rownames(coefs)[which(coefs != 0)][-1] # 移除截距

      if (length(selected) > 0) {
        cat("✓ LASSO选择了", length(selected), "个特征\n")
        return(list(features = selected, result = cv_fit))
      } else {
        stop("LASSO未选出任何特征")
      }
    },
    error = function(e) {
      log_message("❌ LASSO失败：", e$message, level = "ERROR")
      return(NULL)
    }
  )
}

# XGBoost重要性特征选择
xgboost_importance_selection <- function(task, top_n = 20) {
  cat("\n运行XGBoost重要性特征选择...\n")
  tryCatch(
    {
      X <- build_numeric_design_matrix(task)
      y_raw <- task$data(cols = task$target_names)[[task$target_names]]
      y <- if (is.factor(y_raw)) {
        as.integer(y_raw == levels(y_raw)[2])
      } else {
        as.integer(as.factor(y_raw)) - 1L
      }

      dtrain <- xgboost::xgb.DMatrix(data = X, label = y)
      xgb_fit <- xgboost::xgb.train(
        params = list(
          objective = "binary:logistic",
          eval_metric = "logloss",
          eta = 0.1,
          max_depth = 4,
          subsample = 0.8,
          colsample_bytree = 0.8
        ),
        data = dtrain,
        nrounds = 100,
        verbose = 0
      )

      importance <- xgboost::xgb.importance(model = xgb_fit, feature_names = colnames(X))
      selected <- head(importance$Feature, top_n)

      if (length(selected) > 0) {
        cat("✓ XGBoost选择了", length(selected), "个特征\n")
        return(list(features = selected, result = importance))
      } else {
        stop("XGBoost未选出任何特征")
      }
    },
    error = function(e) {
      log_message("❌ XGBoost重要性选择失败：", e$message, level = "ERROR")
      return(NULL)
    }
  )
}

# 特征集组合函数
combine_feature_sets <- function(feature_sets, method = "intersection", max_features = NULL) {
  log_message("组合特征集，方法：", method)

  if (length(feature_sets) == 0) {
    return(NULL)
  }

  if (length(feature_sets) == 1) {
    result <- feature_sets[[1]]
    # 如果指定了最大特征数，进行截断
    if (!is.null(max_features) && length(result) > max_features) {
      log_message("特征数超过限制，从", length(result), "截断到", max_features)
      result <- head(result, max_features)
    }
    return(result)
  }

  if (method == "intersection") {
    # 取交集：所有方法都选中的特征
    result <- Reduce(intersect, feature_sets)
    log_message("交集特征数：", length(result))
  } else if (method == "union") {
    # 取并集：任一方法选中的特征
    result <- Reduce(union, feature_sets)
    log_message("并集特征数：", length(result))
  } else if (method == "majority") {
    # 多数投票：超过半数方法选中的特征
    all_features <- unlist(feature_sets)
    feature_counts <- table(all_features)
    threshold <- ceiling(length(feature_sets) / 2)
    result <- names(feature_counts[feature_counts >= threshold])
    log_message("多数投票特征数：", length(result), "（阈值：", threshold, "/", length(feature_sets), "）")
  } else {
    log_message("⚠️  未知的组合方法，使用第一个特征集", level = "WARNING")
    result <- feature_sets[[1]]
  }

  # 如果指定了最大特征数且结果超过限制，进行截断
  if (!is.null(max_features) && length(result) > max_features) {
    log_message("特征数", length(result), "超过限制", max_features, "，进行截断", level = "WARNING")

    # 尝试按特征在各方法中的出现频率排序
    all_features <- unlist(feature_sets)
    feature_counts <- table(all_features)

    # 只保留在result中的特征的计数
    result_counts <- feature_counts[names(feature_counts) %in% result]

    # 按出现频率降序排序
    sorted_features <- names(sort(result_counts, decreasing = TRUE))

    # 取前max_features个
    result <- head(sorted_features, max_features)
    log_message("截断后特征数：", length(result))
  }

  return(result)
}

# 多方法特征选择主函数（两套方案）
multi_method_feature_selection <- function(task, opts, efs_result = NULL, knee_result = NULL) {
  log_message("=== 两套方案特征选择系统 ===")

  selected_features <- NULL
  method_used <- NULL
  all_results <- list()

  # 第一套方案：EFS（优先）
  log_message("【第一套方案】尝试使用EFS")
  if (!is.null(efs_result) && !is.null(knee_result)) {
    if (!(knee_result$method %in% c("default", "error_fallback"))) {
      log_message("✓ EFS膝点检测成功，使用EFS结果")
      res <- efs_result$feature_ranking(method = "sav", committee_size = knee_result$n_features)
      selected_features <- res$feature
      method_used <- paste0("EFS (", knee_result$method, ")")
      all_results[["efs"]] <- list(features = selected_features, method = method_used)

      log_message("✓ 第一套方案成功，特征数量：", length(selected_features))
    } else {
      log_message("❌ EFS膝点检测失败，切换到第二套方案", level = "WARNING")
    }
  } else {
    log_message("❌ EFS结果不可用，切换到第二套方案", level = "WARNING")
  }

  # 第二套方案：多方法组合（EFS失败时）
  if (is.null(selected_features)) {
    log_message("【第二套方案】多方法组合系统")

    # 解析组合策略
    combine_method <- opts$feature_combine_method
    if (combine_method == "none") {
      # 如果用户设置为none，默认使用majority
      combine_method <- "majority"
      log_message("自动启用多数投票策略")
    }

    log_message("组合策略：", combine_method)
    log_message("同时运行多种方法...")

    feature_sets <- list()

    # 1. Boruta
    log_message("1/4 运行Boruta...")
    result_boruta <- boruta_feature_selection(task, max_runs = 500)
    if (!is.null(result_boruta) && length(result_boruta$features) > 0) {
      feature_sets[["boruta"]] <- result_boruta$features
      all_results[["boruta"]] <- result_boruta
    }

    # 2. RFE
    log_message("2/4 运行RFE...")
    result_rfe <- rfe_feature_selection(task, n_features = opts$n_features)
    if (!is.null(result_rfe) && length(result_rfe$features) > 0) {
      feature_sets[["rfe"]] <- result_rfe$features
      all_results[["rfe"]] <- result_rfe
    }

    # 3. LASSO
    log_message("3/4 运行LASSO...")
    result_lasso <- lasso_feature_selection(task)
    if (!is.null(result_lasso) && length(result_lasso$features) > 0) {
      feature_sets[["lasso"]] <- result_lasso$features
      all_results[["lasso"]] <- result_lasso
    }

    # 4. XGBoost Importance
    log_message("4/4 运行XGBoost重要性...")
    result_xgb <- xgboost_importance_selection(task, top_n = opts$n_features)
    if (!is.null(result_xgb) && length(result_xgb$features) > 0) {
      feature_sets[["xgboost"]] <- result_xgb$features
      all_results[["xgboost"]] <- result_xgb
    }

    # 组合特征集
    log_message("组合", length(feature_sets), "个方法的结果...")
    if (length(feature_sets) > 0) {
      # 使用opts$n_features作为最大特征数限制
      max_features_limit <- opts$n_features * 2 # 允许最多2倍的目标特征数
      selected_features <- combine_feature_sets(feature_sets,
        method = combine_method,
        max_features = max_features_limit
      )
      method_used <- paste0("MultiMethod_", combine_method, "(", paste(names(feature_sets), collapse = "+"), ")")

      log_message("✓ 第二套方案成功，特征数量：", length(selected_features))
    }

    # 如果组合后没有特征，使用第一个成功方法的结果
    if (is.null(selected_features) || length(selected_features) == 0) {
      log_message("⚠️  组合后没有特征，使用第一个成功方法的结果", level = "WARNING")
      for (method_name in names(feature_sets)) {
        if (length(feature_sets[[method_name]]) > 0) {
          selected_features <- head(feature_sets[[method_name]], opts$n_features * 2)
          method_used <- paste0(method_name, "_fallback")
          log_message("使用", method_name, "的结果，特征数：", length(selected_features))
          break
        }
      }
    }
  }

  log_message("=== 特征选择完成 ===")
  log_message("最终方案：", ifelse(grepl("EFS", method_used), "第一套（EFS）", "第二套（多方法组合）"))
  log_message("最终方法：", method_used)
  log_message("特征数量：", length(selected_features))

  return(list(
    features = selected_features,
    method = method_used,
    all_results = all_results
  ))
}

# 03. 数据读取 --------------------------------------------------------------
if (redraw_only_mode) {
  redraw_saved_outputs()
  quit(save = "no", status = 0)
}

# 只有在全量运行或从检查点恢复且不是重绘模式时才执行训练代码
if ((opts$load_rdata == "none" || resume_training_mode) && !redraw_only_mode) {
  names_values <- NULL

  if (resume_training_mode) {
    log_message("恢复训练模式：重建数据与任务对象，并跳过EFS和特征选择")
  }

  # 检查是否加载预处理数据
  if (opts$load_preprocessed != "none") {
    cat("\n=== 加载预处理数据 ===\n")
    cat("预处理数据文件：", opts$load_preprocessed, "\n")

    if (!file.exists(opts$load_preprocessed)) {
      stop(paste0("指定的预处理数据文件不存在：", opts$load_preprocessed))
    }

    # 加载预处理数据
    load(opts$load_preprocessed)

    # 验证必需对象
    required_objects <- c("Data_preprocessed", "Gp_preprocessed", "preprocessing_metadata")
    missing_objects <- setdiff(required_objects, ls())
    if (length(missing_objects) > 0) {
      stop(paste0(
        "预处理数据文件格式不正确，缺少必需对象：",
        paste(missing_objects, collapse = ", ")
      ))
    }

    # 使用预处理数据
    Data <- Data_preprocessed
    Gp <- Gp_preprocessed
    names_values <- tibble(
      old_colnames = colnames(Data),
      new_colnames = c(
        sanitize_feature_ids(colnames(Data)[seq_len(ncol(Data) - 1)]),
        "group"
      )
    )
    colnames(Data) <- names_values$new_colnames

    # 从元数据中获取分组信息
    gp <- preprocessing_metadata$original_data_info$group_names

    # 显示预处理信息
    cat("\n预处理数据信息：\n")
    cat("  原始样本数：", preprocessing_metadata$original_data_info$n_samples, "\n")
    cat("  原始特征数：", preprocessing_metadata$original_data_info$n_features, "\n")
    cat("  预处理后样本数：", preprocessing_metadata$preprocessed_data_info$n_samples, "\n")
    cat("  预处理后特征数：", preprocessing_metadata$preprocessed_data_info$n_features, "\n")
    cat("  预处理配置：", preprocessing_metadata$config$preset, "\n")
    cat("  预处理时间：", preprocessing_metadata$timestamp, "\n")
    cat("  分组：", paste(gp, collapse = " vs "), "\n")
    cat("=========================\n\n")

    # 跳过原始数据加载和预处理步骤
    skip_data_loading <- TRUE
  } else {
    # 未提供预处理数据，使用原始流程
    skip_data_loading <- FALSE
  }

  # 原始数据加载流程（如果未加载预处理数据）
  if (!skip_data_loading) {
    mapcol <- c("#61d04f", "#df536b", "#377EB8", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
    Gp <- read_tsv(opts$map) %>%
      rename_all(~ c("SampleID", "group")) %>%
      mutate(group = fct_inorder(group))

    if (opts$gp != "none") {
      gp <- strsplit(opts$gp, split = "-")[[1]]
      if (length(gp) == 1) stop("Length of variable gp is equal to 1. Program terminated.")
      Gp <- Gp %>%
        filter(group %in% gp) %>%
        mutate(group = factor(group, levels = gp))
      if (length(unique(Gp$group)) == 1) stop("Length of variable Gp  is equal to 1. Program terminated.")
    } else {
      gp <- levels(Gp$group)
    }

    if (opts$color != "none") {
      sc <- read.table(opts$color, sep = "\t", comment.char = "", check.names = FALSE)
      sc <- sc[which(as.vector(sc[, 1]) %in% unique(unlist(Gp$group))), ]
      Gp$group <- factor(Gp$group, levels = as.vector(sc[, 1]))
      Gp <- Gp %>% arrange(group)
    } else {
      sc <- cbind(levels(Gp$group), mapcol[1:nlevels(Gp$group)]) %>%
        as.data.frame(stringsAsFactors = FALSE)
    }

    Data <- read_tsv(opts$input) %>% rename_at(1, ~"Features")
    if (isTRUE(opts$unif)) {
      Data <- Data %>% mutate(across(where(is.numeric), ~ . / sum(.)))
    }
    if (opts$select != "none") {
      ss <- read_tsv(opts$select, col_names = F) %>% rename_at(1, ~"Features")
      Data <- inner_join(Data, ss)
    }
    if (opts$delete != "none") {
      dd <- read_tsv(opts$delete, col_names = F) %>% rename_at(1, ~"Features")
      Data <- Data %>% filter(!Features %in% (dd %>% t() %>% as.character()))
    }
    Data <- Data %>%
      pivot_longer(!Features) %>%
      pivot_wider(names_from = "Features", values_from = "value") %>%
      rename_at(1, ~"SampleID") %>%
      inner_join(Gp)
    Data <- as.data.frame(Data)
    rownames(Data) <- Data$SampleID
    Data <- Data[, -1]

    names_values <- tibble(
      old_colnames = colnames(Data),
      new_colnames = c(paste0("feature_", seq(1, length(old_colnames) - 1)), "group")
    )
    colnames(Data) <- names_values$new_colnames
    Data$group <- factor(Data$group, levels = gp)
  } # 结束：if (!skip_data_loading)

  # 类别分布统计 ------------------------------------------------------------------
  cat("\n=== 类别分布统计 ===\n")
  class_counts <- table(Data$group)
  cat("训练集样本数：\n")
  print(class_counts)
  cat("\n")

  imbalance_ratio <- max(class_counts) / min(class_counts)
  cat("不平衡比例：", round(imbalance_ratio, 2), ":1\n")

  if (imbalance_ratio > 2 && imbalance_ratio <= 5) {
    log_message("⚠️  检测到轻度类别不平衡（2:1-5:1），建议使用 --use_class_weight TRUE", level = "WARNING")
  } else if (imbalance_ratio > 5 && imbalance_ratio <= 10) {
    log_message("⚠️  检测到中度类别不平衡（5:1-10:1），强烈建议使用 --use_class_weight TRUE", level = "WARNING")
  } else if (imbalance_ratio > 10) {
    log_message("⚠️  检测到严重类别不平衡（>10:1），强烈建议使用 --use_class_weight TRUE", level = "WARNING")
  } else {
    cat("✓ 类别分布相对平衡\n")
  }

  if (opts$use_class_weight) {
    cat("✓ 已启用类别权重处理\n")
  }
  cat("===================\n\n")

  # 计算类别权重（如果启用）
  class_weights <- NULL
  if (opts$use_class_weight) {
    if (opts$class_weight_method == "manual" && opts$manual_weights != "none") {
      # 解析手动指定的权重
      manual_w <- as.numeric(strsplit(opts$manual_weights, ",")[[1]])
      if (length(manual_w) != length(gp)) {
        stop("手动权重数量必须与类别数量一致")
      }
      names(manual_w) <- gp
      class_weights <- calculate_class_weights(Data, "group", method = "manual", manual_weights = manual_w)
    } else {
      class_weights <- calculate_class_weights(Data, "group", method = "balanced")
    }

    # 为每个样本分配权重（用于GLMNet的observation weights）
    Data$sample_weight <- ifelse(Data$group == gp[1],
      class_weights[gp[1]],
      class_weights[gp[2]]
    )
    cat("已添加样本权重列，用于GLMNet\n")
  }

  # 03. 建模准备 ------------------------------------------------------------------

  pbp_task <- TaskClassif$new(
    id = "classify_model",
    backend = Data,
    target = "group",
    positive = gp[1]
  )
  if ("stratum" %in% names(pbp_task$col_roles)) {
    pbp_task$set_col_roles("group", add_to = "stratum")
  }

  # 如果启用了类别权重，设置权重列的角色
  if (opts$use_class_weight && "sample_weight" %in% colnames(Data)) {
    pbp_task$set_col_roles("sample_weight", roles = "weights_learner")
    cat("已将sample_weight设置为学习器权重列，GLMNet将使用observation weights\n")
  }

  binary_check <- sapply(Data, is_binary)
  categorical_cols <- names(Data)[binary_check]
  continuous_cols <- names(Data)[!binary_check]

  pbp_prep <- po("removeconstants", ratio = 0.05) %>>%
    po("filter", filter = flt("variance"), filter.cutoff = 1e-9) %>>%
    po("imputehist", affect_columns = selector_name(continuous_cols)) %>>%
    po("imputemode", affect_columns = selector_name(categorical_cols))

  # 保存训练好的预处理管道供外部验证使用
  pbp_prep_trained <- pbp_prep$clone()
  task_prep <- pbp_prep_trained$train(pbp_task)[[1]]
  dim(task_prep$data())

  if (!isTRUE(opts$trainOnly)) {
    task_train <- task_prep$clone()
  } else {
    # 数据划分 - 使用预处理后的任务
    if (opts$split == "none") {
      # 只在没有提供 split 文件时才处理 opts$part
      if (opts$part == "none") {
        stop("当 trainOnly=TRUE 且未提供 split 文件时,必须指定 --part 参数(如 2/3 或 0.8)")
      }

      if (grepl("/", opts$part)) {
        aa <- strsplit(opts$part, "/")[[1]][1] %>% as.numeric()
        bb <- strsplit(opts$part, "/")[[1]][2] %>% as.numeric()
      } else {
        aa <- strsplit(MASS::fractions(opts$part %>% as.numeric()) %>% as.character(), "/")[[1]][1] %>% as.numeric()
        bb <- strsplit(MASS::fractions(opts$part %>% as.numeric()) %>% as.character(), "/")[[1]][2] %>% as.numeric()
      }
      zz <- aa / bb

      split_gp <- partition(task = pbp_task, ratio = zz)

      split_gp_pre <- bind_rows(map(split_gp, as_tibble), .id = "Split") %>%
        mutate(value = rownames(Data)[value]) %>%
        rename_at(2, ~"SampleID") %>%
        left_join(Data %>% select(group) %>% rownames_to_column(var = "SampleID") %>%
          mutate(group = as.character(group)), .) %>%
        write_tsv(file.path(opts$outdir, "split.map-group.txt"))
    } else {
      # 使用客户指定的 split 文件
      split_gp_pre <- Data %>%
        select(group) %>%
        rownames_to_column(var = "SampleID") %>%
        left_join(read_tsv(opts$split, col_names = c("SampleID", "group", "Split"), skip = 1))
      split_gp_pre0 <- split_gp_pre %>%
        mutate(group = factor(group, levels = gp)) %>%
        mutate(Split = factor(Split, levels = c("train", "test"))) %>%
        group_by(group) %>%
        nest() %>%
        mutate(
          data2 = map(data, ~ dplyr::filter(.x, Split == "train") %>% dplyr::select(SampleID)),
          data3 = map(data, ~ dplyr::filter(.x, Split == "test") %>% dplyr::select(SampleID))
        ) %>%
        mutate(
          train = map(data2, ~ factor(pull(.x, SampleID), levels = rownames(Data)) %>% as.numeric()),
          test = map(data3, ~ factor(pull(.x, SampleID), levels = rownames(Data)) %>% as.numeric())
        )
      split_gp <- list(train = unlist(split_gp_pre0$train), test = unlist(split_gp_pre0$test))
    }
    task_train <- task_prep$clone()$filter(split_gp$train)
    task_test <- task_prep$clone()$filter(split_gp$test)
  }
  if ("stratum" %in% names(task_train$col_roles)) {
    task_train$set_col_roles(task_train$target_names, add_to = "stratum")
  }

  # 样本量分析和LOOCV自动决策
  n_total <- nrow(Data)
  class_counts <- table(Data$group)
  n_minority <- min(class_counts)
  n_majority <- max(class_counts)
  imbalance_ratio <- n_majority / n_minority

  cat("\n=== 数据集分析 ===\n")
  cat("总样本数：", n_total, "\n")
  cat("多数类样本数：", n_majority, "（", names(class_counts)[which.max(class_counts)], "）\n")
  cat("少数类样本数：", n_minority, "（", names(class_counts)[which.min(class_counts)], "）\n")
  cat("不平衡比例：", round(imbalance_ratio, 2), ":1\n")

  # LOOCV决策逻辑
  use_loocv_final <- FALSE
  loocv_scope_final <- "outer"

  if (opts$use_loocv) {
    # 用户手动启用
    use_loocv_final <- TRUE
    loocv_scope_final <- opts$loocv_scope
    cat("\n✓ 用户手动启用LOOCV，范围：", loocv_scope_final, "\n")
  } else if (opts$auto_loocv) {
    # 自动决策（优先级从高到低）
    cat("\n=== 自动决策LOOCV策略 ===\n")

    # 1️⃣ 少数类极少（<10）→ 必须全面LOOCV
    if (n_minority < 10) {
      use_loocv_final <- TRUE
      loocv_scope_final <- "full"
      cat("决策依据：少数类样本极少（", n_minority, " < 10）\n")
      cat("推荐策略：全面LOOCV（full）\n")
      cat("原因：少数类样本太少，必须最大化利用每个样本\n")

      # 2️⃣ 少数类很少（<15）且不平衡（>5:1）→ 内外层LOOCV
    } else if (n_minority < 15 && imbalance_ratio > 5) {
      use_loocv_final <- TRUE
      loocv_scope_final <- "inner"
      cat(
        "决策依据：少数类样本很少（", n_minority, " < 15）且不平衡（",
        round(imbalance_ratio, 2), ":1 > 5:1）\n"
      )
      cat("推荐策略：内外层LOOCV（inner）\n")
      cat("原因：少数类少+不平衡，需要充分的交叉验证\n")

      # 3️⃣ 少数类较少（<20）→ 仅外层LOOCV
    } else if (n_minority < 20) {
      use_loocv_final <- TRUE
      loocv_scope_final <- "outer"
      cat("决策依据：少数类样本较少（", n_minority, " < 20）\n")
      cat("推荐策略：仅外层LOOCV（outer）\n")
      cat("原因：少数类样本有限，模型评估需要LOOCV\n")

      # 4️⃣ 总样本少（<50）→ 仅外层LOOCV
    } else if (n_total < 50) {
      use_loocv_final <- TRUE
      loocv_scope_final <- "outer"
      cat("决策依据：总样本数较少（", n_total, " < 50）\n")
      cat("推荐策略：仅外层LOOCV（outer）\n")
      cat("原因：总样本量小，需要充分利用数据\n")

      # 5️⃣ 总样本中等（<threshold）→ 仅外层LOOCV
    } else if (n_total < opts$loocv_threshold) {
      use_loocv_final <- TRUE
      loocv_scope_final <- "outer"
      cat("决策依据：总样本数中等（", n_total, " < ", opts$loocv_threshold, "）\n")
      cat("推荐策略：仅外层LOOCV（outer）\n")
      cat("原因：样本量有限，建议使用LOOCV提高评估准确性\n")

      # 6️⃣ 样本充足 → 不使用LOOCV
    } else {
      cat("决策依据：样本量充足（总样本", n_total, "，少数类", n_minority, "）\n")
      cat("推荐策略：常规交叉验证（5-fold CV）\n")
      cat("原因：样本量充足，不需要LOOCV\n")
    }

    cat("\n")
  }

  # 应用resampling策略
  if (use_loocv_final) {
    cat("=== 应用LOOCV策略 ===\n")
    cat("LOOCV范围：", loocv_scope_final, "\n")

    # 估算模型训练阶段的基础拟合次数：外层评估 × 随机搜索评估 × 内层CV × 算法数。
    # 旧估算未乘 tune_evals，遇到 LOOCV + AutoTuner 时会严重低估耗时。
    outer_iterations <- task_train$nrow
    if (loocv_scope_final == "outer") {
      inner_iterations <- opts$inner_cv
    } else if (loocv_scope_final == "inner") {
      inner_iterations <- max(1L, task_train$nrow - 1L)
    } else { # full
      inner_iterations <- max(1L, task_train$nrow - 1L)
    }
    estimated_trains <- length(selected_model_names) * outer_iterations * (opts$tune_evals * inner_iterations + 1)
    cat("预计训练次数：约", estimated_trains, "次\n")
    cat("预计耗时：", round(estimated_trains / 60, 1), "-", round(estimated_trains / 30, 1), "分钟\n\n")

    # 设置resampling策略
    if (loocv_scope_final == "outer") {
      # 策略A：仅外层LOOCV
      init_resampling <- rsmp("bootstrap", repeats = opts$resample, ratio = 1)
      inner_resampling <- rsmp("cv", folds = opts$inner_cv)
      outer_resampling <- rsmp("loo")
    } else if (loocv_scope_final == "inner") {
      # 策略B：内外层LOOCV
      init_resampling <- rsmp("bootstrap", repeats = 3, ratio = 1)
      inner_resampling <- rsmp("loo")
      outer_resampling <- rsmp("loo")
    } else if (loocv_scope_final == "full") {
      # 策略C：全面LOOCV
      init_resampling <- rsmp("bootstrap", repeats = 2, ratio = 1)
      inner_resampling <- rsmp("loo")
      outer_resampling <- rsmp("loo")
    }
  } else {
    # 常规策略
    cat("=== 使用常规交叉验证 ===\n")
    cat("内层CV：", opts$inner_cv, "-fold\n")
    cat("外层CV：", opts$outer_cv, "-fold\n\n")

    inner_resampling <- rsmp("cv", folds = opts$inner_cv)
    outer_resampling <- rsmp("cv", folds = opts$outer_cv)
    init_resampling <- rsmp("subsampling", repeats = opts$resample, ratio = 0.8)
    estimated_trains <- length(selected_model_names) * opts$outer_cv * (opts$tune_evals * opts$inner_cv + 1)
    cat("预计模型训练阶段基础拟合次数：约", estimated_trains, "次\n\n")
  }

  if (isTRUE(opts$use_smote)) {
    cat("=== 启用SMOTE训练折过采样 ===\n")
    cat("SMOTE仅在后续模型训练的训练折内部生效，不作用于测试折/验证集\n")
    cat("EFS特征筛选阶段保持原生学习器，以兼容RFE与svm_rfe\n")
    cat("SMOTE参数：K =", opts$smote_k, ", dup_size =", opts$smote_dup_size, "\n\n")
  }

  rfe <- fs("rfe", n_features = opts$n_features, feature_fraction = opts$feature_fraction)

  max_nrounds <- 500

  # 创建学习器（应用class weight如果启用）
  if (opts$use_class_weight && !is.null(class_weights)) {
    # 计算scale_pos_weight（用于XGBoost和LightGBM）
    scale_pos_weight <- class_weights[gp[2]] / class_weights[gp[1]]

    cat("\n应用类别权重到学习器：\n")
    cat("XGBoost scale_pos_weight:", round(scale_pos_weight, 4), "\n")
    cat("LightGBM is_unbalance: TRUE\n")
    cat("SVM class.weights:", paste(round(class_weights, 4), collapse = ", "), "\n")
    cat("Random Forest class.weights:", paste(round(class_weights, 4), collapse = ", "), "\n\n")

    learners_efs <- make_efs_learners(max_nrounds, opts, class_weights)

    # Benchmark专用学习器（包含所有算法）
    # 创建RF和SVM learners，使用$set_values()设置含点号的参数
    rf_bench <- lrn("classif.ranger",
      id = "rf",
      importance = "permutation"
    )
    rf_bench$use_weights <- "ignore"
    rf_bench$param_set$values$class.weights <- class_weights

    svm_bench <- lrn("classif.svm",
      id = "svm",
      type = "C-classification",
      kernel = "linear"
    )
    svm_bench$use_weights <- "ignore"
    svm_bench$param_set$values$class.weights <- class_weights

    xgb_bench <- lrn("classif.xgboost",
      id = "xgb", nrounds = max_nrounds,
      early_stopping_rounds = 20,
      scale_pos_weight = scale_pos_weight
    )
    xgb_bench$use_weights <- "ignore"

    nb_bench <- lrn("classif.naive_bayes", id = "nb", predict_type = "prob")
    nb_bench$use_weights <- "ignore"
    knn_bench <- lrn("classif.kknn", id = "knn", predict_type = "prob", k = 5)
    knn_bench$use_weights <- "ignore"
    tree_bench <- lrn("classif.rpart", id = "tree", predict_type = "prob")
    tree_bench$use_weights <- "ignore"
    adaboost_bench <- lrn("classif.AdaBoostM1", id = "adaboost", predict_type = "prob")
    adaboost_bench$use_weights <- "ignore"
    nnet_bench <- lrn("classif.nnet", id = "nnet", predict_type = "prob", size = 10, MaxNWts = 5000)
    nnet_bench$use_weights <- "ignore"

    learners <- list(
      xgb_bench,
      rf_bench,
      svm_bench,
      # Benchmark最终模型集固定为9种候选分类器，lightgbm仅用于EFS层诊断/筛选。
      # lrn("classif.lightgbm",
      #   id = "lgbm",
      #   num_iterations = max_nrounds,
      #   learning_rate = 0.05,
      #   num_leaves = 31,
      #   objective = "binary",
      #   min_data_in_leaf = 20,
      #   feature_fraction = 0.8,
      #   bagging_fraction = 0.8,
      #   bagging_freq = 5,
      #   is_unbalance = TRUE
      # ),
      # 新增算法（不支持importance，不参与EFS）
      nb_bench,
      knn_bench,
      tree_bench,
      adaboost_bench,
      nnet_bench
    )
  } else {
    learners_efs <- make_efs_learners(max_nrounds, opts, NULL)

    # Benchmark专用学习器（包含所有算法）
    learners <- list(
      lrn("classif.xgboost",
        id = "xgb", nrounds = max_nrounds,
        early_stopping_rounds = 20
      ),
      lrn("classif.ranger", id = "rf", importance = "permutation"),
      lrn("classif.svm", id = "svm", type = "C-classification", kernel = "linear"),
      # Benchmark最终模型集固定为9种候选分类器，lightgbm仅用于EFS层诊断/筛选。
      # lrn("classif.lightgbm",
      #   id = "lgbm",
      #   num_iterations = max_nrounds,
      #   learning_rate = 0.05,
      #   num_leaves = 31,
      #   objective = "binary",
      #   min_data_in_leaf = 20,
      #   feature_fraction = 0.8,
      #   bagging_fraction = 0.8,
      #   bagging_freq = 5
      # ),
      # 新增算法（不支持importance，不参与EFS）
      lrn("classif.naive_bayes", id = "nb", predict_type = "prob"),
      lrn("classif.kknn", id = "knn", predict_type = "prob", k = 5),
      lrn("classif.rpart", id = "tree", predict_type = "prob"),
      lrn("classif.AdaBoostM1", id = "adaboost", predict_type = "prob"),
      lrn("classif.nnet", id = "nnet", predict_type = "prob", size = 10, MaxNWts = 5000)
    )
  }

  # 自适应小样本处理配置函数
  adaptive_small_sample_config <- function(task, n_threshold = 100, opts) {
    n_samples <- task$nrow
    n_features <- length(task$feature_names)
    class_counts <- table(task$data()[[task$target_names]])
    n_minority <- min(class_counts)
    efs_init_repeats <- max(1L, as.integer(opts$resample))

    cat("\n=== 自适应配置分析 ===\n")
    cat("总样本数：", n_samples, "\n")
    cat("特征数：", n_features, "\n")
    cat("少数类样本数：", n_minority, "\n")
    cat("EFS初始重采样次数：", efs_init_repeats, "\n")

    config <- list()

    # 1. 决定重采样策略
    if (n_minority < 10) {
      cat("策略：极小样本（<10），使用全面LOOCV\n")
      config$init_resampling <- rsmp("bootstrap", repeats = efs_init_repeats, ratio = 1)
      config$inner_resampling <- rsmp("loo")
      config$outer_resampling <- rsmp("loo")
      config$use_loocv <- TRUE
      config$loocv_scope <- "full"
    } else if (n_minority < 20) {
      cat("策略：很小样本（<20），使用内外层LOOCV\n")
      config$init_resampling <- rsmp("bootstrap", repeats = efs_init_repeats, ratio = 1)
      config$inner_resampling <- rsmp("loo")
      config$outer_resampling <- rsmp("loo")
      config$use_loocv <- TRUE
      config$loocv_scope <- "inner"
    } else if (n_samples < n_threshold) {
      cat("策略：小样本（<", n_threshold, "），使用外层LOOCV\n")
      config$init_resampling <- rsmp("subsampling", repeats = 30, ratio = 0.8)
      config$inner_resampling <- rsmp("cv", folds = opts$inner_cv)
      config$outer_resampling <- rsmp("loo")
      config$use_loocv <- TRUE
      config$loocv_scope <- "outer"
    } else {
      cat("策略：正常样本，使用标准交叉验证\n")
      config$init_resampling <- rsmp("subsampling", repeats = opts$resample, ratio = 0.8)
      config$inner_resampling <- rsmp("cv", folds = opts$inner_cv)
      config$outer_resampling <- rsmp("cv", folds = opts$outer_cv)
      config$use_loocv <- FALSE
      config$loocv_scope <- "none"
    }

    # 2. 调整特征选择参数
    if (n_samples < 50) {
      config$n_features <- min(opts$n_features, floor(n_samples / 5))
      config$feature_fraction <- 0.9 # 更保守
      cat("调整特征数上限：", config$n_features, "\n")
    } else {
      config$n_features <- opts$n_features
      config$feature_fraction <- opts$feature_fraction
    }

    cat("===================\n\n")
    return(config)
  }

  # 安全包装的EFS调用函数
  safe_ensemble_fselect <- function(...) {
    tryCatch(
      {
        log_message("=== 开始EFS特征选择 ===")

        start_time <- Sys.time()

        result <- with_progress({
          ensemble_fselect(...)
        })

        end_time <- Sys.time()
        elapsed_time <- difftime(end_time, start_time, units = "mins")

        log_message("✓ EFS完成，耗时：", round(elapsed_time, 2), "分钟")

        return(result)
      },
      error = function(e) {
        log_message("❌ EFS运行失败", level = "ERROR")
        log_message("错误信息：", e$message, level = "ERROR")
        log_message("错误位置：", deparse(e$call), level = "ERROR")

        # 保存错误日志
        error_log <- list(
          timestamp = Sys.time(),
          error_message = e$message,
          error_call = deparse(e$call),
          traceback = traceback()
        )
        saveRDS(error_log, file.path(opts$outdir, "efs_error_log.rds"))

        log_message("错误日志已保存到：efs_error_log.rds", level = "ERROR")

        stop("EFS运行失败，请检查错误日志")
      }
    )
  }

  svm_rfe <- clbk("mlr3fselect.svm_rfe")
  one_se_clbk <- clbk("mlr3fselect.one_se_rule")

  # 早停回调函数（注释掉，使用terminator代替）
  # early_stopping_clbk <- callback_fselect("early_stopping",
  #     on_eval_after = function(callback, context) {
  #         # 获取当前性能
  #         current_performance <- context$instance$archive$data$classif.acc
  #
  #         # 如果最近10次迭代性能没有提升超过0.001，则停止
  #         if (length(current_performance) > 10) {
  #             recent_performance <- tail(current_performance, 10)
  #             if (max(recent_performance) - min(recent_performance) < 0.001) {
  #                 cat("早停：最近10次迭代性能提升<0.001\n")
  #                 context$instance$terminate()
  #             }
  #         }
  #     }
  # )

  callbacks <- list(
    xgb  = list(one_se_clbk),
    rf   = list(one_se_clbk),
    svm  = list(one_se_clbk, svm_rfe),
    lgbm = list(one_se_clbk)
  )

  if (isTRUE(opts$use_smote)) {
    # SMOTE只包裹最终建模学习器；EFS阶段仍使用原生学习器。
    # 因此中低维任务可继续保留svm_rfe；高维任务会在后面按特征数排除SVM-RFE。
    log_message("⚠️  SMOTE模式下EFS使用原生学习器；高维任务将自动排除SVM-RFE", level = "WARNING")
  }

  if (!resume_training_mode) {
  # 04. EFS特征选择 --------------------------------------------------------------

  # EFS阶段不使用task-level observation weights。
  # 否则会触发不支持weights的learner（如svm）在AutoFSelector中报错。
  # 类别权重在EFS里仍通过各learner自己的class.weights参数生效。
  task_train_efs <- task_train$clone()
  if (opts$use_class_weight && "sample_weight" %in% task_train_efs$col_roles$weights_learner) {
    task_train_efs$set_col_roles("sample_weight", remove_from = "weights_learner")
  }

  if (length(task_train_efs$feature_names) <= opts$n_features) {
    selected_features <- task_train_efs$feature_names
    method_used <- paste0("All_features_low_dimensional(n=", length(selected_features), ")")
    n_features <- length(selected_features)
    detection_method <- "not_applicable_low_dimensional_all_features"
    efs <- NULL
    feature_importance_stats <- NULL
    tradeoff_stats <- NULL
    log_message(
      "特征数不超过 --n_features，跳过EFS并使用全部特征：",
      length(selected_features),
      "个"
    )

    cat("\n=== 特征选择最终结果 ===\n")
    cat("最终选择的特征数：", length(selected_features), "\n")
    cat("选择方法：", method_used, "\n")
    cat("====================\n\n")
  } else {
  smote_svm_rfe_feature_threshold <- 500L
  smote_high_dim_efs <- FALSE
  if (isTRUE(opts$use_smote) && length(task_train_efs$feature_names) > smote_svm_rfe_feature_threshold) {
    keep_efs <- !vapply(learners_efs, function(x) identical(x$id, "svm"), logical(1))
    if (any(!keep_efs)) {
      learners_efs <- learners_efs[keep_efs]
      callbacks <- callbacks[names(callbacks) %in% vapply(learners_efs, function(x) x$id, character(1))]
      smote_high_dim_efs <- TRUE
      log_message(
        "⚠️  SMOTE高维EFS特征数 ",
        length(task_train_efs$feature_names),
        " > ",
        smote_svm_rfe_feature_threshold,
        "，排除svm_rfe；保留EFS学习器：",
        paste(vapply(learners_efs, function(x) x$id, character(1)), collapse = ","),
        level = "WARNING"
      )
    }
  }

  # 使用自适应小样本配置
  adaptive_config <- adaptive_small_sample_config(task_train_efs, n_threshold = 100, opts = opts)

  # 应用自适应配置
  init_resampling_adaptive <- adaptive_config$init_resampling
  inner_resampling_adaptive <- adaptive_config$inner_resampling
  if (isTRUE(smote_high_dim_efs)) {
    init_resampling_adaptive <- rsmp("bootstrap", repeats = min(3L, max(1L, as.integer(opts$resample))), ratio = 1)
    inner_resampling_adaptive <- rsmp("cv", folds = max(2L, min(as.integer(opts$inner_cv), 3L)))
    log_message("⚠️  SMOTE高维EFS使用轻量重采样：bootstrap repeats=", init_resampling_adaptive$param_set$values$repeats,
      ", inner CV folds=", inner_resampling_adaptive$param_set$values$folds, level = "WARNING")
  }
  rfe_adaptive <- fs("rfe",
    n_features = adaptive_config$n_features,
    feature_fraction = adaptive_config$feature_fraction
  )

  # 调试：检查learners_efs中的每个学习器
  cat("\n=== 调试：检查学习器参数 ===\n")
  for (i in seq_along(learners_efs)) {
    learner <- learners_efs[[i]]
    cat(sprintf("学习器 %d: %s\n", i, learner$id))
    tryCatch(
      {
        param_ids <- learner$param_set$ids()
        cat(sprintf("  参数数量: %d\n", length(param_ids)))
        cat(sprintf("  参数名: %s\n", paste(head(param_ids, 10), collapse = ", ")))
      },
      error = function(e) {
        cat(sprintf("  ✗ 获取参数失败: %s\n", e$message))
      }
    )
  }
  cat("================================\n\n")

  # 使用安全包装的EFS调用
  if (isTRUE(smote_high_dim_efs)) {
    log_message("⚠️  SMOTE高维分支跳过ensemble EFS，直接进入第二套多方法特征选择", level = "WARNING")
    efs <- NULL
  } else {
    efs <- safe_ensemble_fselect(
      fselector = rfe_adaptive,
      task = task_train_efs,
      learners = learners_efs, # 使用EFS专用学习器（只包含支持importance的）
      init_resampling = init_resampling_adaptive,
      inner_resampling = inner_resampling_adaptive,
      inner_measure = msr("classif.ce"),
      measure = msr("classif.acc"),
      terminator = trm("none"),
      callbacks = callbacks,
      store_benchmark_result = TRUE,
      store_models = FALSE # 节省内存
    )
  }

  if (!is.null(efs)) {
  print(efs)
  efs$result
  efs$active_measure
  efs$measure

  p1 <- autoplot(efs, type = "performance", theme = theme_minimal(base_size = 14, base_family = plot_font_family)) +
    scale_fill_brewer(palette = "Set1")
  save_pdf_plot(file.path(opts$outdir, "step1.1.efs.performance.pdf"), p1, width = 12.5, height = 12, units = "in", dpi = 300)

  p2 <- autoplot(efs, type = "n_features", theme = theme_minimal(base_size = 14, base_family = plot_font_family)) +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(breaks = seq(0, 60, 10))
  save_pdf_plot(file.path(opts$outdir, "step1.2.efs.n_features.pdf"), p2, width = 12.5, height = 12, units = "in", dpi = 300)

  p3 <- autoplot(efs, type = "pareto", theme = theme_minimal(base_size = 14, base_family = plot_font_family)) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Empirical Pareto front")
  save_pdf_plot(file.path(opts$outdir, "step1.3.efs.pareto.pdf"), p3, width = 12.5, height = 12, units = "in", dpi = 300)

  p4 <- autoplot(efs,
    type = "pareto", pareto_front = "estimated",
    theme = theme_minimal(base_size = 14, base_family = plot_font_family)
  ) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Estimated Pareto front")
  save_pdf_plot(file.path(opts$outdir, "step1.4.efs.pareto_estimated.pdf"), p4, width = 12.5, height = 12, units = "in", dpi = 300)

  p5 <- autoplot(efs, type = "stability", theme = theme_minimal(base_size = 14, base_family = plot_font_family)) +
    scale_fill_brewer(palette = "Set1")
  save_pdf_plot(file.path(opts$outdir, "step1.5.efs.stability.pdf"), p5, width = 12.5, height = 12, units = "in", dpi = 300)

  efs$stability(stability_measure = "jaccard", global = TRUE)
  efs$stability(stability_measure = "jaccard", global = FALSE)
  efs$knee_points(type = "estimated")
  efs$knee_points(type = "empirical")

  # 使用改进的膝点检测函数（5层容错机制）
  knee_result <- improved_knee_detection(efs, default_n = opts$n_features)
  n_features <- knee_result$n_features
  detection_method <- knee_result$method

  cat("\n=== 膝点检测结果 ===\n")
  cat("检测方法：", detection_method, "\n")
  cat("选择的特征数：", n_features, "\n")
  cat("===================\n\n")

  # 使用新的多方法特征选择系统
  fs_result <- multi_method_feature_selection(
    task = task_train,
    opts = opts,
    efs_result = efs,
    knee_result = knee_result
  )

  selected_features <- fs_result$features
  method_used <- fs_result$method

  # 如果所有方法都失败，使用EFS ranking作为最后备选
  if (is.null(selected_features) || length(selected_features) == 0) {
    log_message("⚠️  所有特征选择方法都失败，使用EFS ranking作为最后备选", level = "WARNING")
    res <- efs$feature_ranking(method = "sav", committee_size = n_features)
    selected_features <- res$feature
    method_used <- "EFS_ranking_fallback"
  }

  cat("\n=== 特征选择最终结果 ===\n")
  cat("最终选择的特征数：", length(selected_features), "\n")
  cat("选择方法：", method_used, "\n")
  cat("====================\n\n")

  # 运行特征重要性聚合分析
  feature_importance_stats <- aggregate_feature_importance(efs, selected_features, opts$outdir)

  # 运行性能-复杂度权衡分析
  tradeoff_stats <- performance_complexity_tradeoff(efs, length(selected_features), opts$outdir)

  } else {
    knee_result <- NULL
    detection_method <- "not_applicable_smote_high_dimensional"
    fs_result <- multi_method_feature_selection(
      task = task_train,
      opts = opts,
      efs_result = NULL,
      knee_result = NULL
    )

    selected_features <- fs_result$features
    method_used <- fs_result$method

    if (is.null(selected_features) || length(selected_features) == 0) {
      log_message("⚠️  第二套多方法特征选择失败，使用全部特征作为最后备选", level = "WARNING")
      selected_features <- task_train_efs$feature_names
      method_used <- "All_features_fallback_after_efs_skip"
    }

    cat("\n=== 特征选择最终结果 ===\n")
    cat("最终选择的特征数：", length(selected_features), "\n")
    cat("选择方法：", method_used, "\n")
    cat("====================\n\n")

    feature_importance_stats <- NULL
    tradeoff_stats <- NULL
  }

  }
  }

  if (resume_training_mode) {
    log_message("已从检查点恢复特征选择结果：", method_used, "；特征数量：", length(selected_features))
  }

  # 05. 模型训练 --------------------------------------------------------------

  task_train_selected <- task_train$clone()$select(selected_features)
  task_test_selected <- task_test$clone()$select(selected_features)
  rf_mtry_upper <- max(1L, min(20L, length(selected_features)))

  prefix_tag <- function(model_id) {
    if (isTRUE(opts$use_smote)) paste0(model_id, ".") else ""
  }

  model_params <- list(
    glmnet = make_glmnet_search_space(prefix_tag("glmnet")),
    xgboost = make_xgboost_search_space(prefix_tag("xgboost")),
    rf = make_rf_search_space(prefix_tag("rf")),
    svm = make_svm_search_space(prefix_tag("svm")),
    naive_bayes = make_naive_bayes_search_space(prefix_tag("naive_bayes")),
    kknn = make_kknn_search_space(prefix_tag("kknn")),
    rpart = make_rpart_search_space(prefix_tag("rpart")),
    adaboost = make_adaboost_search_space(prefix_tag("adaboost")),
    nnet = make_nnet_search_space(prefix_tag("nnet"))
  )

  # 创建基础学习器（用于超参数调优）
  if (opts$use_class_weight && !is.null(class_weights)) {
    scale_pos_weight <- class_weights[gp[2]] / class_weights[gp[1]]

    # 注意：GLMNet通过observation weights实现类别权重
    # 已在数据准备阶段添加sample_weight列，并设置为任务的权重列
    # mlr3会自动将权重传递给GLMNet的weights参数
    learner_base_glmnet <- lrn("classif.glmnet",
      id = "glmnet",
      lambda = 0.1, alpha = 0.5,
      predict_type = "prob"
    )
    learner_base_xgb <- lrn("classif.xgboost",
      id = "xgboost",
      predict_type = "prob",
      nrounds = 100, eta = 0.1, max_depth = 6,
      scale_pos_weight = scale_pos_weight
    )
    learner_base_xgb$use_weights <- "ignore"
    learner_base_rf <- lrn("classif.ranger",
      id = "rf",
      predict_type = "prob",
      importance = "impurity"
    )
    learner_base_rf$use_weights <- "ignore"
    learner_base_rf$param_set$values$num.trees <- 500
    learner_base_rf$param_set$values$class.weights <- class_weights

    learner_base_svm <- lrn("classif.svm",
      id = "svm",
      predict_type = "prob",
      type = "C-classification",
      kernel = "radial", cost = 1
    )
    learner_base_svm$use_weights <- "ignore"
    learner_base_svm$param_set$values$class.weights <- class_weights

    # Benchmark最终模型集固定为9种候选分类器，lightgbm仅用于EFS层诊断/筛选。
    # learner_base_lightgbm <- lrn("classif.lightgbm",
    #   is_unbalance = TRUE
    # )
    # 新增算法的基础学习器
    learner_base_nb <- lrn("classif.naive_bayes", id = "naive_bayes", predict_type = "prob")
    learner_base_nb$use_weights <- "ignore"
    learner_base_knn <- lrn("classif.kknn", id = "kknn", predict_type = "prob", k = 5)
    learner_base_knn$use_weights <- "ignore"
    learner_base_tree <- lrn("classif.rpart", id = "rpart", predict_type = "prob")
    learner_base_tree$use_weights <- "ignore"
    learner_base_adaboost <- lrn("classif.AdaBoostM1", id = "adaboost", predict_type = "prob")
    learner_base_adaboost$use_weights <- "ignore"
    learner_base_nnet <- lrn("classif.nnet", id = "nnet", predict_type = "prob", size = 10, MaxNWts = 5000)
    learner_base_nnet$use_weights <- "ignore"
  } else {
    learner_base_glmnet <- lrn("classif.glmnet",
      id = "glmnet",
      lambda = 0.1, alpha = 0.5,
      predict_type = "prob"
    )
    learner_base_xgb <- lrn("classif.xgboost",
      id = "xgboost",
      predict_type = "prob",
      nrounds = 100, eta = 0.1, max_depth = 6
    )
    learner_base_rf <- lrn("classif.ranger",
      id = "rf",
      predict_type = "prob",
      importance = "impurity"
    )
    learner_base_rf$param_set$values$num.trees <- 500

    learner_base_svm <- lrn("classif.svm",
      id = "svm",
      predict_type = "prob",
      type = "C-classification",
      kernel = "radial", cost = 1
    )
    # Benchmark最终模型集固定为9种候选分类器，lightgbm仅用于EFS层诊断/筛选。
    # learner_base_lightgbm <- lrn("classif.lightgbm")
    # 新增算法的基础学习器
    learner_base_nb <- lrn("classif.naive_bayes", id = "naive_bayes", predict_type = "prob")
    learner_base_knn <- lrn("classif.kknn", id = "kknn", predict_type = "prob", k = 5)
    learner_base_tree <- lrn("classif.rpart", id = "rpart", predict_type = "prob")
    learner_base_adaboost <- lrn("classif.AdaBoostM1", id = "adaboost", predict_type = "prob")
    learner_base_nnet <- lrn("classif.nnet", id = "nnet", predict_type = "prob", size = 10, MaxNWts = 5000)
  }

  learner_base_glmnet <- wrap_learner_with_optional_smote(learner_base_glmnet, "glmnet", opts)
  learner_base_xgb <- wrap_learner_with_optional_smote(learner_base_xgb, "xgboost", opts)
  learner_base_rf <- wrap_learner_with_optional_smote(learner_base_rf, "rf", opts)
  learner_base_svm <- wrap_learner_with_optional_smote(learner_base_svm, "svm", opts)
  learner_base_nb <- wrap_learner_with_optional_smote(learner_base_nb, "naive_bayes", opts)
  learner_base_knn <- wrap_learner_with_optional_smote(learner_base_knn, "kknn", opts)
  learner_base_tree <- wrap_learner_with_optional_smote(learner_base_tree, "rpart", opts)
  learner_base_adaboost <- wrap_learner_with_optional_smote(learner_base_adaboost, "adaboost", opts)
  learner_base_nnet <- wrap_learner_with_optional_smote(learner_base_nnet, "nnet", opts)

  terminator <- trm("combo",
    terminators = list(
      trm("stagnation", iters = 10, threshold = 0.005),
      trm("evals", n_evals = opts$tune_evals),
      trm("run_time", secs = 3600),
      trm("perf_reached", level = 0.85)
    )
  )
  random_search_tuner <- mlr3tuning::tnr("random_search", batch_size = min(opts$tune_batch_size, opts$tune_evals))

  set_mlr3_log_level(opts$mlr3_log_level)

  learner_base_glmnet$encapsulate(
    method = "evaluate",
    fallback = lrn("classif.featureless", predict_type = "prob")
  )
  learner_base_xgb$encapsulate(
    method = "evaluate",
    fallback = lrn("classif.featureless", predict_type = "prob")
  )
  learner_base_rf$encapsulate(
    method = "evaluate",
    fallback = lrn("classif.featureless", predict_type = "prob")
  )
  learner_base_svm$encapsulate(
    method = "evaluate",
    fallback = lrn("classif.featureless", predict_type = "prob")
  )
  # 新增算法的encapsulate
  learner_base_nb$encapsulate(
    method = "evaluate",
    fallback = lrn("classif.featureless", predict_type = "prob")
  )
  learner_base_knn$encapsulate(
    method = "evaluate",
    fallback = lrn("classif.featureless", predict_type = "prob")
  )
  learner_base_tree$encapsulate(
    method = "evaluate",
    fallback = lrn("classif.featureless", predict_type = "prob")
  )
  learner_base_adaboost$encapsulate(
    method = "evaluate",
    fallback = lrn("classif.featureless", predict_type = "prob")
  )
  learner_base_nnet$encapsulate(
    method = "evaluate",
    fallback = lrn("classif.featureless", predict_type = "prob")
  )

  auto_tuners <- list(
    glmnet = auto_tuner(
      tuner = random_search_tuner,
      learner = learner_base_glmnet,
      resampling = inner_resampling,
      measure = msr("classif.ce"),
      search_space = model_params$glmnet,
      terminator = terminator,
      store_tuning_instance = TRUE
    ),
    xgboost = auto_tuner(
      tuner = random_search_tuner,
      learner = learner_base_xgb,
      resampling = inner_resampling,
      measure = msr("classif.ce"),
      search_space = model_params$xgboost,
      terminator = terminator,
      store_tuning_instance = TRUE
    ),
    rf = auto_tuner(
      tuner = random_search_tuner,
      learner = learner_base_rf,
      resampling = inner_resampling,
      measure = msr("classif.ce"),
      search_space = model_params$rf,
      terminator = terminator,
      store_tuning_instance = TRUE
    ),
    svm = auto_tuner(
      tuner = random_search_tuner,
      learner = learner_base_svm,
      resampling = inner_resampling,
      measure = msr("classif.ce"),
      search_space = model_params$svm,
      terminator = terminator,
      store_tuning_instance = TRUE
    ),
    # 新增算法的auto_tuners
    naive_bayes = auto_tuner(
      tuner = random_search_tuner,
      learner = learner_base_nb,
      resampling = inner_resampling,
      measure = msr("classif.ce"),
      search_space = model_params$naive_bayes,
      terminator = terminator,
      store_tuning_instance = TRUE
    ),
    kknn = auto_tuner(
      tuner = random_search_tuner,
      learner = learner_base_knn,
      resampling = inner_resampling,
      measure = msr("classif.ce"),
      search_space = model_params$kknn,
      terminator = terminator,
      store_tuning_instance = TRUE
    ),
    rpart = auto_tuner(
      tuner = random_search_tuner,
      learner = learner_base_tree,
      resampling = inner_resampling,
      measure = msr("classif.ce"),
      search_space = model_params$rpart,
      terminator = terminator,
      store_tuning_instance = TRUE
    ),
    adaboost = auto_tuner(
      tuner = random_search_tuner,
      learner = learner_base_adaboost,
      resampling = inner_resampling,
      measure = msr("classif.ce"),
      search_space = model_params$adaboost,
      terminator = terminator,
      store_tuning_instance = TRUE
    ),
    nnet = auto_tuner(
      tuner = random_search_tuner,
      learner = learner_base_nnet,
      resampling = inner_resampling,
      measure = msr("classif.ce"),
      search_space = model_params$nnet,
      terminator = terminator,
      store_tuning_instance = TRUE
    )
  )

  auto_tuners <- auto_tuners[selected_model_names]

  if (opts$use_class_weight) {
    # glmnet保留task-level observation weights，其余算法走各自的class.weights/scale_pos_weight，
    # 因此需要让AutoTuner这一层也忽略task权重，避免wrapper先行报错。
    for (learner_name in intersect(c("xgboost", "rf", "svm", "naive_bayes", "kknn", "rpart", "adaboost", "nnet"), names(auto_tuners))) {
      auto_tuners[[learner_name]]$use_weights <- "ignore"
    }
  }

  benchmark_grid <- benchmark_grid(
    tasks = task_train_selected,
    learners = auto_tuners,
    resamplings = outer_resampling
  )

  if (resume_training_mode) {
    log_message("恢复训练模式下跳过重复检查点保存，直接进入模型训练")
  } else {
    log_message("=== 保存训练前检查点 ===")
    tryCatch(
      {
        finalize_workspace_save(opts$outdir)
        log_message("✓ 已保存训练前检查点，可用于保留EFS与特征选择结果")
      },
      error = function(e) {
        log_message("⚠️  训练前检查点保存失败：", e$message, level = "WARNING")
      }
    )
  }

  log_message("=== 开始模型训练和评估 ===")
  log_message("使用", outer_resampling$id, "进行交叉验证")
  log_message("预计模型训练阶段基础拟合次数：", estimated_trains)
  log_message("调参设置：n_evals=", opts$tune_evals, ", batch_size=", min(opts$tune_batch_size, opts$tune_evals))
  log_message("训练", length(auto_tuners), "种算法，这可能需要较长时间...")

  benchmark_start_time <- Sys.time()
  bmr <- with_progress({
    benchmark(benchmark_grid, store_models = TRUE)
  })
  benchmark_end_time <- Sys.time()
  benchmark_elapsed <- difftime(benchmark_end_time, benchmark_start_time, units = "mins")
  log_message("✓ Benchmark完成，耗时：", round(benchmark_elapsed, 2), "分钟")

  p7 <- autoplot(bmr, measure = msr("classif.auc")) +
    mytheme() +
    labs(title = "model comparison - auc")
  save_pdf_plot(file.path(opts$outdir, "step2.1.1.model_comparison.auc.pdf"), p7, width = 12.5, height = 12, units = "in", dpi = 300)

  p7 <- autoplot(bmr, measure = msr("classif.acc")) +
    mytheme() +
    labs(title = "model comparison - acc")
  save_pdf_plot(file.path(opts$outdir, "step2.1.2.model_comparison.acc.pdf"), p7, width = 12.5, height = 12, units = "in", dpi = 300)

  p7 <- autoplot(bmr, measure = msr("classif.ce")) +
    mytheme() +
    labs(title = "model comparison - ce")
  save_pdf_plot(file.path(opts$outdir, "step2.1.3.model_comparison.ce.pdf"), p7, width = 12.5, height = 12, units = "in", dpi = 300)

  p8 <- tryCatch(
    {
      autoplot(bmr, type = "roc")
    },
    error = function(e) {
      log_message("⚠️  模型比较ROC总览图生成失败：", e$message, level = "WARNING")
      ggplot() +
        theme_void() +
        annotate(
          "text",
          x = 0.5, y = 0.5,
          label = paste(
            "Model comparison ROC unavailable",
            "Some resampling slices contain only one class",
            sep = "\n"
          ),
          size = 5
        ) +
        labs(title = "model comparison - roc")
    }
  )
  save_pdf_plot(file.path(opts$outdir, "step2.2.model_comparison.roc.pdf"), p8, width = 12.5, height = 12, units = "in", dpi = 300)

  # 06. 模型评估 --------------------------------------------------------------

  performance_results <- bmr$aggregate(msrs(c("classif.auc", "classif.ce", "classif.acc", "classif.fbeta", "classif.precision", "classif.recall", "classif.mcc")))
  cv_results <- performance_results[, c("learner_id", "classif.auc", "classif.acc", "classif.ce", "classif.fbeta", "classif.precision", "classif.recall", "classif.mcc")] %>%
    mutate(learner_id = normalize_model_id(learner_id)) %>%
    rename(
      Model = learner_id, CV_AUC = classif.auc, CV_ACC = classif.acc, CV_CE = classif.ce,
      CV_F1 = classif.fbeta, CV_Precision = classif.precision, CV_Recall = classif.recall, CV_MCC = classif.mcc
    )

  cv_roc_all <- extract_roc_data_from_bmr(bmr)

  best_learner_id <- performance_results[which.min(performance_results$classif.ce), "learner_id"]

  model_colors <- c(
    "glmnet" = "#E41A1C",
    "xgboost" = "#377EB8",
    "ranger" = "#4DAF4A",
    "svm" = "#984EA3",
    "lightgbm" = "#FF7F00",
    "naive_bayes" = "#FFFF33",
    "kknn" = "#A65628",
    "rpart" = "#F781BF",
    "adaboost" = "#999999",
    "nnet" = "#66C2A5"
  )
  model_colors <- model_colors[names(model_colors) %in% c(selected_model_names, "ranger")]

  best_learner_name <- normalize_model_id(best_learner_id$learner_id)
  best_learner <- auto_tuners[[best_learner_name]]
  print(paste("选择的最佳模型:", best_learner_name))

  results_dt <- as.data.table(bmr)

  all_models_results <- list()
  for (i in 1:nrow(results_dt)) {
    task_id <- results_dt$iteration[i]
    learner_id <- results_dt$learner[[1]]$learner$id

    current_model <- list(
      task_id = task_id,
      learner_id = learner_id,
      model_type = "standard",
      parameters = list(),
      performance = list()
    )

    learner <- results_dt$learner[[i]]

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

    all_models_results[[paste(task_id, learner_id, sep = "_")]] <- current_model
  }

  # 07. 训练和测试集预测（新增：混淆矩阵和阈值性能）-------------------------------

  log_message("=== 开始训练和测试集预测 ===")
  model_results <- list()
  confusion_matrices <- list(train = list(), test = list())

  for (learner_name in names(auto_tuners)) {
    log_message("处理模型:", learner_name, "(", match(learner_name, names(auto_tuners)), "/", length(auto_tuners), ")")
    current_learner <- auto_tuners[[learner_name]]

    tryCatch(
      {
        current_learner$train(task_train_selected)

        train_pred <- current_learner$predict(task_train_selected)
        train_performance <- train_pred$score(msrs(c("classif.auc", "classif.acc", "classif.ce", "classif.fbeta", "classif.precision", "classif.recall", "classif.mcc")))

        test_pred <- current_learner$predict(task_test_selected)
        test_performance <- test_pred$score(msrs(c("classif.auc", "classif.acc", "classif.ce", "classif.fbeta", "classif.precision", "classif.recall", "classif.mcc")))

        log_message(
          "  ✓ 训练成功 - 训练集 AUC:", round(train_performance["classif.auc"], 4),
          "测试集 AUC:", round(test_performance["classif.auc"], 4)
        )
      },
      error = function(e) {
        log_message("  ✗ 模型训练或预测失败：", e$message, level = "ERROR")

        # 针对不同模型提供具体的诊断信息
        if (learner_name == "adaboost") {
          log_message("  AdaBoost失败原因可能是：", level = "ERROR")
          log_message("    1. RWeka包未正确安装", level = "ERROR")
          log_message("    2. Java环境未配置或版本不兼容", level = "ERROR")
          log_message("    3. 数据中存在NaN或Inf值", level = "ERROR")
          log_message("  解决方案：", level = "ERROR")
          log_message("    - 安装RWeka: install.packages('RWeka')", level = "ERROR")
          log_message("    - 检查Java: Sys.getenv('JAVA_HOME')", level = "ERROR")
          log_message("    - 清理数据中的异常值", level = "ERROR")
        } else {
          log_message("  可能原因：数据不兼容、超参数设置问题、或依赖包缺失", level = "ERROR")
        }

        # 创建NA性能指标
        train_performance <<- c(
          classif.auc = NA, classif.acc = NA, classif.ce = NA,
          classif.fbeta = NA, classif.precision = NA, classif.recall = NA, classif.mcc = NA
        )
        test_performance <<- c(
          classif.auc = NA, classif.acc = NA, classif.ce = NA,
          classif.fbeta = NA, classif.precision = NA, classif.recall = NA, classif.mcc = NA
        )
        # 创建空的预测数据
        train_pred <<- NULL
        test_pred <<- NULL
      }
    )

    # 只有在预测成功时才继续处理
    if (!is.null(train_pred) && !is.null(test_pred)) {
      # 安全提取预测概率
      train_prob <- tryCatch(
        {
          prob_col <- train_pred$prob[, task_train_selected$positive]
          # 尝试多种转换方法
          if (is.list(prob_col)) {
            as.numeric(unlist(prob_col))
          } else if (is.data.frame(prob_col)) {
            as.numeric(unlist(prob_col[, 1]))
          } else {
            as.numeric(prob_col)
          }
        },
        error = function(e) {
          log_message("⚠️  训练集概率提取失败，使用默认值0.5：", e$message, level = "WARNING")
          rep(0.5, nrow(task_train_selected$data()))
        }
      )

      test_prob <- tryCatch(
        {
          prob_col <- test_pred$prob[, task_train_selected$positive]
          # 尝试多种转换方法
          if (is.list(prob_col)) {
            as.numeric(unlist(prob_col))
          } else if (is.data.frame(prob_col)) {
            as.numeric(unlist(prob_col[, 1]))
          } else {
            as.numeric(prob_col)
          }
        },
        error = function(e) {
          log_message("⚠️  测试集概率提取失败，使用默认值0.5：", e$message, level = "WARNING")
          rep(0.5, nrow(task_test_selected$data()))
        }
      )

      train_roc_data <- data.frame(
        SampleID = rownames(task_train_selected$data()),
        truth = as.integer(train_pred$truth == task_train_selected$positive),
        prob = train_prob,
        dataset = "Training"
      )

      test_roc_data <- data.frame(
        SampleID = rownames(task_test_selected$data()),
        truth = as.integer(test_pred$truth == task_train_selected$positive),
        prob = test_prob,
        dataset = "Testing"
      )

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
        model <- current_learner$model$learner$model
        if (inherits(model, "lgb.Booster")) {
          imp <- lightgbm::lgb.importance(model)
          if (!is.null(imp)) {
            feature_importance <- data.frame(
              Feature = imp$Feature,
              Importance = imp$Gain
            )
          }
        }
      } else if (learner_name == "svm") {
        model <- current_learner$model$learner$model
        sv_matrix <- model$SV
        if (!is.null(sv_matrix)) {
          sv_matrix <- as.matrix(sv_matrix)
        }
        if (!is.null(sv_matrix) && is.numeric(sv_matrix)) {
          sv_importance <- colMeans(abs(sv_matrix))
          feature_importance <- data.frame(
            Feature = names(sv_importance),
            Importance = sv_importance
          )
        }
      } else if (learner_name == "rpart") {
        # Decision Tree (rpart)
        model <- current_learner$model$learner$model
        if (!is.null(model$variable.importance)) {
          feature_importance <- data.frame(
            Feature = names(model$variable.importance),
            Importance = as.numeric(model$variable.importance)
          )
        }
      } else if (learner_name == "nnet") {
        # Neural Network - 使用Garson算法计算重要性
        tryCatch(
          {
            model <- current_learner$model$learner$model
            # 提取权重矩阵
            wts <- model$wts
            # 计算Garson重要性
            n_inputs <- length(task_train_selected$feature_names)
            n_hidden <- model$n[2]

            # 输入层到隐层的权重
            w_ih <- matrix(wts[1:(n_inputs * n_hidden)], nrow = n_inputs, ncol = n_hidden)
            # 隐层到输出层的权重
            w_ho_start <- n_inputs * n_hidden + n_hidden + 1 # 跳过隐层偏置
            w_ho <- wts[w_ho_start:(w_ho_start + n_hidden - 1)]

            # Garson算法：计算每个输入特征的相对重要性
            importance_scores <- numeric(n_inputs)
            for (i in 1:n_inputs) {
              importance_scores[i] <- sum(abs(w_ih[i, ] * w_ho)) / sum(abs(w_ih * rep(w_ho, each = n_inputs)))
            }

            feature_importance <- data.frame(
              Feature = task_train_selected$feature_names,
              Importance = importance_scores
            )
          },
          error = function(e) {
            # 如果Garson算法失败，使用简单的权重绝对值和
            tryCatch(
              {
                model <- current_learner$model$learner$model
                n_inputs <- length(task_train_selected$feature_names)
                n_hidden <- suppressWarnings(as.integer(model$n[2]))
                wts <- as.numeric(model$wts)
                if (is.na(n_hidden) || n_hidden < 1 || length(wts) < n_inputs * n_hidden) {
                  stop("nnet weight structure is unavailable for fallback importance")
                }
                w_ih <- matrix(wts[1:(n_inputs * n_hidden)], nrow = n_inputs, ncol = n_hidden)
                importance_scores <- rowSums(abs(w_ih))

                feature_importance <<- data.frame(
                  Feature = task_train_selected$feature_names,
                  Importance = importance_scores
                )
              },
              error = function(e2) {
                log_message("  ⚠️  nnet特征重要性计算失败，跳过：", e2$message, level = "WARNING")
                feature_importance <<- NULL
              }
            )
          }
        )
      } else if (learner_name == "kknn") {
        # KNN - 使用permutation importance
        tryCatch(
          {
            # 改进方法：使用多次permutation importance取平均，并在测试集上计算
            base_pred <- current_learner$predict(task_test_selected)
            base_auc <- base_pred$score(msr("classif.auc"))

            importance_scores <- numeric(length(task_test_selected$feature_names))
            names(importance_scores) <- task_test_selected$feature_names

            # 自适应计算重复次数
            n_samples <- nrow(task_test_selected$data())
            n_features <- length(task_test_selected$feature_names)
            n_repeats <- get_perm_repeats(n_samples, n_features)
            log_message("  KNN permutation重复次数:", n_repeats, "（样本数:", n_samples, "，特征数:", n_features, "）")

            for (i in seq_along(task_test_selected$feature_names)) {
              feat <- task_test_selected$feature_names[i]
              perm_scores <- numeric(n_repeats)

              for (rep in 1:n_repeats) {
                data_perm <- task_test_selected$data()
                data_perm[[feat]] <- sample(data_perm[[feat]])
                task_perm <- TaskClassif$new(
                  id = "perm", backend = data_perm,
                  target = "group", positive = task_test_selected$positive
                )
                perm_pred <- current_learner$predict(task_perm)
                perm_auc <- perm_pred$score(msr("classif.auc"))
                perm_scores[rep] <- base_auc - perm_auc
              }

              # 取平均值
              importance_scores[i] <- mean(perm_scores)
            }

            feature_importance <- data.frame(
              Feature = names(importance_scores),
              Importance = pmax(importance_scores, 0) # 确保非负
            )
          },
          error = function(e) {
            # 如果permutation失败，返回NULL
            feature_importance <- NULL
          }
        )
      } else if (learner_name == "naive_bayes") {
        # Naive Bayes - 使用条件概率差异作为重要性
        tryCatch(
          {
            model <- current_learner$model$learner$model
            # 提取每个特征的条件概率表
            # 计算类别间的差异作为重要性度量
            importance_scores <- numeric(length(task_train_selected$feature_names))
            names(importance_scores) <- task_train_selected$feature_names

            # 简化方法：使用训练数据计算特征与目标的相关性
            data <- task_train_selected$data()
            for (feat in task_train_selected$feature_names) {
              # 计算特征与目标的相关性（绝对值）
              if (is.numeric(data[[feat]])) {
                cor_val <- abs(cor(as.numeric(data[[feat]]),
                  as.numeric(data$group),
                  use = "complete.obs"
                ))
                importance_scores[feat] <- ifelse(is.na(cor_val), 0, cor_val)
              } else {
                # 对于分类特征，使用卡方检验
                chi_test <- tryCatch(
                  {
                    chisq.test(table(data[[feat]], data$group))$statistic
                  },
                  error = function(e) 0
                )
                importance_scores[feat] <- chi_test
              }
            }

            feature_importance <- data.frame(
              Feature = names(importance_scores),
              Importance = importance_scores
            )
          },
          error = function(e) {
            feature_importance <- NULL
          }
        )
      } else if (learner_name == "adaboost") {
        # AdaBoost - 提取弱学习器的重要性
        tryCatch(
          {
            model <- current_learner$model$learner$model
            # RWeka的AdaBoost模型结构
            # 尝试提取特征使用频率作为重要性
            model_str <- capture.output(print(model))

            # 改进方法：使用多次permutation importance取平均，并在测试集上计算
            base_pred <- current_learner$predict(task_test_selected)
            base_auc <- base_pred$score(msr("classif.auc"))

            importance_scores <- numeric(length(task_test_selected$feature_names))
            importance_se <- numeric(length(task_test_selected$feature_names)) # 新增：标准误差
            names(importance_scores) <- task_test_selected$feature_names
            names(importance_se) <- task_test_selected$feature_names

            # 自适应计算重复次数（AdaBoost至少10次，因为容易出现负值）
            n_samples <- nrow(task_test_selected$data())
            n_features <- length(task_test_selected$feature_names)
            n_repeats <- max(10, get_perm_repeats(n_samples, n_features)) # 至少10次
            log_message("  AdaBoost permutation重复次数:", n_repeats, "（样本数:", n_samples, "，特征数:", n_features, "）")

            for (i in seq_along(task_test_selected$feature_names)) {
              feat <- task_test_selected$feature_names[i]
              perm_scores <- numeric(n_repeats)

              for (rep in 1:n_repeats) {
                data_perm <- task_test_selected$data()
                data_perm[[feat]] <- sample(data_perm[[feat]])
                task_perm <- TaskClassif$new(
                  id = "perm", backend = data_perm,
                  target = "group", positive = task_test_selected$positive
                )
                perm_pred <- current_learner$predict(task_perm)
                perm_auc <- perm_pred$score(msr("classif.auc"))
                perm_scores[rep] <- base_auc - perm_auc
              }

              # 取平均值和标准误差
              importance_scores[i] <- mean(perm_scores)
              importance_se[i] <- sd(perm_scores) / sqrt(n_repeats)
            }

            # 计算95%置信区间
            ci_lower <- importance_scores - 1.96 * importance_se
            ci_upper <- importance_scores + 1.96 * importance_se

            # 统计显著性：如果置信区间包含0，则设为0
            importance_final <- ifelse(ci_lower > 0, importance_scores,
              ifelse(ci_upper < 0, importance_scores, 0)
            )

            # 记录负值特征（可能是噪声）
            negative_features <- names(importance_scores)[importance_scores < -0.001]
            if (length(negative_features) > 0) {
              log_message("  ⚠️  AdaBoost发现", length(negative_features), "个可能的噪声/冗余特征:",
                paste(negative_features, collapse = ", "),
                level = "WARNING"
              )
            }

            feature_importance <- data.frame(
              Feature = names(importance_scores),
              Importance = importance_final, # 使用统计显著性过滤后的值
              Raw_Importance = importance_scores, # 保留原始值
              Std_Error = importance_se # 保留标准误差
            )
          },
          error = function(e) {
            feature_importance <- NULL
          }
        )
      }

      # 新增：计算阈值和混淆矩阵
      threshold_used <- get_threshold(train_roc_data$truth, train_roc_data$prob, opts$threshold)

      train_thr <- perf_with_threshold(train_roc_data, threshold_used)
      test_thr <- perf_with_threshold(test_roc_data, threshold_used)

      cm_train_df <- tibble(
        Model = learner_name, Dataset = "train", Threshold = threshold_used,
        TP = train_thr$cm["TP"], FP = train_thr$cm["FP"], TN = train_thr$cm["TN"], FN = train_thr$cm["FN"],
        ACC = train_thr$acc, CE = train_thr$ce,
        Precision = train_thr$precision, Recall = train_thr$recall, F1 = train_thr$f1
      )
      cm_test_df <- tibble(
        Model = learner_name, Dataset = "test", Threshold = threshold_used,
        TP = test_thr$cm["TP"], FP = test_thr$cm["FP"], TN = test_thr$cm["TN"], FN = test_thr$cm["FN"],
        ACC = test_thr$acc, CE = test_thr$ce,
        Precision = test_thr$precision, Recall = test_thr$recall, F1 = test_thr$f1
      )
      confusion_matrices$train[[learner_name]] <- cm_train_df
      confusion_matrices$test[[learner_name]] <- cm_test_df

      model_results[[learner_name]] <- list(
        model = current_learner,
        train_performance = train_performance,
        test_performance = test_performance,
        train_performance_thr = c(
          classif.acc = train_thr$acc,
          classif.ce = train_thr$ce,
          classif.precision = train_thr$precision,
          classif.recall = train_thr$recall,
          classif.f1 = train_thr$f1
        ),
        test_performance_thr = c(
          classif.acc = test_thr$acc,
          classif.ce = test_thr$ce,
          classif.precision = test_thr$precision,
          classif.recall = test_thr$recall,
          classif.f1 = test_thr$f1
        ),
        threshold_used = threshold_used,
        feature_importance = feature_importance,
        train_roc_data = train_roc_data,
        test_roc_data = test_roc_data
      )
    } else {
      # 如果预测失败，创建空的model_results条目
      log_message("  跳过", learner_name, "的结果保存（训练/预测失败）", level = "WARNING")
      model_results[[learner_name]] <- list(
        model = current_learner,
        train_performance = train_performance,
        test_performance = test_performance,
        train_performance_thr = c(
          classif.acc = NA, classif.ce = NA, classif.precision = NA,
          classif.recall = NA, classif.f1 = NA
        ),
        test_performance_thr = c(
          classif.acc = NA, classif.ce = NA, classif.precision = NA,
          classif.recall = NA, classif.f1 = NA
        ),
        threshold_used = NA,
        feature_importance = NULL,
        train_roc_data = NULL,
        test_roc_data = NULL
      )
    }
  }

  calc_sensitivity <- function(tp, fn) {
    denom <- tp + fn
    ifelse(denom > 0, tp / denom, NA_real_)
  }

  calc_specificity <- function(tn, fp) {
    denom <- tn + fp
    ifelse(denom > 0, tn / denom, NA_real_)
  }

  calc_mcc_from_cm <- function(tp, fp, tn, fn) {
    denom <- (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)
    ifelse(denom > 0, ((tp * tn) - (fp * fn)) / sqrt(denom), NA_real_)
  }

  enrich_confusion_metrics <- function(x) {
    if (!nrow(x) || !all(c("TP", "FP", "TN", "FN") %in% names(x))) {
      return(x)
    }
    x$Sensitivity <- calc_sensitivity(x$TP, x$FN)
    x$Specificity <- calc_specificity(x$TN, x$FP)

    preferred <- c(
      "Model", "Dataset", "Threshold", "TP", "FP", "TN", "FN",
      "ACC", "CE", "Precision", "Recall", "Sensitivity", "Specificity", "F1"
    )
    x[, c(intersect(preferred, names(x)), setdiff(names(x), preferred)), drop = FALSE]
  }

  threshold_summary_from_cm <- function(x, prefix) {
    if (!nrow(x) || !"Model" %in% names(x) || !"Threshold" %in% names(x)) {
      return(data.frame(Model = character(0), stringsAsFactors = FALSE))
    }
    out <- x[, intersect(c("Model", "Threshold", "TP", "FP", "TN", "FN", "Sensitivity", "Specificity"), names(x)), drop = FALSE]
    if (all(c("TP", "FP", "TN", "FN") %in% names(out))) {
      out[[paste0(prefix, "_MCC_from_cm")]] <- calc_mcc_from_cm(out$TP, out$FP, out$TN, out$FN)
    }
    keep <- c("Model", "Threshold", "Sensitivity", "Specificity", paste0(prefix, "_MCC_from_cm"))
    out <- out[, intersect(keep, names(out)), drop = FALSE]
    names(out)[names(out) == "Threshold"] <- paste0(prefix, "_Threshold")
    names(out)[names(out) == "Sensitivity"] <- paste0(prefix, "_Sensitivity")
    names(out)[names(out) == "Specificity"] <- paste0(prefix, "_Specificity")
    out
  }

  move_after <- function(cols, col, after) {
    if (!col %in% cols || !after %in% cols) {
      return(cols)
    }
    cols <- cols[cols != col]
    append(cols, col, after = match(after, cols))
  }

  clean_threshold_performance_table <- function(df, confusion_by_prefix) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    metric_names <- c("ACC", "CE", "Precision", "Recall", "F1")

    for (prefix in names(confusion_by_prefix)) {
      for (metric in metric_names) {
        base_col <- paste(prefix, metric, sep = "_")
        thr_col <- paste0(base_col, "_thr")
        if (thr_col %in% names(df)) {
          df[[base_col]] <- df[[thr_col]]
        }
      }
    }

    remove_cols <- grep("(_thr$|\\.(x|y)$)", names(df), value = TRUE)
    if (length(remove_cols)) {
      df <- df[, setdiff(names(df), remove_cols), drop = FALSE]
    }

    for (prefix in names(confusion_by_prefix)) {
      th <- threshold_summary_from_cm(confusion_by_prefix[[prefix]], prefix)
      if (!nrow(th)) {
        next
      }
      refresh_cols <- setdiff(names(th), "Model")
      refresh_cols <- unique(c(refresh_cols, paste0(refresh_cols, ".x"), paste0(refresh_cols, ".y")))
      df <- df[, setdiff(names(df), refresh_cols), drop = FALSE]
      df <- as.data.frame(dplyr::left_join(df, as.data.frame(th, stringsAsFactors = FALSE), by = "Model"), stringsAsFactors = FALSE)

      mcc_col <- paste0(prefix, "_MCC")
      mcc_from_cm <- paste0(prefix, "_MCC_from_cm")
      if (mcc_from_cm %in% names(df)) {
        df[[mcc_col]] <- df[[mcc_from_cm]]
        df[[mcc_from_cm]] <- NULL
      }
    }

    cols <- names(df)
    for (prefix in names(confusion_by_prefix)) {
      threshold_col <- paste0(prefix, "_Threshold")
      sensitivity_col <- paste0(prefix, "_Sensitivity")
      specificity_col <- paste0(prefix, "_Specificity")
      auc_col <- paste0(prefix, "_AUC")
      recall_col <- paste0(prefix, "_Recall")

      if (threshold_col %in% cols && auc_col %in% cols) {
        cols <- move_after(cols, threshold_col, auc_col)
      }
      if (sensitivity_col %in% cols && recall_col %in% cols) {
        cols <- move_after(cols, sensitivity_col, recall_col)
      }
      if (specificity_col %in% cols && sensitivity_col %in% cols) {
        cols <- move_after(cols, specificity_col, sensitivity_col)
      }
    }

    df[, cols, drop = FALSE]
  }

  confusion_train_all <- enrich_confusion_metrics(bind_rows(confusion_matrices$train))
  confusion_test_all <- enrich_confusion_metrics(bind_rows(confusion_matrices$test))

  feature_descriptions <- names_values$old_colnames
  names(feature_descriptions) <- names_values$new_colnames

  performance_comparison <- data.frame(
    Model = character(),
    Train_AUC = numeric(),
    Train_ACC = numeric(),
    Train_CE = numeric(),
    Train_Precision = numeric(),
    Train_Recall = numeric(),
    Train_F1 = numeric(),
    Train_MCC = numeric(),
    Test_AUC = numeric(),
    Test_ACC = numeric(),
    Test_CE = numeric(),
    Test_Precision = numeric(),
    Test_Recall = numeric(),
    Test_F1 = numeric(),
    Test_MCC = numeric(),
    Train_ACC_thr = numeric(),
    Train_CE_thr = numeric(),
    Train_Precision_thr = numeric(),
    Train_Recall_thr = numeric(),
    Train_F1_thr = numeric(),
    Test_ACC_thr = numeric(),
    Test_CE_thr = numeric(),
    Test_Precision_thr = numeric(),
    Test_Recall_thr = numeric(),
    Test_F1_thr = numeric(),
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
        Train_Precision = model_results[[model_name]]$train_performance["classif.precision"],
        Train_Recall = model_results[[model_name]]$train_performance["classif.recall"],
        Train_F1 = model_results[[model_name]]$train_performance["classif.fbeta"],
        Train_MCC = model_results[[model_name]]$train_performance["classif.mcc"],
        Test_AUC = model_results[[model_name]]$test_performance["classif.auc"],
        Test_ACC = model_results[[model_name]]$test_performance["classif.acc"],
        Test_CE = model_results[[model_name]]$test_performance["classif.ce"],
        Test_Precision = model_results[[model_name]]$test_performance["classif.precision"],
        Test_Recall = model_results[[model_name]]$test_performance["classif.recall"],
        Test_F1 = model_results[[model_name]]$test_performance["classif.fbeta"],
        Test_MCC = model_results[[model_name]]$test_performance["classif.mcc"],
        Train_ACC_thr = model_results[[model_name]]$train_performance_thr["classif.acc"],
        Train_CE_thr = model_results[[model_name]]$train_performance_thr["classif.ce"],
        Train_Precision_thr = model_results[[model_name]]$train_performance_thr["classif.precision"],
        Train_Recall_thr = model_results[[model_name]]$train_performance_thr["classif.recall"],
        Train_F1_thr = model_results[[model_name]]$train_performance_thr["classif.f1"],
        Test_ACC_thr = model_results[[model_name]]$test_performance_thr["classif.acc"],
        Test_CE_thr = model_results[[model_name]]$test_performance_thr["classif.ce"],
        Test_Precision_thr = model_results[[model_name]]$test_performance_thr["classif.precision"],
        Test_Recall_thr = model_results[[model_name]]$test_performance_thr["classif.recall"],
        Test_F1_thr = model_results[[model_name]]$test_performance_thr["classif.f1"]
      )
    )
  }

  performance_comparison <- left_join(cv_results, performance_comparison)

  train_row_ids <- task_train_selected$row_ids
  test_row_ids <- task_test_selected$row_ids

  real_train_sample_ids <- rownames(Data)[train_row_ids]
  real_test_sample_ids <- rownames(Data)[test_row_ids]

  train_roc_all <- list()
  test_roc_all <- list()
  for (model_name in names(model_results)) {
    # 安全处理train_roc_data
    train_roc_all[[model_name]] <- tryCatch(
      {
        if (!is.null(model_results[[model_name]]$train_roc_data)) {
          model_results[[model_name]]$train_roc_data %>% mutate(model = model_name)
        } else {
          NULL
        }
      },
      error = function(e) {
        log_message("⚠️  ", model_name, "的train_roc_data处理失败：", e$message, level = "WARNING")
        NULL
      }
    )

    # 安全处理test_roc_data
    test_roc_all[[model_name]] <- tryCatch(
      {
        if (!is.null(model_results[[model_name]]$test_roc_data)) {
          model_results[[model_name]]$test_roc_data %>% mutate(model = model_name)
        } else {
          NULL
        }
      },
      error = function(e) {
        log_message("⚠️  ", model_name, "的test_roc_data处理失败：", e$message, level = "WARNING")
        NULL
      }
    )
  }

  # 移除NULL条目
  train_roc_all <- train_roc_all[!sapply(train_roc_all, is.null)]
  test_roc_all <- test_roc_all[!sapply(test_roc_all, is.null)]

  log_message("有效的train_roc模型数：", length(train_roc_all))
  log_message("有效的test_roc模型数：", length(test_roc_all))

  for (model_name in names(train_roc_all)) {
    train_roc_all[[model_name]]$SampleID <- real_train_sample_ids
  }

  for (model_name in names(test_roc_all)) {
    test_roc_all[[model_name]]$SampleID <- real_test_sample_ids
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

  # 新增：PR曲线绘制
  train_pr_plot <- pr_pict(train_roc_all,
    title = "训练集PR曲线", path = file.path(opts$outdir, "train_pr_plot"),
    colors = model_colors, labels = names(train_roc_all)
  )

  test_pr_plot <- pr_pict(test_roc_all,
    title = "测试集PR曲线", path = file.path(opts$outdir, "test_pr_plot"),
    colors = model_colors, labels = names(test_roc_all)
  )

  # 新增：计算并添加AUPRC到性能对比表
  auprc_results <- data.frame(Model = character(), Train_AUPRC = numeric(), Test_AUPRC = numeric())
  for (model_name in names(model_results)) {
    train_pred_obj <- ROCR::prediction(
      model_results[[model_name]]$train_roc_data$prob,
      factor(model_results[[model_name]]$train_roc_data$truth, levels = c(0, 1))
    )
    test_pred_obj <- ROCR::prediction(
      model_results[[model_name]]$test_roc_data$prob,
      factor(model_results[[model_name]]$test_roc_data$truth, levels = c(0, 1))
    )

    train_auprc <- ROCR::performance(train_pred_obj, "aucpr")@y.values[[1]]
    test_auprc <- ROCR::performance(test_pred_obj, "aucpr")@y.values[[1]]

    auprc_results <- rbind(auprc_results, data.frame(
      Model = model_name, Train_AUPRC = train_auprc, Test_AUPRC = test_auprc
    ))
  }
  performance_comparison <- left_join(performance_comparison, auprc_results, by = "Model")

  # 重新排列列顺序：CV -> Train -> Test；阈值指标先保留，随后归一化为正式展示列。
  performance_comparison <- performance_comparison %>%
    select(
      Model,
      # CV指标
      CV_AUC, CV_ACC, CV_CE, CV_F1, CV_Precision, CV_Recall, CV_MCC,
      # Train指标（普通+_thr混合）
      Train_AUC, Train_ACC, Train_ACC_thr,
      Train_CE, Train_CE_thr,
      Train_Precision, Train_Precision_thr,
      Train_Recall, Train_Recall_thr,
      Train_F1, Train_F1_thr,
      Train_MCC,
      Train_AUPRC,
      # Test指标（普通+_thr混合）
      Test_AUC, Test_ACC, Test_ACC_thr,
      Test_CE, Test_CE_thr,
      Test_Precision, Test_Precision_thr,
      Test_Recall, Test_Recall_thr,
      Test_F1, Test_F1_thr,
      Test_MCC,
      Test_AUPRC
    )

  performance_comparison <- clean_threshold_performance_table(
    performance_comparison,
    list(Train = confusion_train_all, Test = confusion_test_all)
  )

  all_train_data <- bind_rows(train_roc_all, .id = "source_model")
  all_test_data <- bind_rows(test_roc_all, .id = "source_model")

  boxplot_data_list <- list(
    "Train_All_Models" = all_train_data,
    "Test_All_Models" = all_test_data
  )

  for (model_name in names(train_roc_all)) {
    boxplot_data_list[[paste0("Train_", model_name)]] <- train_roc_all[[model_name]]
    boxplot_data_list[[paste0("Test_", model_name)]] <- test_roc_all[[model_name]]
  }

  write.xlsx(boxplot_data_list, file = file.path(opts$outdir, "boxplot_data.xlsx"))

  # 08. 模型详细信息收集 -------------------------------------------------------

  model_details <- list()
  for (model_name in names(model_results)) {
    model_details[[model_name]] <- get_model_info(
      model_name,
      model_results[[model_name]]$model,
      task_train_selected
    )

    # 如果get_model_info没有提取到特征重要性，但model_results中有，则使用model_results中的
    if (is.null(model_details[[model_name]]$feature_importance) &&
      !is.null(model_results[[model_name]]$feature_importance)) {
      model_details[[model_name]]$feature_importance <- model_results[[model_name]]$feature_importance
    }
  }

  names(train_roc_all)
  names(test_roc_all)
  names(cv_roc_all)
  names(cv_roc_all) <- gsub("ranger", "rf", names(train_roc_all))
  nn <- names(cv_roc_all)
  FS <- as.data.frame(feature_descriptions) %>%
    rownames_to_column() %>%
    rename_all(~ c("Feature", "Description"))

  Plot_data4 <- lapply(model_details, function(x) {
    if (is.null(x$feature_importance) || nrow(x$feature_importance) == 0) {
      return(data.frame(
        Feature = character(0),
        Importance = numeric(0),
        Description = character(0),
        Description_fold = character(0),
        stringsAsFactors = FALSE
      ))
    }

    x$feature_importance$Importance <- as.numeric(x$feature_importance$Importance)

    result <- left_join(x$feature_importance, FS, by = c("Feature" = "Feature")) %>%
      arrange(Importance) %>%
      mutate(Description = ifelse(is.na(Description) | Description == "", Feature, Description)) %>%
      mutate(Description_fold = Add_breaks(Description)) %>%
      mutate(Description_fold = fct_inorder(Description_fold))

    return(result)
  })

  # 09. 绘制各模型详细图表（新增：ROC+PR曲线） ----------------------------------

  for (i in 1:length(nn)) {
    threshold_used <- if (!is.null(model_results[[nn[i]]])) model_results[[nn[i]]]$threshold_used else NA
    p1 <- roc_box_plot(train_roc_all[[nn[i]]], title = "train", label = nn[i], threshold = threshold_used)
    p2 <- roc_box_plot(test_roc_all[[nn[i]]], title = "test", label = nn[i], threshold = threshold_used)
    pr1 <- pr_box_plot(train_roc_all[[nn[i]]], title = "train", label = nn[i], threshold = threshold_used)
    pr2 <- pr_box_plot(test_roc_all[[nn[i]]], title = "test", label = nn[i], threshold = threshold_used)

    if (nrow(Plot_data4[[nn[i]]]) > 0) {
      p4 <- plot_p_feat(Plot_data4[[nn[i]]], out = opts$outdir, plot = FALSE)
    } else {
      p4 <- ggplot() +
        geom_text(aes(x = 0.5, y = 0.5, label = "No feature importance data available"),
          size = 6, hjust = 0.5, vjust = 0.5
        ) +
        theme_void() +
        ggtitle(paste("Feature importance for", nn[i]))
    }

    bind_rows(list(
      p1[[3]] %>% broom::tidy() %>% mutate(data = "train"),
      p2[[3]] %>% broom::tidy() %>% mutate(data = "test")
    )) %>%
      write_tsv(file.path(opts$outdir, paste0("step3.", nn[i], "_pod_wilcox_test.tsv")))

    ComBn_plot(list(p1[1:2], p2[1:2]),
      w = p4, out_parm = opts$outdir, biomaker_num_fix_parm = paste0(nn[i], "_roc")
    )
    ComBn_plot(list(pr1[1:2], pr2[1:2]),
      w = p4, out_parm = opts$outdir, biomaker_num_fix_parm = paste0(nn[i], "_pr")
    )
  }

  # 10. Excel报告生成（新增：混淆矩阵和AUPRC）---------------------------------

  wb <- createWorkbook()

  for (model_name in names(model_details)) {
    info <- model_details[[model_name]]

    addWorksheet(wb, model_name)

    performance_data <- performance_comparison %>%
      filter(Model == model_name) %>%
      pivot_longer(
        cols = starts_with(c("CV_", "Train_", "Test_")),
        names_to = "Metric_Split",
        values_to = "Value"
      ) %>%
      separate(
        col = "Metric_Split",
        into = c("Data_Split", "Metric"),
        sep = "_",
        extra = "merge", fill = "right"
      ) %>%
      pivot_wider(
        names_from = "Data_Split",
        values_from = "Value"
      ) %>%
      select(-Model)

    writeData(wb, model_name, "模型性能", startRow = 1, startCol = 1)
    writeData(wb, model_name, performance_data, startRow = 2, startCol = 1)

    current_row <- 2 + nrow(performance_data) + 2

    writeData(wb, model_name, "最优超参数", startRow = current_row, startCol = 1)
    hyperparams_df <- data.frame(
      Parameter = names(info$hyperparameters),
      Value = as.character(info$hyperparameters)
    )
    writeData(wb, model_name, hyperparams_df, startRow = current_row + 1, startCol = 1)

    current_row <- current_row + 1 + nrow(hyperparams_df) + 2

    if (!is.null(info$feature_importance)) {
      writeData(wb, model_name, "特征重要性", startRow = current_row, startCol = 1)

      feature_importance <- info$feature_importance
      feature_importance <- feature_importance[order(-feature_importance$Importance), ]

      feature_importance$Original_Name <- feature_importance$Feature

      for (i in 1:nrow(feature_importance)) {
        feature_name <- feature_importance$Feature[i]
        if (feature_name %in% names(feature_descriptions)) {
          feature_importance$Description[i] <- feature_descriptions[[feature_name]]
        } else {
          feature_importance$Description[i] <- feature_name
        }
      }

      feature_importance <- feature_importance[, c("Original_Name", "Description", "Importance")]
      colnames(feature_importance) <- c("原始特征名", "特征描述", "重要性得分")

      writeData(wb, model_name, feature_importance,
        startRow = current_row + 1, startCol = 1
      )
    }

    setColWidths(wb, model_name, cols = 1:3, widths = "auto")

    hs1 <- createStyle(textDecoration = "bold", fontSize = 12)
    addStyle(wb, model_name, hs1, rows = c(1, 7, 10 + nrow(hyperparams_df)), cols = 1)

    borderStyle <- createStyle(border = "TopBottom", borderColour = "#4F81BD", borderStyle = "medium")
    addStyle(wb, model_name, borderStyle, rows = c(6, 9 + nrow(hyperparams_df)), cols = 1:3, gridExpand = TRUE)
  }

  # 新增：混淆矩阵sheet
  addWorksheet(wb, "混淆矩阵_Train")
  writeData(wb, "混淆矩阵_Train", confusion_train_all)
  setColWidths(wb, "混淆矩阵_Train", cols = seq_len(ncol(confusion_train_all)), widths = "auto")

  addWorksheet(wb, "混淆矩阵_Test")
  writeData(wb, "混淆矩阵_Test", confusion_test_all)
  setColWidths(wb, "混淆矩阵_Test", cols = seq_len(ncol(confusion_test_all)), widths = "auto")

  addWorksheet(wb, "模型比较")

  writeData(wb, "模型比较", performance_comparison, startRow = 1)
  setColWidths(wb, "模型比较", cols = 1:ncol(performance_comparison), widths = "auto")

  headerStyle <- createStyle(
    fgFill = "#4F81BD", halign = "center", textDecoration = "bold",
    fontColour = "white", fontSize = 12
  )
  addStyle(wb, "模型比较", headerStyle, rows = 1, cols = 1:ncol(performance_comparison), gridExpand = TRUE)

  tableStyle <- createStyle(
    border = "TopBottomLeftRight", borderColour = "#4F81BD",
    borderStyle = "thin"
  )
  addStyle(wb, "模型比较", tableStyle,
    rows = 1:(nrow(performance_comparison) + 1),
    cols = 1:ncol(performance_comparison), gridExpand = TRUE
  )

  saveWorkbook(wb, file.path(opts$outdir, "model_performance_comparison.xlsx"), overwrite = TRUE)

  print("已生成model_performance_comparison.xlsx文件，包含以下sheet:")
  print(paste("- 模型sheet:", paste(names(model_details), collapse = ", ")))
  print("- 混淆矩阵sheet: 混淆矩阵_Train, 混淆矩阵_Test")
  print("- 模型比较sheet")

  finalize_workspace_save(opts$outdir)

  write_tsv(
    opts %>% as.matrix() %>% as.data.frame() %>% rownames_to_column() %>%
      as_tibble() %>% mutate(V1 = as.character(V1)),
    str_c(
      "Parameter",
      str_replace_all(as.character(date()), " ", "_") %>% str_replace_all(":", "_"),
      ".xls"
    ),
    col_names = FALSE
  )
} # 结束训练代码的条件判断块

# 11. 外部验证（新增完整模块）-----------------------------------------------

if (opts$valid != "none" && !redraw_only_mode) {
  log_message("=== 开始外部验证分析 ===")

  Valid_Data <- read_tsv(opts$valid) %>% rename_at(1, ~"Features")
  log_message("外部验证数据维度：", nrow(Valid_Data), "特征 x", ncol(Valid_Data) - 1, "样本")

  if (isTRUE(opts$unif)) {
    Valid_Data <- Valid_Data %>% mutate(across(where(is.numeric), ~ . / sum(.)))
    log_message("已对外部验证数据进行归一化处理")
  }

  if (opts$select != "none") {
    ss <- read_tsv(opts$select, col_names = F) %>% rename_at(1, ~"Features")
    Valid_Data <- inner_join(Valid_Data, ss)
    log_message("应用特征选择列表，保留", nrow(Valid_Data), "个特征")
  }
  if (opts$delete != "none") {
    dd <- read_tsv(opts$delete, col_names = F) %>% rename_at(1, ~"Features")
    Valid_Data <- Valid_Data %>% filter(!Features %in% (dd %>% t() %>% as.character()))
    log_message("应用特征删除列表，剩余", nrow(Valid_Data), "个特征")
  }

  if (opts$map2 != "none") {
    Valid_Gp <- read_tsv(opts$map2) %>%
      rename_all(~ c("SampleID", "group")) %>%
      mutate(group = fct_inorder(group))

    if (opts$gp != "none") {
      gp_valid <- strsplit(opts$gp, split = "-")[[1]]
      Valid_Gp <- Valid_Gp %>%
        filter(group %in% gp_valid) %>%
        mutate(group = factor(group, levels = gp_valid))
      log_message("外部验证分组：", paste(gp_valid, collapse = " vs "))
    } else {
      Valid_Gp <- Valid_Gp %>%
        mutate(group = factor(group, levels = gp))
      log_message("外部验证分组：", paste(gp, collapse = " vs "))
    }

    log_message("外部验证样本数：", paste(names(table(Valid_Gp$group)), table(Valid_Gp$group), collapse = ", "))
  } else {
    stop("请提供外部验证分组文件(--map2参数)！")
  }

  Valid_Data <- Valid_Data %>%
    pivot_longer(!Features) %>%
    pivot_wider(names_from = "Features", values_from = "value") %>%
    rename_at(1, ~"SampleID") %>%
    inner_join(Valid_Gp)

  Valid_Data <- as.data.frame(Valid_Data)
  rownames(Valid_Data) <- Valid_Data$SampleID
  Valid_Data <- Valid_Data[, -1]

  common_features <- intersect(
    names_values$old_colnames[-length(names_values$old_colnames)],
    colnames(Valid_Data)[-ncol(Valid_Data)]
  )

  if (length(common_features) == 0) {
    stop("外部验证数据与训练数据没有共同特征！")
  }

  cat("训练数据与外部验证数据的共同特征数：", length(common_features), "\n")

  valid_names_mapping <- names_values[names_values$old_colnames %in% c(common_features, "group"), ]

  Valid_Data_subset <- Valid_Data[, c(common_features, "group")]
  colnames(Valid_Data_subset) <- c(valid_names_mapping$new_colnames[-length(valid_names_mapping$new_colnames)], "group")
  Valid_Data <- Valid_Data_subset

  # 确保与训练阶段预处理管道一致的特征集
  full_training_features <- setdiff(names_values$new_colnames, "group")
  missing_full_features <- setdiff(full_training_features, colnames(Valid_Data))
  if (length(missing_full_features) > 0) {
    if (isTRUE(opts$fill_missing)) {
      for (feat in missing_full_features) {
        Valid_Data[[feat]] <- 0
      }
      cat("已为外部验证数据补全训练阶段缺失特征（用0）：", paste(missing_full_features, collapse = ", "), "\n")
    } else {
      stop(paste0(
        "外部验证数据缺少训练阶段使用的特征，请开启 --fill_missing 以补零。缺失特征：",
        paste(missing_full_features, collapse = ", ")
      ))
    }
  }
  Valid_Data <- Valid_Data[, c(full_training_features, "group")]

  # 此时 Valid_Data 已包含训练阶段使用的全部特征，保持完整输入以匹配预处理管道

  valid_task <- TaskClassif$new(
    id = "external_validation",
    backend = Valid_Data,
    target = "group",
    positive = gp[1]
  )

  # === 检查positive类一致性 ===
  log_message("=== Positive类一致性检查 ===")
  log_message("训练时positive类: ", task_train_selected$positive)
  log_message("验证时positive类: ", valid_task$positive)

  if (task_train_selected$positive != valid_task$positive) {
    log_message("❌ Positive类不一致！", level = "ERROR")
    log_message("这会导致AUC计算错误！", level = "ERROR")
    stop(
      "Positive类定义不一致：训练时=", task_train_selected$positive,
      "，验证时=", valid_task$positive
    )
  } else {
    log_message("✓ Positive类一致")
  }
  log_message("==========================")


  valid_task_prep <- pbp_prep_trained$predict(valid_task)[[1]]
  cat("外部验证预处理后特征数：", length(valid_task_prep$feature_names), "\n")

  features_to_use <- intersect(selected_features, valid_task_prep$feature_names)
  if (length(features_to_use) < length(selected_features)) {
    removed_after_prep <- setdiff(selected_features, features_to_use)
    cat("警告：经训练阶段预处理映射后，外部验证缺少", length(removed_after_prep), "个选中特征：", paste(removed_after_prep, collapse = ", "), "\n")
  }
  if (length(features_to_use) == 0) {
    stop("外部验证数据不包含任何可用于预测的选中特征！")
  }
  valid_task_selected <- valid_task_prep$clone()$select(features_to_use)

  valid_results <- list()
  valid_roc_all <- list()
  confusion_matrices$valid <- list()

  log_message("开始外部验证预测...")
  for (model_name in names(model_results)) {
    log_message(
      "使用", model_name, "模型进行外部验证预测 (",
      match(model_name, names(model_results)), "/", length(model_results), ")"
    )

    current_model <- model_results[[model_name]]$model

    tryCatch(
      {
        # === 详细诊断开始 ===
        log_message("  === ", model_name, " 外部验证详细诊断 ===")

        # 1. 预测
        valid_pred <- current_model$predict(valid_task_selected)

        # 2. 检查positive类和truth水平
        log_message("  Positive类: ", valid_task_selected$positive)
        log_message("  Truth水平: ", paste(levels(valid_pred$truth), collapse = ", "))
        log_message("  概率列名: ", paste(colnames(valid_pred$prob), collapse = ", "))

        # 3. 计算性能（先计算，后面可能需要验证）
        valid_performance <- valid_pred$score(msrs(c("classif.auc", "classif.acc", "classif.ce", "classif.fbeta", "classif.precision", "classif.recall", "classif.mcc")))

        # 4. 安全提取预测概率（增强诊断）
        valid_prob <- tryCatch(
          {
            prob_col <- valid_pred$prob[, valid_task_selected$positive]

            # 转换为numeric
            if (is.list(prob_col)) {
              result <- as.numeric(unlist(prob_col))
            } else if (is.data.frame(prob_col)) {
              result <- as.numeric(unlist(prob_col[, 1]))
            } else {
              result <- as.numeric(prob_col)
            }

            # === 详细诊断 ===
            log_message(
              "  概率范围: ", round(min(result, na.rm = TRUE), 4), " - ",
              round(max(result, na.rm = TRUE), 4)
            )
            log_message("  概率均值: ", round(mean(result, na.rm = TRUE), 4))
            log_message("  概率中位数: ", round(median(result, na.rm = TRUE), 4))
            log_message("  唯一值数量: ", length(unique(result)))

            # 创建truth标签
            truth_int <- as.integer(valid_pred$truth == valid_task_selected$positive)

            # 按truth分组统计
            if (length(unique(truth_int)) >= 2) {
              mean_prob_0 <- mean(result[truth_int == 0], na.rm = TRUE)
              mean_prob_1 <- mean(result[truth_int == 1], na.rm = TRUE)
              log_message(
                "  Truth=0平均prob: ", round(mean_prob_0, 4),
                " (n=", sum(truth_int == 0), ")"
              )
              log_message(
                "  Truth=1平均prob: ", round(mean_prob_1, 4),
                " (n=", sum(truth_int == 1), ")"
              )
              log_message("  两组差异: ", round(abs(mean_prob_1 - mean_prob_0), 4))

              # === 检查异常情况并自动修复 ===
              # 检查1：概率反转 - 自动修复
              if (mean_prob_1 < mean_prob_0 - 0.01) {
                log_message("  ⚠️  检测到概率反转！Truth=1的prob < Truth=0的prob",
                  level = "WARNING"
                )
                log_message("  🔧 自动修复：反转概率 (1 - prob)", level = "WARNING")
                result <- 1 - result
                mean_prob_0 <- mean(result[truth_int == 0], na.rm = TRUE)
                mean_prob_1 <- mean(result[truth_int == 1], na.rm = TRUE)
                log_message("  修复后 Truth=0平均prob: ", round(mean_prob_0, 4))
                log_message("  修复后 Truth=1平均prob: ", round(mean_prob_1, 4))
              }

              # 检查2：概率范围太窄
              prob_range <- max(result, na.rm = TRUE) - min(result, na.rm = TRUE)
              if (prob_range < 0.02) {
                log_message("  ⚠️  概率范围太窄 (", round(prob_range, 4), " < 0.02)！",
                  level = "WARNING"
                )
                log_message("  两组概率几乎完全重叠，AUC应该接近0.5", level = "WARNING")
              }

              # 检查3：两组差异太小
              if (abs(mean_prob_1 - mean_prob_0) < 0.01) {
                log_message("  ⚠️  两组平均概率差异太小 (",
                  round(abs(mean_prob_1 - mean_prob_0), 4), " < 0.01)！",
                  level = "WARNING"
                )
                log_message("  AUC应该接近0.5，不应该是极端值", level = "WARNING")
              }
            } else {
              log_message("  ⚠️  只有一个truth类别！", level = "WARNING")
            }

            # 检查4：所有概率相同
            if (length(unique(result)) == 1) {
              log_message("  ⚠️  所有概率都相同 (", unique(result)[1], ")！",
                level = "WARNING"
              )
              log_message("  AUC将是0.5（完全随机）", level = "WARNING")
            }

            # 检查5：唯一值太少
            if (length(unique(result)) < 3) {
              log_message("  ⚠️  唯一值太少 (", length(unique(result)), " < 3)！",
                level = "WARNING"
              )
            }

            result
          },
          error = function(e) {
            log_message("  ❌ 概率提取失败: ", e$message, level = "ERROR")
            rep(0.5, nrow(valid_task_selected$data()))
          }
        )

        # 5. 创建ROC数据
        valid_roc_data <- data.frame(
          truth = as.integer(valid_pred$truth == valid_task_selected$positive),
          prob = valid_prob,
          dataset = "External_Validation"
        )

        # 6. 重新计算AUC（使用修复后的概率）
        manual_roc <- tryCatch(
          {
            if (length(unique(valid_roc_data$truth)) >= 2 &&
              length(unique(valid_roc_data$prob)) > 1) {
              pROC::roc(valid_roc_data$truth, valid_roc_data$prob,
                direction = "<", quiet = TRUE
              )
            } else {
              NULL
            }
          },
          error = function(e) NULL
        )

        if (!is.null(manual_roc)) {
          manual_auc <- as.numeric(pROC::auc(manual_roc))
          log_message("  重新计算的AUC: ", round(manual_auc, 4))

          # 使用重新计算的AUC替换原始AUC
          auc_value <- manual_auc
          valid_performance["classif.auc"] <- manual_auc
          log_message("  ✓ 使用修复后的AUC: ", round(auc_value, 4))
        } else {
          auc_value <- valid_performance["classif.auc"]
          log_message("  报告的AUC: ", round(auc_value, 4))
        }

        log_message("  === 诊断结束 ===")
        # === 详细诊断结束 ===

        # 计算验证集混淆矩阵
        threshold_used <- model_results[[model_name]]$threshold_used
        valid_thr <- perf_with_threshold(valid_roc_data, threshold_used)

        cm_valid_df <- tibble(
          Model = model_name, Dataset = "valid", Threshold = threshold_used,
          TP = valid_thr$cm["TP"], FP = valid_thr$cm["FP"], TN = valid_thr$cm["TN"], FN = valid_thr$cm["FN"],
          ACC = valid_thr$acc, CE = valid_thr$ce,
          Precision = valid_thr$precision, Recall = valid_thr$recall, F1 = valid_thr$f1
        )
        confusion_matrices$valid[[model_name]] <- cm_valid_df

        valid_results[[model_name]] <- list(
          model_name = model_name,
          performance = valid_performance,
          performance_thr = c(
            classif.acc = valid_thr$acc,
            classif.ce = valid_thr$ce,
            classif.precision = valid_thr$precision,
            classif.recall = valid_thr$recall,
            classif.f1 = valid_thr$f1
          ),
          roc_data = valid_roc_data
        )

        # 安全处理valid_roc_data（与train/test保持一致）
        valid_roc_all[[model_name]] <- tryCatch(
          {
            if (!is.null(valid_roc_data) && nrow(valid_roc_data) > 0) {
              valid_roc_data %>% mutate(model = model_name)
            } else {
              log_message("  ⚠️  ", model_name, "的valid_roc_data为空", level = "WARNING")
              NULL
            }
          },
          error = function(e) {
            log_message("  ⚠️  ", model_name, "的valid_roc_data处理失败：", e$message, level = "WARNING")
            NULL
          }
        )

        log_message(
          "  ✓ AUC:", round(valid_performance["classif.auc"], 4),
          "ACC:", round(valid_performance["classif.acc"], 4),
          "CE:", round(valid_performance["classif.ce"], 4)
        )
      },
      error = function(e) {
        log_message("  ✗ 预测失败：", e$message, level = "ERROR")
      }
    )
  }

  if (length(valid_results) == 0) {
    stop("所有模型的外部验证预测都失败了！")
  }

  # 移除NULL条目（与train/test保持一致）
  valid_roc_all <- valid_roc_all[!sapply(valid_roc_all, is.null)]
  log_message("有效的valid_roc模型数：", length(valid_roc_all))

  if (length(valid_roc_all) == 0) {
    log_message("⚠️  所有模型的valid_roc_data都为空，跳过ROC曲线绘制", level = "WARNING")
  } else {
    cat("\n生成外部验证ROC曲线...\n")
    valid_roc_plot <- roc_pict(valid_roc_all,
      title = "外部验证ROC曲线", path = file.path(opts$outdir, "valid_roc_plot"),
      colors = model_colors, labels = names(valid_roc_all)
    )

    # 新增：外部验证PR曲线
    valid_pr_plot <- pr_pict(valid_roc_all,
      title = "外部验证PR曲线", path = file.path(opts$outdir, "valid_pr_plot"),
      colors = model_colors, labels = names(valid_roc_all)
    )
  }

  valid_performance_comparison <- data.frame(
    Model = character(),
    Valid_AUC = numeric(),
    Valid_ACC = numeric(),
    Valid_ACC_thr = numeric(),
    Valid_CE = numeric(),
    Valid_CE_thr = numeric(),
    Valid_Precision = numeric(),
    Valid_Precision_thr = numeric(),
    Valid_Recall = numeric(),
    Valid_Recall_thr = numeric(),
    Valid_F1 = numeric(),
    Valid_F1_thr = numeric(),
    Valid_MCC = numeric(),
    stringsAsFactors = FALSE
  )

  for (model_name in names(valid_results)) {
    valid_performance_comparison <- rbind(
      valid_performance_comparison,
      data.frame(
        Model = model_name,
        Valid_AUC = valid_results[[model_name]]$performance["classif.auc"],
        Valid_ACC = valid_results[[model_name]]$performance["classif.acc"],
        Valid_ACC_thr = valid_results[[model_name]]$performance_thr["classif.acc"],
        Valid_CE = valid_results[[model_name]]$performance["classif.ce"],
        Valid_CE_thr = valid_results[[model_name]]$performance_thr["classif.ce"],
        Valid_Precision = valid_results[[model_name]]$performance["classif.precision"],
        Valid_Precision_thr = valid_results[[model_name]]$performance_thr["classif.precision"],
        Valid_Recall = valid_results[[model_name]]$performance["classif.recall"],
        Valid_Recall_thr = valid_results[[model_name]]$performance_thr["classif.recall"],
        Valid_F1 = valid_results[[model_name]]$performance["classif.fbeta"],
        Valid_F1_thr = valid_results[[model_name]]$performance_thr["classif.f1"],
        Valid_MCC = valid_results[[model_name]]$performance["classif.mcc"]
      )
    )
  }

  # 新增：计算外部验证AUPRC
  valid_auprc_results <- data.frame(Model = character(), Valid_AUPRC = numeric())
  for (model_name in names(valid_results)) {
    valid_pred_obj <- ROCR::prediction(
      valid_results[[model_name]]$roc_data$prob,
      factor(valid_results[[model_name]]$roc_data$truth, levels = c(0, 1))
    )
    valid_auprc <- ROCR::performance(valid_pred_obj, "aucpr")@y.values[[1]]
    valid_auprc_results <- rbind(valid_auprc_results, data.frame(
      Model = model_name, Valid_AUPRC = valid_auprc
    ))
  }
  valid_performance_comparison <- left_join(valid_performance_comparison, valid_auprc_results, by = "Model")

  # 合并验证集混淆矩阵
  confusion_valid_all <- enrich_confusion_metrics(bind_rows(confusion_matrices$valid))

  # 创建验证集汇总数据
  all_valid_data <- bind_rows(valid_roc_all, .id = "source_model")

  # 更新boxplot_data.xlsx，添加验证集数据
  boxplot_data_list[["Valid_All_Models"]] <- all_valid_data # 添加汇总sheet
  for (model_name in names(valid_roc_all)) {
    boxplot_data_list[[paste0("Valid_", model_name)]] <- valid_roc_all[[model_name]]
  }
  write.xlsx(boxplot_data_list, file = file.path(opts$outdir, "boxplot_data.xlsx"))
  log_message("已更新boxplot_data.xlsx，包含", length(valid_roc_all), "个模型的外部验证数据")

  valid_performance_comparison <- clean_threshold_performance_table(
    valid_performance_comparison,
    list(Valid = confusion_valid_all)
  )

  performance_comparison_with_valid <- left_join(performance_comparison, valid_performance_comparison)

  cat("\n生成各模型外部验证详细图表...\n")
  for (i in 1:length(names(valid_roc_all))) {
    model_name <- names(valid_roc_all)[i]
    cat("生成", model_name, "模型外部验证图表...\n")

    threshold_used <- if (!is.null(model_results[[model_name]])) model_results[[model_name]]$threshold_used else NA
    p_valid <- roc_box_plot(valid_roc_all[[model_name]], title = "External Validation", label = model_name, threshold = threshold_used)
    pr_valid <- pr_box_plot(valid_roc_all[[model_name]], title = "External Validation", label = model_name, threshold = threshold_used)

    p_valid[[3]] %>%
      broom::tidy() %>%
      mutate(data = "external_validation") %>%
      write_tsv(file.path(opts$outdir, paste0("step4.", model_name, "_external_validation_wilcox_test.tsv")))

    if (exists("Plot_data4") && model_name %in% names(Plot_data4)) {
      p4_valid <- plot_p_feat(Plot_data4[[model_name]], out = opts$outdir, plot = FALSE)
    } else {
      p4_valid <- ggplot() +
        theme_void() +
        labs(title = "特征重要性不可用")
    }

    ComBn_plot(list(p_valid[1:2]),
      w = p4_valid, out_parm = opts$outdir,
      biomaker_num_fix_parm = paste0(model_name, "_external_validation_roc")
    )

    ComBn_plot(list(pr_valid[1:2]),
      w = p4_valid, out_parm = opts$outdir,
      biomaker_num_fix_parm = paste0(model_name, "_external_validation_pr")
    )
  }

  cat("\n更新Excel报告...\n")

  # 创建只包含Valid信息的数据框，明确指定列顺序（与Test列顺序一致）
  valid_only_comparison <- valid_performance_comparison %>%
    select(
      Model,
      Valid_AUC,
      Valid_Threshold,
      Valid_ACC,
      Valid_CE,
      Valid_Precision,
      Valid_Recall,
      Valid_Sensitivity,
      Valid_Specificity,
      Valid_F1,
      Valid_MCC
    )

  # 添加AUPRC（最后一列）
  valid_only_comparison <- left_join(valid_only_comparison,
    valid_auprc_results %>% select(Model, Valid_AUPRC),
    by = "Model"
  )

  if ("外部验证比较" %in% names(wb)) {
    removeWorksheet(wb, "外部验证比较")
  }
  addWorksheet(wb, "外部验证比较")
  writeData(wb, "外部验证比较", valid_only_comparison, startRow = 1)
  setColWidths(wb, "外部验证比较", cols = 1:ncol(valid_only_comparison), widths = "auto")

  headerStyle <- createStyle(
    fgFill = "#4F81BD", halign = "center", textDecoration = "bold",
    fontColour = "white", fontSize = 12
  )
  addStyle(wb, "外部验证比较", headerStyle, rows = 1, cols = 1:ncol(valid_only_comparison), gridExpand = TRUE)

  tableStyle <- createStyle(
    border = "TopBottomLeftRight", borderColour = "#4F81BD",
    borderStyle = "thin"
  )
  addStyle(wb, "外部验证比较", tableStyle,
    rows = 1:(nrow(valid_only_comparison) + 1),
    cols = 1:ncol(valid_only_comparison), gridExpand = TRUE
  )

  valid_cols <- grep("Valid_", colnames(valid_only_comparison))
  for (col in valid_cols) {
    conditionalFormatting(wb, "外部验证比较",
      cols = col,
      rows = 2:(nrow(valid_only_comparison) + 1),
      type = "colorScale",
      style = c("#E41A1C", "#FFFFFF", "#4F81BD")
    )
  }

  # 添加验证集混淆矩阵sheet
  if ("混淆矩阵_Valid" %in% names(wb)) {
    removeWorksheet(wb, "混淆矩阵_Valid")
  }
  addWorksheet(wb, "混淆矩阵_Valid")
  writeData(wb, "混淆矩阵_Valid", confusion_valid_all)
  setColWidths(wb, "混淆矩阵_Valid", cols = seq_len(ncol(confusion_valid_all)), widths = "auto")

  for (model_name in names(valid_results)) {
    if (model_name %in% names(wb)) {
      # 从model_details获取信息，手动计算行号
      info <- model_details[[model_name]]

      # 重新计算performance_data的行数（与生成模型sheet时一致）
      performance_data <- performance_comparison %>%
        filter(Model == model_name) %>%
        pivot_longer(
          cols = starts_with(c("CV_", "Train_", "Test_")),
          names_to = "Metric_Split",
          values_to = "Value"
        ) %>%
        separate(
          col = "Metric_Split",
          into = c("Data_Split", "Metric"),
          sep = "_",
          extra = "merge", fill = "right"
        ) %>%
        pivot_wider(
          names_from = "Data_Split",
          values_from = "Value"
        ) %>%
        select(-Model)

      perf_data_rows <- nrow(performance_data)
      hyperparams_rows <- length(info$hyperparameters)
      feature_imp_rows <- if (!is.null(info$feature_importance)) nrow(info$feature_importance) else 0

      # 计算start_row
      # 1 (标题"模型性能") + perf_data_rows + 2 (空行) + 1 (标题"最优超参数") + hyperparams_rows + 2 (空行)
      start_row <- 1 + perf_data_rows + 2 + 1 + hyperparams_rows + 2

      # 如果有特征重要性，再加上特征重要性的行数
      if (feature_imp_rows > 0) {
        start_row <- start_row + 1 + feature_imp_rows + 2
      }

      writeData(wb, model_name, "外部验证性能", startRow = start_row, startCol = 1)

      valid_row <- valid_performance_comparison %>%
        filter(Model == model_name)
      valid_perf_data <- data.frame(
        Metric = c(
          "Threshold", "AUC", "Accuracy", "Classification Error",
          "Precision", "Recall", "Sensitivity", "Specificity", "F1", "MCC", "AUPRC"
        ),
        External_Validation = c(
          valid_row$Valid_Threshold,
          valid_row$Valid_AUC,
          valid_row$Valid_ACC,
          valid_row$Valid_CE,
          valid_row$Valid_Precision,
          valid_row$Valid_Recall,
          valid_row$Valid_Sensitivity,
          valid_row$Valid_Specificity,
          valid_row$Valid_F1,
          valid_row$Valid_MCC,
          valid_row$Valid_AUPRC
        )
      )

      writeData(wb, model_name, valid_perf_data, startRow = start_row + 1, startCol = 1)

      hs1 <- createStyle(textDecoration = "bold", fontSize = 12)
      addStyle(wb, model_name, hs1, rows = start_row, cols = 1)
    }
  }

  saveWorkbook(wb, file.path(opts$outdir, "model_performance_comparison.xlsx"), overwrite = TRUE)

  log_message("=== 外部验证分析完成！ ===")
  log_message("生成的外部验证文件：")
  log_message("- 外部验证ROC曲线：valid_roc_plot.pdf")
  log_message("- 外部验证PR曲线：valid_pr_plot.pdf")
  log_message("- 各模型外部验证详细图表：step4.[model_name]_external_validation_roc.patchwork.pdf")
  log_message("- 各模型外部验证PR图表：step4.[model_name]_external_validation_pr.patchwork.pdf")
  log_message("- 外部验证统计检验结果：step4.[model_name]_external_validation_wilcox_test.tsv")
  log_message("- 更新的Excel报告已包含外部验证结果（包含混淆矩阵_Valid sheet）")
  log_message("- 更新的boxplot_data.xlsx已包含验证集数据")

  log_message("外部验证性能总结：")
  print(valid_performance_comparison)
} else {
  log_message("未提供外部验证数据(--valid参数)，跳过外部验证步骤")
}

# 生成分析流程图
if (opts$generate_flowchart && !redraw_only_mode) {
  # 收集分析信息
  analysis_info <- list(
    n_samples = nrow(Data),
    n_features_original = ncol(Data) - 1, # 减去group列
    n_features_selected = length(selected_features),
    train_samples = nrow(task_train$data()),
    test_samples = nrow(task_test$data()),
    feature_selection_method = method_used, # 使用实际的特征选择方法
    knee_detection_method = detection_method,
    use_loocv = use_loocv_final,
    loocv_scope = loocv_scope_final,
    models = names(auto_tuners),
    best_model = best_learner_name
  )

  # 生成流程图
  generate_analysis_flowchart(opts, analysis_info, opts$outdir)
}

if (!redraw_only_mode) {
  log_message("保存完整分析结果（包含外部验证）...")
  finalize_workspace_save(opts$outdir)
} else {
  log_message("重绘模式跳过保存efs.RData")
}
log_message("=== 分析完成！ ===")
