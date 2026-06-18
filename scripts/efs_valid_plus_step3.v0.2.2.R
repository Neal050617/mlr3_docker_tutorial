# =============================================================================
# EFS Step3: SHAP Analysis
# 基于 Step2 输出的 efs.RData，对已训练模型做 SHAP 解释分析
# =============================================================================

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

required_pkgs <- c(
  "optparse", "tidyverse", "kernelshap", "shapviz", "openxlsx", "showtext",
  "mlr3verse", "mlr3learners", "mlr3tuning", "mlr3extralearners",
  "ranger", "xgboost", "glmnet", "e1071", "nnet", "kknn", "rpart", "RWeka",
  "naivebayes", "ggplot2", "smotefamily"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Step3 缺少依赖包：", paste(missing_pkgs, collapse = ", "),
    "。请先在当前 R 环境中安装后再运行。"
  )
}

invisible(lapply(required_pkgs, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

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

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

cleanup_numbered_pdf_siblings <- function(filename) {
  if (!grepl("\\.pdf$", filename, ignore.case = TRUE)) return(invisible(NULL))
  parent <- dirname(filename)
  stem <- sub("\\.pdf$", "", basename(filename), ignore.case = TRUE)
  pattern <- paste0("^", escape_regex(stem), " [0-9]+\\.pdf$")
  extras <- list.files(parent, pattern = pattern, full.names = TRUE)
  if (length(extras)) unlink(extras)
  invisible(NULL)
}

save_pdf_plot <- function(filename, plot_obj, width, height, units = "in", dpi = 300, ...) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  cleanup_numbered_pdf_siblings(filename)
  tmp_file <- paste0(filename, ".tmp")
  if (file.exists(tmp_file)) {
    unlink(tmp_file)
  }

  if (nzchar(plot_font_family) && capabilities("cairo")) {
    ggplot2::ggsave(
      filename = tmp_file,
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
      filename = tmp_file,
      plot = plot_obj,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      ...
    )
  }

  if (file.exists(filename)) {
    unlink(filename)
  }
  ok <- file.rename(tmp_file, filename)
  if (!ok) {
    stop("无法将临时 PDF 文件改名为目标文件：", filename)
  }
  cleanup_numbered_pdf_siblings(filename)
}

option_list <- list(
  make_option(c("--load_rdata"), type = "character", default = "none", help = "Step2 生成的 efs.RData 文件路径"),
  make_option(c("--models"), type = "character", default = "all", help = "指定模型，逗号分隔；默认 all"),
  make_option(c("--datasets"), type = "character", default = "train,test,valid", help = "指定数据集，逗号分隔；默认 train,test,valid"),
  make_option(c("--require_all"), type = "logical", default = FALSE, help = "是否要求所有请求的模型/数据集组合都必须成功完成"),
  make_option(c("--skip_poor_models"), type = "logical", default = TRUE, help = "是否跳过效果较差的模型并输出说明文件"),
  make_option(c("--min_auc_for_shap"), type = "double", default = 0.65, help = "低于该 AUC 阈值的模型默认跳过 SHAP"),
  make_option(c("--skip_zero_recall_models"), type = "logical", default = TRUE, help = "是否跳过在 valid/test 上 Recall<=0 的退化模型"),
  make_option(c("--shap_metric_priority"), type = "character", default = "valid,test,cv", help = "判定模型是否值得做 SHAP 的指标优先级"),
  make_option(c("--display_features"), type = "integer", default = 0, help = "SHAP 图显示的特征数；<=0 表示自动"),
  make_option(c("--auto_display_cap"), type = "integer", default = 30, help = "自动模式下的最大展示特征数；默认 30"),
  make_option(c("--local_display_features"), type = "integer", default = 0, help = "waterfall/force 单样本图显示的特征数；<=0 表示沿用 --display_features"),
  make_option(c("--feature_label_width"), type = "integer", default = 28, help = "SHAP 图特征名超过该字符数后按 '-' 优先断为最多两行"),
  make_option(c("--waterfall_samples"), type = "character", default = "", help = "指定 waterfall/force 样本ID，逗号分隔；支持 train:ID、test:ID、valid:ID 或不带数据集前缀"),
  make_option(c("--waterfall_groups"), type = "character", default = "case,control", help = "未指定样本ID时按组自动选择 waterfall/force 样本；支持 case,control 或实际组名；空字符串表示禁用"),
  make_option(c("--waterfall_per_group"), type = "integer", default = 1, help = "按组自动选择时每组输出多少个样本图"),
  make_option(c("--waterfall_default_n"), type = "integer", default = 1, help = "未指定样本且无法按组选择时，默认输出前多少个解释样本的 waterfall/force 图"),
  make_option(c("--force_plots"), type = "logical", default = TRUE, help = "是否为 waterfall 目标样本同时输出 force 图"),
  make_option(c("--nsim_train"), type = "integer", default = 50, help = "训练集 SHAP 近似采样步数"),
  make_option(c("--nsim_test"), type = "integer", default = 30, help = "测试集 SHAP 近似采样步数"),
  make_option(c("--nsim_valid"), type = "integer", default = 30, help = "验证集 SHAP 近似采样步数"),
  make_option(c("--max_train_rows"), type = "integer", default = 100, help = "训练集最多解释多少样本"),
  make_option(c("--max_test_rows"), type = "integer", default = 100, help = "测试集最多解释多少样本"),
  make_option(c("--max_valid_rows"), type = "integer", default = 100, help = "验证集最多解释多少样本"),
  make_option(c("--background_rows"), type = "integer", default = 200, help = "背景数据最多使用多少训练样本"),
  make_option(c("--seed"), type = "integer", default = 115702, help = "随机种子"),
  make_option(c("-o", "--outdir"), type = "character", default = "", help = "输出目录；默认位于 load_rdata 同级的 step3_shap")
)

opts <- parse_args(OptionParser(option_list = option_list))
runtime_opts <- opts

if (opts$load_rdata == "none") {
  stop("Step3 需要提供 --load_rdata=step2输出的efs.RData")
}
if (!file.exists(opts$load_rdata)) {
  stop("指定的RData文件不存在：", opts$load_rdata)
}

if (opts$outdir == "") {
  opts$outdir <- file.path(dirname(opts$load_rdata), "step3_shap")
}
if (!dir.exists(opts$outdir)) {
  dir.create(opts$outdir, recursive = TRUE)
}

set.seed(opts$seed)

log_file <- file.path(opts$outdir, paste0("run_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
log_con <- file(log_file, open = "wt")

log_message <- function(..., level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste0("[", timestamp, "] [", level, "] ", paste(..., collapse = " "))
  cat(msg, "\n")
  tryCatch(
    {
      cat(msg, "\n", file = log_con)
      flush(log_con)
    },
    error = function(e) NULL
  )
}

on.exit({
  try(close(log_con), silent = TRUE)
}, add = TRUE)

parse_csv_arg <- function(x, allow_all = TRUE) {
  if (is.null(x) || !nzchar(x)) return(character())
  vals <- strsplit(x, ",", fixed = TRUE)[[1]] %>%
    trimws() %>%
    discard(~ .x == "")
  if (allow_all && length(vals) == 1 && tolower(vals) == "all") {
    return("all")
  }
  vals
}

sample_rows_df <- function(df, max_rows) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (is.na(max_rows) || max_rows <= 0 || nrow(df) <= max_rows) return(df)
  sample_groups <- attr(df, "sample_groups")
  sampled_idx <- sample(seq_len(nrow(df)), size = max_rows)
  sampled_df <- df[sampled_idx, , drop = FALSE]
  attr(sampled_df, "sample_ids") <- attr(df, "sample_ids")[sampled_idx]
  if (!is.null(sample_groups)) {
    attr(sampled_df, "sample_groups") <- sample_groups[sampled_idx]
  }
  sampled_df
}

sample_rows_df_keep_ids <- function(df, max_rows, keep_ids = character()) {
  if (is.null(df) || nrow(df) == 0) return(df)
  sample_ids <- attr(df, "sample_ids")
  sample_groups <- attr(df, "sample_groups")
  if (is.null(sample_ids)) {
    sample_ids <- rownames(df)
  }
  if (is.null(sample_ids)) {
    sample_ids <- paste0("row_", seq_len(nrow(df)))
  }
  attr(df, "sample_ids") <- sample_ids

  keep_ids <- unique(keep_ids[keep_ids %in% sample_ids])
  keep_idx <- match(keep_ids, sample_ids)
  keep_idx <- keep_idx[!is.na(keep_idx)]

  if (is.na(max_rows) || max_rows <= 0 || nrow(df) <= max_rows) {
    return(df)
  }

  if (length(keep_idx) >= max_rows) {
    sampled_idx <- keep_idx[seq_len(max_rows)]
  } else {
    remaining_idx <- setdiff(seq_len(nrow(df)), keep_idx)
    extra_n <- max_rows - length(keep_idx)
    extra_idx <- if (extra_n > 0) sample(remaining_idx, size = extra_n) else integer()
    sampled_idx <- c(keep_idx, extra_idx)
  }

  sampled_df <- df[sampled_idx, , drop = FALSE]
  attr(sampled_df, "sample_ids") <- sample_ids[sampled_idx]
  if (!is.null(sample_groups)) {
    attr(sampled_df, "sample_groups") <- sample_groups[sampled_idx]
  }
  sampled_df
}

write_xlsx_atomic <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tmp_file <- paste0(file, ".tmp")
  if (file.exists(tmp_file)) {
    unlink(tmp_file)
  }
  openxlsx::write.xlsx(x, tmp_file)
  ok <- file.rename(tmp_file, file)
  if (!ok) {
    stop("无法将临时 Excel 文件改名为目标文件：", file)
  }
}

map_feature_labels <- function(feature_names, mapping_df) {
  if (is.null(mapping_df) || length(feature_names) == 0) {
    return(feature_names)
  }
  mapping_df <- mapping_df %>%
    filter(new_colnames %in% feature_names) %>%
    distinct(new_colnames, .keep_all = TRUE)
  if (nrow(mapping_df) == 0) {
    return(feature_names)
  }
  name_map <- setNames(mapping_df$old_colnames, mapping_df$new_colnames)
  mapped <- unname(name_map[feature_names])
  mapped[is.na(mapped)] <- feature_names[is.na(mapped)]
  mapped
}

resolve_trained_learner <- function(model_entry) {
  if (is.null(model_entry) || is.null(model_entry$model)) {
    return(NULL)
  }
  model_entry$model
}

get_positive_class <- function(trained_learner) {
  if (exists("task_train_selected", inherits = TRUE) &&
      !is.null(get("task_train_selected", inherits = TRUE)$positive)) {
    return(get("task_train_selected", inherits = TRUE)$positive)
  }
  if (exists("task_train", inherits = TRUE) &&
      !is.null(get("task_train", inherits = TRUE)$positive)) {
    return(get("task_train", inherits = TRUE)$positive)
  }
  if (!is.null(trained_learner$state$train_task) &&
      !is.null(trained_learner$state$train_task$positive)) {
    return(trained_learner$state$train_task$positive)
  }
  NULL
}

resolve_native_state <- function(trained_learner, model_name = NULL) {
  learner <- trained_learner
  if (inherits(learner, "AutoTuner") &&
      !is.null(learner$model) &&
      !is.null(learner$model$learner)) {
    learner <- learner$model$learner
  }

  if (!inherits(learner, "GraphLearner") &&
      !is.null(model_name) &&
      nzchar(model_name) &&
      !is.null(learner$state) &&
      !is.null(learner$state$model)) {
    state <- learner$state
    if (identical(model_name, "adaboost")) {
      state <- refresh_adaboost_state_for_shap(state, learner)
    }
    return(list(model_name = model_name, state = state))
  }

  if (!inherits(learner, "GraphLearner") || is.null(learner$model)) {
    return(NULL)
  }

  graph_model <- learner$model
  candidate_names <- unique(c(model_name, names(graph_model)))
  candidate_names <- candidate_names[!is.na(candidate_names) & nzchar(candidate_names)]
  candidate_names <- setdiff(candidate_names, "smote")

  for (candidate_name in candidate_names) {
    state <- graph_model[[candidate_name]]
    if (!is.null(state) && !is.null(state$model)) {
      if (identical(candidate_name, "adaboost")) {
        state <- refresh_adaboost_state_for_shap(state, learner)
      }
      return(list(model_name = candidate_name, state = state))
    }
  }

  NULL
}

adaboost_state_predictable <- function(state) {
  if (is.null(state) || is.null(state$model) || is.null(state$feature_names)) {
    return(FALSE)
  }
  if (!exists("task_train_selected", inherits = TRUE)) {
    return(FALSE)
  }
  probe_task <- get("task_train_selected", inherits = TRUE)
  probe <- as.data.frame(probe_task$data(cols = state$feature_names)[1, , drop = FALSE], check.names = FALSE)
  ok <- tryCatch(
    {
      stats::predict(state$model, probe, type = "probability")
      TRUE
    },
    error = function(e) FALSE
  )
  isTRUE(ok)
}

refresh_adaboost_state_for_shap <- function(state, graph_learner) {
  if (adaboost_state_predictable(state)) {
    return(state)
  }
  if (!exists("task_train_selected", inherits = TRUE)) {
    stop("adaboost模型无法从RData直接预测，且缺少task_train_selected，无法为SHAP重建模型")
  }

  if (exists("log_message", mode = "function", inherits = TRUE)) {
    get("log_message", mode = "function", inherits = TRUE)(
      "adaboost模型RData反序列化后不可预测；使用保存的调参参数重建用于SHAP的模型",
      level = "WARNING"
    )
  }

  train_task <- get("task_train_selected", inherits = TRUE)$clone()
  train_task$select(state$feature_names)

  adaboost <- lrn("classif.AdaBoostM1", id = "adaboost", predict_type = "prob")
  tuned_i <- NULL
  if (!is.null(state$param_vals) && !is.null(state$param_vals$I)) {
    tuned_i <- state$param_vals$I
  } else if (!is.null(graph_learner$param_set$values[["adaboost.I"]])) {
    tuned_i <- graph_learner$param_set$values[["adaboost.I"]]
  }
  if (!is.null(tuned_i)) {
    adaboost$param_set$values$I <- tuned_i
  }

  smote_k <- graph_learner$param_set$values[["smote.K"]]
  smote_dup_size <- graph_learner$param_set$values[["smote.dup_size"]]

  set.seed(opts$seed)
  if (!is.null(smote_k) && !is.null(smote_dup_size)) {
    refreshed_learner <- as_learner(po("smote", K = smote_k, dup_size = smote_dup_size) %>>% adaboost)
    refreshed_learner$train(train_task)
    refreshed_state <- refreshed_learner$model[["adaboost"]]
  } else {
    adaboost$train(train_task)
    refreshed_state <- adaboost$state
  }

  attr(refreshed_state, "retrained_for_shap") <- TRUE
  refreshed_state
}

predict_native_state <- function(native_state, newdata, positive_class = NULL) {
  model_name <- native_state$model_name
  state <- native_state$state
  feature_names <- state$feature_names
  newdata_df <- as.data.frame(newdata, check.names = FALSE)
  if (!is.null(feature_names)) {
    newdata_df <- newdata_df[, feature_names, drop = FALSE]
  }

  if (identical(model_name, "glmnet")) {
    prob <- as.numeric(stats::predict(
      state$model,
      newx = as.matrix(newdata_df),
      type = "response"
    ))
    model_positive <- state$model$classnames[2]
    if (!is.null(positive_class) && !is.null(model_positive) && positive_class != model_positive) {
      prob <- 1 - prob
    }
    return(prob)
  }

  if (identical(model_name, "xgboost")) {
    booster <- attr(state$model[[1]], "model")
    if (is.null(booster)) {
      stop("xgboost底层booster为空")
    }
    prob <- as.numeric(stats::predict(
      booster,
      xgboost::xgb.DMatrix(data = as.matrix(newdata_df))
    ))
    model_positive <- state$train_task$class_names[1]
    if (!is.null(positive_class) && !is.null(model_positive) && positive_class != model_positive) {
      prob <- 1 - prob
    }
    return(prob)
  }

  if (identical(model_name, "rf")) {
    pred_obj <- stats::predict(state$model, data = newdata_df)
    prob_matrix <- pred_obj$predictions
    if (is.null(prob_matrix)) {
      stop("rf未返回概率矩阵")
    }
    if (is.null(dim(prob_matrix))) {
      prob_matrix <- matrix(prob_matrix, ncol = 1)
    }
    target_col <- positive_class
    if (is.null(target_col) || is.null(colnames(prob_matrix)) || !target_col %in% colnames(prob_matrix)) {
      target_col <- if (!is.null(colnames(prob_matrix))) colnames(prob_matrix)[ncol(prob_matrix)] else ncol(prob_matrix)
    }
    return(as.numeric(prob_matrix[, target_col]))
  }

  if (identical(model_name, "svm")) {
    pred <- stats::predict(state$model, newdata_df, probability = TRUE)
    prob_matrix <- attr(pred, "probabilities")
    if (is.null(prob_matrix)) {
      stop("svm未返回概率矩阵")
    }
    target_col <- positive_class
    if (is.null(target_col) || !target_col %in% colnames(prob_matrix)) {
      target_col <- colnames(prob_matrix)[ncol(prob_matrix)]
    }
    return(as.numeric(prob_matrix[, target_col]))
  }

  if (identical(model_name, "rpart")) {
    prob_matrix <- stats::predict(state$model, newdata_df, type = "prob")
    target_col <- positive_class
    if (is.null(target_col) || !target_col %in% colnames(prob_matrix)) {
      target_col <- colnames(prob_matrix)[ncol(prob_matrix)]
    }
    return(as.numeric(prob_matrix[, target_col]))
  }

  if (identical(model_name, "naive_bayes")) {
    prob_matrix <- stats::predict(state$model, newdata_df, type = "raw")
    target_col <- positive_class
    if (is.null(target_col) || !target_col %in% colnames(prob_matrix)) {
      target_col <- colnames(prob_matrix)[ncol(prob_matrix)]
    }
    return(as.numeric(prob_matrix[, target_col]))
  }

  if (identical(model_name, "nnet")) {
    prob_matrix <- stats::predict(state$model, newdata_df, type = "raw")
    if (is.null(dim(prob_matrix))) {
      model_positive <- state$model$lev[2]
      prob <- as.numeric(prob_matrix)
      if (!is.null(positive_class) && !is.null(model_positive) && positive_class != model_positive) {
        prob <- 1 - prob
      }
      return(prob)
    }
    if (ncol(prob_matrix) == 1L) {
      model_positive <- state$model$lev[2]
      prob <- as.numeric(prob_matrix[, 1])
      if (!is.null(positive_class) && !is.null(model_positive) && positive_class != model_positive) {
        prob <- 1 - prob
      }
      return(prob)
    }
    target_col <- positive_class
    if (is.null(target_col) || !target_col %in% colnames(prob_matrix)) {
      target_col <- colnames(prob_matrix)[ncol(prob_matrix)]
    }
    return(as.numeric(prob_matrix[, target_col]))
  }

  if (identical(model_name, "kknn")) {
    if (is.null(state$model$formula) || is.null(state$model$data) || is.null(state$model$pv)) {
      stop("kknn底层模型缺少公式、训练数据或参数")
    }
    fit <- kknn::kknn(
      formula = state$model$formula,
      train = as.data.frame(state$model$data, check.names = FALSE),
      test = newdata_df,
      k = state$model$pv$k,
      distance = state$model$pv$distance,
      kernel = state$model$pv$kernel
    )
    prob_matrix <- fit$prob
    if (is.null(prob_matrix)) {
      stop("kknn未返回概率矩阵")
    }
    target_col <- positive_class
    if (is.null(target_col) || is.null(colnames(prob_matrix)) || !target_col %in% colnames(prob_matrix)) {
      target_col <- if (!is.null(colnames(prob_matrix))) colnames(prob_matrix)[ncol(prob_matrix)] else ncol(prob_matrix)
    }
    return(as.numeric(prob_matrix[, target_col]))
  }

  if (identical(model_name, "adaboost")) {
    prob_matrix <- stats::predict(state$model, newdata_df, type = "probability")
    target_col <- positive_class
    if (is.null(target_col) || !target_col %in% colnames(prob_matrix)) {
      target_col <- colnames(prob_matrix)[ncol(prob_matrix)]
    }
    return(as.numeric(prob_matrix[, target_col]))
  }

  stop("未支持的底层模型原生预测：", model_name)
}

predict_prob_wrapper <- function(trained_learner, positive_class = NULL, model_name = NULL) {
  force(positive_class)
  native_state <- resolve_native_state(trained_learner, model_name)
  if (!is.null(native_state)) {
    force(native_state)
  }
  function(object, newdata) {
    newdata_df <- as.data.frame(newdata, check.names = FALSE)
    if (!is.null(native_state)) {
      native_prob <- tryCatch(
        predict_native_state(native_state, newdata_df, positive_class),
        error = function(e) {
          if (grepl("未支持的底层模型原生预测", conditionMessage(e), fixed = TRUE)) {
            return(NULL)
          }
          stop(e)
        }
      )
      if (!is.null(native_prob)) {
        return(native_prob)
      }
    }

    pred <- object$predict_newdata(newdata_df)

    if (!is.null(pred$prob)) {
      prob_matrix <- pred$prob
      target_col <- positive_class
      if (is.null(target_col) || !target_col %in% colnames(prob_matrix)) {
        target_col <- colnames(prob_matrix)[ncol(prob_matrix)]
      }
      return(as.numeric(prob_matrix[, target_col]))
    }

    if (!is.null(pred$response)) {
      response_vec <- as.character(pred$response)
      if (is.null(positive_class)) {
        return(as.numeric(response_vec == response_vec[1]))
      }
      return(as.numeric(response_vec == positive_class))
    }

    stop("无法从预测结果中提取概率")
  }
}

prepare_dataset_frame <- function(dataset_name, selected_features) {
  if (dataset_name == "train") {
    if (exists("task_train_selected", inherits = TRUE) &&
        !is.null(get("task_train_selected", inherits = TRUE))) {
      train_task <- get("task_train_selected", inherits = TRUE)
      df <- train_task$data(cols = selected_features)
      group_df <- train_task$data(cols = train_task$target_names)
      sample_groups <- as.character(group_df[[1]])
      sample_ids <- NULL
      if (exists("real_train_sample_ids", inherits = TRUE)) {
        sample_ids <- get("real_train_sample_ids", inherits = TRUE)
      }
    } else {
      df <- Data[split_gp$train, selected_features, drop = FALSE]
      sample_groups <- if ("group" %in% colnames(Data)) as.character(Data[split_gp$train, "group", drop = TRUE]) else NULL
      sample_ids <- rownames(df)
    }
  } else if (dataset_name == "test") {
    if (exists("task_test_selected", inherits = TRUE) &&
        !is.null(get("task_test_selected", inherits = TRUE))) {
      test_task <- get("task_test_selected", inherits = TRUE)
      df <- test_task$data(cols = selected_features)
      group_df <- test_task$data(cols = test_task$target_names)
      sample_groups <- as.character(group_df[[1]])
      sample_ids <- NULL
      if (exists("real_test_sample_ids", inherits = TRUE)) {
        sample_ids <- get("real_test_sample_ids", inherits = TRUE)
      }
    } else {
      df <- Data[split_gp$test, selected_features, drop = FALSE]
      sample_groups <- if ("group" %in% colnames(Data)) as.character(Data[split_gp$test, "group", drop = TRUE]) else NULL
      sample_ids <- rownames(df)
    }
  } else if (dataset_name == "valid") {
    if (!exists("Valid_Data", inherits = TRUE) || is.null(Valid_Data)) {
      return(NULL)
    }
    valid_df <- Valid_Data
    sample_groups <- if ("group" %in% colnames(valid_df)) as.character(valid_df$group) else NULL
    missing_features <- setdiff(selected_features, colnames(valid_df))
    if (length(missing_features) > 0) {
      for (feature_name in missing_features) {
        valid_df[[feature_name]] <- 0
      }
    }
    df <- valid_df[, selected_features, drop = FALSE]
    sample_ids <- rownames(df)
  } else {
    return(NULL)
  }

  df <- as.data.frame(df, check.names = FALSE)
  if (is.null(sample_ids)) {
    sample_ids <- rownames(df)
  }
  if (is.null(sample_ids)) {
    sample_ids <- paste0(dataset_name, "_", seq_len(nrow(df)))
  }
  attr(df, "sample_ids") <- sample_ids
  if (!is.null(sample_groups)) {
    attr(df, "sample_groups") <- sample_groups
  }
  df
}

rename_shap_object <- function(sv_obj, mapping_df) {
  if (is.null(sv_obj)) return(sv_obj)
  mapped_names <- map_feature_labels(colnames(sv_obj$X), mapping_df)
  colnames(sv_obj$X) <- mapped_names
  colnames(sv_obj$S) <- mapped_names
  sv_obj
}

build_shapviz_safe <- function(shap_result, explain_df, sample_ids, sample_groups = NULL) {
  shap_matrix <- shap_result[["S"]]
  baseline <- shap_result[["baseline"]]

  if (is.list(shap_matrix)) {
    if (length(shap_matrix) != 1L) {
      stop("当前 Step3 仅支持单输出/二分类 SHAP 结果")
    }
    shap_matrix <- shap_matrix[[1]]
    if (length(baseline) > 1L) {
      baseline <- baseline[[1]]
    }
  }

  shap_matrix <- as.matrix(shap_matrix)
  explain_mat <- as.matrix(explain_df)

  if (exists("log_message", mode = "function", inherits = TRUE)) {
    get("log_message", mode = "function", inherits = TRUE)(
      "SHAP 对象构建输入：S_dim=",
      paste(dim(shap_matrix), collapse = "x"),
      " X_dim=",
      paste(dim(explain_mat), collapse = "x")
    )
  }

  if (is.null(colnames(shap_matrix))) {
    colnames(shap_matrix) <- colnames(explain_mat)[seq_len(ncol(shap_matrix))]
  }

  keep_rows <- stats::complete.cases(shap_matrix)
  dropped_n <- sum(!keep_rows)
  if (dropped_n > 0) {
    shap_matrix <- shap_matrix[keep_rows, , drop = FALSE]
    explain_mat <- explain_mat[keep_rows, , drop = FALSE]
    explain_df <- explain_df[keep_rows, , drop = FALSE]
    sample_ids <- sample_ids[keep_rows]
    if (!is.null(sample_groups)) {
      sample_groups <- sample_groups[keep_rows]
    }
  }

  baseline <- as.numeric(baseline)[1]
  if (!is.finite(baseline)) {
    baseline <- 0
  }

  sv_obj <- shapviz::shapviz(shap_matrix, X = explain_mat, baseline = baseline)
  list(
    sv_obj = sv_obj,
    explain_df = explain_df,
    sample_ids = sample_ids,
    sample_groups = sample_groups,
    dropped_n = dropped_n
  )
}

resolve_display_features <- function(feature_count, requested_count, auto_cap) {
  if (feature_count <= 0) {
    return(0L)
  }
  if (!is.na(requested_count) && requested_count > 0) {
    return(as.integer(min(requested_count, feature_count)))
  }
  if (is.na(auto_cap) || auto_cap <= 0) {
    return(as.integer(feature_count))
  }
  as.integer(min(feature_count, auto_cap))
}

sanitize_filename <- function(x) {
  x %>%
    gsub("[^A-Za-z0-9._-]+", "_", ., perl = TRUE) %>%
    gsub("_+", "_", ., perl = TRUE) %>%
    gsub("^_|_$", "", ., perl = TRUE)
}

wrap_one_feature_label <- function(one, width) {
  one <- as.character(one)
  if (!nzchar(one) || nchar(one, type = "width") <= width) {
    return(one)
  }
  chars <- strsplit(one, "", fixed = TRUE)[[1]]
  hyphen_pos <- which(chars == "-")
  if (length(hyphen_pos) > 0) {
    target <- nchar(one, type = "width") / 2
    break_at <- hyphen_pos[which.min(abs(hyphen_pos - target))]
    left <- substr(one, 1L, break_at)
    right <- substr(one, break_at + 1L, nchar(one))
    if (nzchar(right)) {
      return(paste(left, right, sep = "\n"))
    }
  }
  hard_width <- max(4L, width - 1L)
  paste0(substr(one, 1L, hard_width), "-\n", substr(one, hard_width + 1L, nchar(one)))
}

wrap_feature_labels <- function(x, width = 18) {
  width <- max(4L, as.integer(width))
  vapply(as.character(x), wrap_one_feature_label, character(1), width = width, USE.NAMES = FALSE)
}

parse_metric_priority <- function(x) {
  vals <- parse_csv_arg(x, allow_all = FALSE)
  vals <- tolower(vals)
  vals <- intersect(vals, c("valid", "test", "cv", "train"))
  if (length(vals) == 0) {
    vals <- c("valid", "test", "cv")
  }
  vals
}

build_model_metric_table <- function(available_models) {
  metric_df <- tibble(Model = available_models)

  if (exists("performance_comparison", inherits = TRUE) && is.data.frame(performance_comparison)) {
    perf_df <- as_tibble(performance_comparison)
    keep_cols <- intersect(
      c("Model", "CV_AUC", "Train_AUC", "Test_AUC", "Test_Recall", "Train_Recall", "CV_Recall", "Test_MCC", "Train_MCC", "CV_MCC"),
      colnames(perf_df)
    )
    if (length(keep_cols) > 1) {
      metric_df <- left_join(metric_df, perf_df[, keep_cols, drop = FALSE], by = "Model")
    }
  }

  if (exists("valid_performance_comparison", inherits = TRUE) && is.data.frame(valid_performance_comparison)) {
    valid_df <- as_tibble(valid_performance_comparison)
    keep_cols <- intersect(c("Model", "Valid_AUC", "Valid_Recall", "Valid_MCC"), colnames(valid_df))
    if (length(keep_cols) > 1) {
      metric_df <- left_join(metric_df, valid_df[, keep_cols, drop = FALSE], by = "Model")
    }
  }

  metric_df
}

resolve_model_quality <- function(model_name, metric_df, metric_priority) {
  row_df <- metric_df %>% filter(Model == model_name)
  if (nrow(row_df) == 0) {
    return(list(metric = NA_character_, auc = NA_real_, recall = NA_real_, mcc = NA_real_))
  }

  metric_map <- c(valid = "Valid_AUC", test = "Test_AUC", cv = "CV_AUC", train = "Train_AUC")
  recall_map <- c(valid = "Valid_Recall", test = "Test_Recall", cv = "CV_Recall", train = "Train_Recall")
  mcc_map <- c(valid = "Valid_MCC", test = "Test_MCC", cv = "CV_MCC", train = "Train_MCC")
  for (metric_name in metric_priority) {
    col_name <- metric_map[[metric_name]]
    if (!is.null(col_name) && col_name %in% colnames(row_df)) {
      auc_value <- suppressWarnings(as.numeric(row_df[[col_name]][1]))
      if (!is.na(auc_value)) {
        recall_col <- recall_map[[metric_name]]
        mcc_col <- mcc_map[[metric_name]]
        recall_value <- if (!is.null(recall_col) && recall_col %in% colnames(row_df)) suppressWarnings(as.numeric(row_df[[recall_col]][1])) else NA_real_
        mcc_value <- if (!is.null(mcc_col) && mcc_col %in% colnames(row_df)) suppressWarnings(as.numeric(row_df[[mcc_col]][1])) else NA_real_
        return(list(metric = col_name, auc = auc_value, recall = recall_value, mcc = mcc_value))
      }
    }
  }

  list(metric = NA_character_, auc = NA_real_, recall = NA_real_, mcc = NA_real_)
}

write_skip_note <- function(model_outdir, model_name, reason_text, auc_metric, auc_value, metric_priority) {
  skip_file <- file.path(model_outdir, "SHAP_SKIPPED.md")
  lines <- c(
    paste0("# SHAP Skipped: ", model_name),
    "",
    paste0("- Reason: ", reason_text),
    paste0("- Metric used: ", ifelse(is.na(auc_metric), "NA", auc_metric)),
    paste0("- Metric value: ", ifelse(is.na(auc_value), "NA", format(round(auc_value, 4), nsmall = 4))),
    paste0("- Metric priority: ", paste(metric_priority, collapse = " > "))
  )
  writeLines(lines, skip_file, useBytes = TRUE)
  skip_file
}

select_requested_samples_for_dataset <- function(requested_samples, dataset_name) {
  if (length(requested_samples) == 0) {
    return(character())
  }
  out <- character()
  for (sample_spec in requested_samples) {
    if (grepl(":", sample_spec, fixed = TRUE)) {
      parts <- strsplit(sample_spec, ":", fixed = TRUE)[[1]]
      prefix <- tolower(trimws(parts[[1]]))
      sample_id <- trimws(paste(parts[-1], collapse = ":"))
      if (prefix %in% c(dataset_name, "all") && nzchar(sample_id)) {
        out <- c(out, sample_id)
      }
    } else {
      out <- c(out, sample_spec)
    }
  }
  unique(out[nzchar(out)])
}

resolve_group_label <- function(group_spec) {
  group_spec <- trimws(group_spec)
  group_key <- tolower(group_spec)
  if (group_key %in% c("case", "positive", "pos")) {
    if (exists("gp", inherits = TRUE) && length(get("gp", inherits = TRUE)) >= 1) {
      return(list(label = "case", group = as.character(get("gp", inherits = TRUE)[[1]])))
    }
  }
  if (group_key %in% c("control", "ctrl", "negative", "neg")) {
    if (exists("gp", inherits = TRUE) && length(get("gp", inherits = TRUE)) >= 2) {
      return(list(label = "control", group = as.character(get("gp", inherits = TRUE)[[2]])))
    }
  }
  list(label = sanitize_filename(group_spec), group = group_spec)
}

resolve_waterfall_targets <- function(
  sample_ids,
  requested_samples,
  default_n,
  sample_groups = NULL,
  waterfall_groups = character(),
  waterfall_per_group = 1L
) {
  if (length(sample_ids) == 0) {
    return(tibble(
      PlotLabel = character(),
      row_id = integer(),
      SampleID = character(),
      SampleGroup = character(),
      Selection = character(),
      Requested = logical()
    ))
  }

  if (length(requested_samples) > 0) {
    matched_idx <- match(requested_samples, sample_ids)
    return(tibble(
      PlotLabel = paste0("sample", seq_along(requested_samples)),
      row_id = matched_idx,
      SampleID = requested_samples,
      SampleGroup = if (!is.null(sample_groups)) sample_groups[matched_idx] else NA_character_,
      Selection = "requested",
      Requested = TRUE
    ))
  }

  waterfall_groups <- waterfall_groups[nzchar(waterfall_groups)]
  if (!is.null(sample_groups) && length(waterfall_groups) > 0) {
    target_rows <- list()
    sample_groups_chr <- as.character(sample_groups)
    waterfall_per_group <- max(1L, as.integer(waterfall_per_group))
    for (group_spec in waterfall_groups) {
      resolved_group <- resolve_group_label(group_spec)
      matched_idx <- which(sample_groups_chr == resolved_group$group)
      if (length(matched_idx) == 0) {
        next
      }
      selected_idx <- matched_idx[seq_len(min(waterfall_per_group, length(matched_idx)))]
      target_rows[[length(target_rows) + 1]] <- tibble(
        PlotLabel = paste0(resolved_group$label, seq_along(selected_idx)),
        row_id = selected_idx,
        SampleID = sample_ids[selected_idx],
        SampleGroup = sample_groups_chr[selected_idx],
        Selection = paste0("group:", resolved_group$label),
        Requested = FALSE
      )
    }
    group_targets <- bind_rows(target_rows)
    if (nrow(group_targets) > 0) {
      group_targets <- group_targets %>%
        distinct(SampleID, .keep_all = TRUE) %>%
        mutate(row_id = match(SampleID, sample_ids))
      return(group_targets)
    }
  }

  waterfall_n <- max(1L, as.integer(default_n))
  waterfall_n <- min(waterfall_n, length(sample_ids))
  tibble(
    PlotLabel = paste0("sample", seq_len(waterfall_n)),
    row_id = seq_len(waterfall_n),
    SampleID = sample_ids[seq_len(waterfall_n)],
    SampleGroup = if (!is.null(sample_groups)) sample_groups[seq_len(waterfall_n)] else NA_character_,
    Selection = "default",
    Requested = FALSE
  )
}

save_shap_plots <- function(
  sv_obj,
  model_name,
  dataset_name,
  model_outdir,
  display_features,
  local_display_features,
  feature_label_width,
  sample_ids,
  waterfall_targets,
  force_plots = TRUE
) {
  title_prefix <- paste0(model_name, " - ", dataset_name, " SHAP")
  wrap_discrete_labels <- function(x) wrap_feature_labels(x, width = feature_label_width)

  save_pdf_plot(
    file.path(model_outdir, paste0(dataset_name, "_SHAP_importance.pdf")),
    sv_importance(sv_obj, max_display = display_features) +
      scale_y_discrete(labels = wrap_discrete_labels) +
      ggtitle(paste0(title_prefix, " Importance")),
    width = 10, height = 6
  )

  save_pdf_plot(
    file.path(model_outdir, paste0(dataset_name, "_SHAP_beeswarm.pdf")),
    sv_importance(sv_obj, kind = "beeswarm", max_display = display_features) +
      scale_y_discrete(labels = wrap_discrete_labels) +
      ggtitle(paste0(title_prefix, " Beeswarm")),
    width = 10, height = 8
  )

  valid_targets <- waterfall_targets %>%
    filter(!is.na(row_id), row_id >= 1, row_id <= length(sample_ids))

  for (i in seq_len(nrow(valid_targets))) {
    row_id <- valid_targets$row_id[[i]]
    sample_id <- valid_targets$SampleID[[i]]
    sample_label <- valid_targets$PlotLabel[[i]]
    safe_sample_id <- sanitize_filename(sample_id)
    if (!nzchar(safe_sample_id)) {
      safe_sample_id <- sample_label
    }

    save_pdf_plot(
      file.path(model_outdir, paste0(dataset_name, "_SHAP_waterfall_", sample_label, "_", safe_sample_id, ".pdf")),
      sv_waterfall(sv_obj, row_id = row_id, max_display = local_display_features) +
        scale_y_discrete(labels = wrap_discrete_labels) +
        ggtitle(paste0(title_prefix, " Waterfall - ", sample_id)),
      width = 11, height = 7
    )

    if (isTRUE(force_plots)) {
      save_pdf_plot(
        file.path(model_outdir, paste0(dataset_name, "_SHAP_force_", sample_label, "_", safe_sample_id, ".pdf")),
        sv_force(sv_obj, row_id = row_id, max_display = local_display_features) +
          ggtitle(paste0(title_prefix, " Force - ", sample_id)),
        width = 11, height = 7
      )
    }
  }
}

extract_importance_table <- function(sv_obj, model_name, dataset_name) {
  tibble(
    Model = model_name,
    Dataset = dataset_name,
    Feature = colnames(sv_obj$S),
    SHAP_Importance = colMeans(abs(sv_obj$S))
  ) %>%
    arrange(desc(SHAP_Importance))
}

log_message("=== 加载 Step2 输出的 RData ===")
log_message("RData 文件路径：", opts$load_rdata)
load(opts$load_rdata)
opts <- runtime_opts
log_message("RData 加载完成")

required_objects <- c("model_results", "selected_features", "split_gp", "Data")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("RData 缺少 Step3 所需对象：", paste(missing_objects, collapse = ", "))
}

if (!exists("names_values", inherits = TRUE)) {
  names_values <- NULL
}

available_models <- names(model_results)
if (length(available_models) == 0) {
  stop("RData 中未找到可用的 model_results")
}
log_message("RData 中可用模型数：", length(available_models))

requested_models <- parse_csv_arg(opts$models)
if (length(requested_models) == 1 && requested_models == "all") {
  target_models <- available_models
} else {
  target_models <- intersect(requested_models, available_models)
}
if (length(target_models) == 0) {
  stop("未找到可处理的模型。可用模型：", paste(available_models, collapse = ", "))
}

requested_datasets <- parse_csv_arg(opts$datasets)
if (length(requested_datasets) == 1 && requested_datasets == "all") {
  target_datasets <- c("train", "test", "valid")
} else {
  target_datasets <- intersect(requested_datasets, c("train", "test", "valid"))
}
if (length(target_datasets) == 0) {
  stop("未找到可处理的数据集。允许的值：train,test,valid")
}

background_train_df <- prepare_dataset_frame("train", selected_features)
background_train_df <- sample_rows_df(background_train_df, opts$background_rows)
display_features <- resolve_display_features(
  feature_count = length(selected_features),
  requested_count = opts$display_features,
  auto_cap = opts$auto_display_cap
)
local_display_features <- if (!is.na(opts$local_display_features) && opts$local_display_features > 0) {
  resolve_display_features(
    feature_count = length(selected_features),
    requested_count = opts$local_display_features,
    auto_cap = opts$auto_display_cap
  )
} else {
  display_features
}
metric_priority <- parse_metric_priority(opts$shap_metric_priority)
model_metric_table <- build_model_metric_table(available_models)
log_message("背景训练集样本数：", nrow(background_train_df))
log_message("解释特征数：", length(selected_features))
log_message("SHAP 图展示特征数：", display_features)
log_message("waterfall/force 图展示特征数：", local_display_features)
log_message("SHAP 图特征名换行宽度：", opts$feature_label_width)
log_message("目标模型：", paste(target_models, collapse = ", "))
log_message("目标数据集：", paste(target_datasets, collapse = ", "))
log_message("SHAP 质量判定优先级：", paste(metric_priority, collapse = " > "))
log_message("SHAP 最低 AUC 阈值：", opts$min_auc_for_shap)
requested_waterfall_samples <- parse_csv_arg(opts$waterfall_samples, allow_all = FALSE)
requested_waterfall_groups <- parse_csv_arg(opts$waterfall_groups, allow_all = FALSE)
if (length(requested_waterfall_samples) > 0) {
  log_message("指定 waterfall 样本：", paste(requested_waterfall_samples, collapse = ", "))
} else if (length(requested_waterfall_groups) > 0) {
  log_message(
    "未指定 waterfall 样本；按组自动选择：",
    paste(requested_waterfall_groups, collapse = ", "),
    "；每组 ",
    opts$waterfall_per_group,
    " 个样本",
    sep = ""
  )
} else {
  log_message("未指定 waterfall 样本；默认每个模型/数据集输出前 ", opts$waterfall_default_n, " 个解释样本", sep = "")
}
log_message("Force 图输出：", ifelse(isTRUE(opts$force_plots), "TRUE", "FALSE"))

nsim_map <- c(train = opts$nsim_train, test = opts$nsim_test, valid = opts$nsim_valid)
max_row_map <- c(train = opts$max_train_rows, test = opts$max_test_rows, valid = opts$max_valid_rows)

summary_rows <- list()
importance_summary_list <- list()
expected_pairs <- tidyr::crossing(Model = target_models, Dataset = target_datasets)
skip_rows <- list()

for (model_name in target_models) {
  log_message("")
  log_message("=== 开始处理模型：", model_name, " ===")

  model_entry <- model_results[[model_name]]
  trained_learner <- resolve_trained_learner(model_entry)
  if (is.null(trained_learner)) {
    log_message("模型对象为空，跳过：", model_name, level = "WARNING")
    summary_rows[[length(summary_rows) + 1]] <- tibble(
      Model = model_name, Dataset = NA_character_, Status = "skipped", Message = "model object missing"
    )
    next
  }

  positive_class <- get_positive_class(trained_learner)
  pred_wrapper <- predict_prob_wrapper(trained_learner, positive_class, model_name)
  model_outdir <- file.path(opts$outdir, model_name)
  if (!dir.exists(model_outdir)) {
    dir.create(model_outdir, recursive = TRUE)
  }

  model_auc_info <- resolve_model_quality(model_name, model_metric_table, metric_priority)
  skip_reason <- NULL

  if (isTRUE(opts$skip_poor_models) && !is.na(model_auc_info$auc) && model_auc_info$auc < opts$min_auc_for_shap) {
    skip_reason <- paste0(
      "模型效果低于 SHAP 阈值；",
      model_auc_info$metric, "=",
      format(round(model_auc_info$auc, 4), nsmall = 4),
      " < ",
      format(round(opts$min_auc_for_shap, 4), nsmall = 4)
    )
  } else if (
    isTRUE(opts$skip_zero_recall_models) &&
    !is.na(model_auc_info$recall) &&
    model_auc_info$recall <= 0 &&
    grepl("^(Valid|Test)_AUC$", model_auc_info$metric)
  ) {
    recall_metric <- sub("_AUC$", "_Recall", model_auc_info$metric)
    skip_reason <- paste0(
      "模型在外部/测试评估中出现退化阈值表现；",
      recall_metric, "=",
      format(round(model_auc_info$recall, 4), nsmall = 4),
      " <= 0"
    )
  }

  if (!is.null(skip_reason)) {
    skip_file <- write_skip_note(
      model_outdir = model_outdir,
      model_name = model_name,
      reason_text = skip_reason,
      auc_metric = model_auc_info$metric,
      auc_value = model_auc_info$auc,
      metric_priority = metric_priority
    )
    log_message(skip_reason, "；已跳过 SHAP，说明文件：", skip_file, level = "WARNING")
    for (dataset_name in target_datasets) {
      summary_rows[[length(summary_rows) + 1]] <- tibble(
        Model = model_name,
        Dataset = dataset_name,
        Status = "skipped_poor_model",
        Message = skip_reason
      )
    }
    skip_rows[[length(skip_rows) + 1]] <- tibble(
      Model = model_name,
      Metric = model_auc_info$metric,
      AUC = model_auc_info$auc,
      Threshold = opts$min_auc_for_shap,
      Reason = skip_reason,
      NoteFile = skip_file
    )
    next
  }

  model_excel <- list()
  model_waterfall_manifest <- list()

  for (dataset_name in target_datasets) {
    log_message("处理数据集：", dataset_name)
    dataset_df <- prepare_dataset_frame(dataset_name, selected_features)

    if (is.null(dataset_df) || nrow(dataset_df) == 0) {
      log_message("数据集不可用或为空，跳过：", dataset_name, level = "WARNING")
      summary_rows[[length(summary_rows) + 1]] <- tibble(
        Model = model_name, Dataset = dataset_name, Status = "skipped", Message = "dataset unavailable"
      )
      next
    }

    dataset_sample_ids <- attr(dataset_df, "sample_ids")
    dataset_sample_groups <- attr(dataset_df, "sample_groups")
    requested_samples_for_dataset <- select_requested_samples_for_dataset(requested_waterfall_samples, dataset_name)
    preselected_waterfall_targets <- resolve_waterfall_targets(
      sample_ids = dataset_sample_ids,
      requested_samples = requested_samples_for_dataset,
      default_n = opts$waterfall_default_n,
      sample_groups = dataset_sample_groups,
      waterfall_groups = requested_waterfall_groups,
      waterfall_per_group = opts$waterfall_per_group
    )
    keep_waterfall_ids <- preselected_waterfall_targets$SampleID[!is.na(preselected_waterfall_targets$row_id)]

    explain_df <- sample_rows_df_keep_ids(
      dataset_df,
      max_rows = max_row_map[[dataset_name]],
      keep_ids = keep_waterfall_ids
    )
    sample_ids <- attr(explain_df, "sample_ids")
    sample_groups <- attr(explain_df, "sample_groups")
    if (is.null(sample_ids)) {
      sample_ids <- rownames(explain_df)
    }
    waterfall_targets <- resolve_waterfall_targets(
      sample_ids = sample_ids,
      requested_samples = requested_samples_for_dataset,
      default_n = opts$waterfall_default_n,
      sample_groups = sample_groups,
      waterfall_groups = requested_waterfall_groups,
      waterfall_per_group = opts$waterfall_per_group
    )
    nsim_value <- as.integer(nsim_map[[dataset_name]])

    kernel_m <- as.integer(max(2L, nsim_value))
    if (kernel_m %% 2L == 1L) {
      kernel_m <- kernel_m + 1L
    }

    log_message(
      "开始 SHAP 计算：模型=", model_name,
      " 数据集=", dataset_name,
      " 样本数=", nrow(explain_df),
      " 背景样本数=", nrow(background_train_df),
      " m=", kernel_m
    )

    shap_result <- tryCatch(
      {
        kernelshap::kernelshap(
          object = trained_learner,
          X = explain_df,
          bg_X = background_train_df,
          pred_fun = pred_wrapper,
          m = kernel_m,
          exact = FALSE,
          hybrid_degree = 0L,
          verbose = FALSE
        )
      },
      error = function(e) {
        log_message("SHAP 计算失败：", e$message, level = "ERROR")
        NULL
      }
    )

    if (is.null(shap_result)) {
      summary_rows[[length(summary_rows) + 1]] <- tibble(
        Model = model_name, Dataset = dataset_name, Status = "failed", Message = "shap explain failed"
      )
      next
    }

    log_message("SHAP 计算完成：模型=", model_name, " 数据集=", dataset_name)

    safe_shap <- tryCatch(
      {
        build_shapviz_safe(
          shap_result = shap_result,
          explain_df = explain_df,
          sample_ids = sample_ids,
          sample_groups = sample_groups
        )
      },
      error = function(e) {
        log_message("SHAP 对象构建失败：", e$message, level = "ERROR")
        NULL
      }
    )

    if (is.null(safe_shap)) {
      summary_rows[[length(summary_rows) + 1]] <- tibble(
        Model = model_name, Dataset = dataset_name, Status = "failed", Message = "shapviz build failed"
      )
      next
    }

    explain_df <- safe_shap$explain_df
    sample_ids <- safe_shap$sample_ids
    sample_groups <- safe_shap$sample_groups
    if (safe_shap$dropped_n > 0) {
      log_message(
        "SHAP 结果中发现缺失值，已过滤异常样本行数：",
        safe_shap$dropped_n,
        level = "WARNING"
      )
    }
    waterfall_targets <- resolve_waterfall_targets(
      sample_ids = sample_ids,
      requested_samples = requested_samples_for_dataset,
      default_n = opts$waterfall_default_n,
      sample_groups = sample_groups,
      waterfall_groups = requested_waterfall_groups,
      waterfall_per_group = opts$waterfall_per_group
    )

    sv_obj <- rename_shap_object(safe_shap$sv_obj, names_values)

    save_shap_plots(
      sv_obj = sv_obj,
      model_name = model_name,
      dataset_name = dataset_name,
      model_outdir = model_outdir,
      display_features = display_features,
      local_display_features = local_display_features,
      feature_label_width = opts$feature_label_width,
      sample_ids = sample_ids,
      waterfall_targets = waterfall_targets,
      force_plots = opts$force_plots
    )

    shap_df <- as.data.frame(sv_obj$S, check.names = FALSE)
    importance_df <- extract_importance_table(sv_obj, model_name, dataset_name)
    sample_imp_df <- as.data.frame(abs(sv_obj$S), check.names = FALSE)

    model_excel[[paste0(dataset_name, "_shap")]] <- cbind(SampleID = sample_ids, shap_df)
    model_excel[[paste0(dataset_name, "_sample_imp")]] <- cbind(SampleID = sample_ids, sample_imp_df)
    model_excel[[paste0(dataset_name, "_importance")]] <- importance_df
    model_waterfall_manifest[[paste0(dataset_name, "_waterfall_manifest")]] <- waterfall_targets %>%
      mutate(
        Matched = !is.na(row_id),
        Dataset = dataset_name
      ) %>%
      select(Dataset, PlotLabel, SampleID, SampleGroup, Selection, row_id, Matched, Requested)
    importance_summary_list[[paste0(model_name, "_", dataset_name)]] <- importance_df

    if (nrow(waterfall_targets) > 0) {
      matched_targets <- waterfall_targets %>% filter(!is.na(row_id))
      if (nrow(matched_targets) > 0) {
        log_message(
          "Waterfall 样本映射：",
          paste0(matched_targets$PlotLabel, "=", matched_targets$SampleID, collapse = ", ")
        )
      }
      unmatched_targets <- waterfall_targets %>% filter(is.na(row_id))
      if (nrow(unmatched_targets) > 0) {
        log_message(
          "以下 waterfall 样本未在当前解释数据集中命中：",
          paste(unmatched_targets$SampleID, collapse = ", "),
          level = "WARNING"
        )
      }
    }

    summary_rows[[length(summary_rows) + 1]] <- tibble(
      Model = model_name,
      Dataset = dataset_name,
      Status = "completed",
      Message = paste0("n=", nrow(explain_df), ", m=", kernel_m)
    )
  }

  if (length(model_excel) > 0) {
    model_excel <- c(model_excel, model_waterfall_manifest)
    write_xlsx_atomic(model_excel, file.path(model_outdir, paste0(model_name, "_SHAP.xlsx")))
    log_message("已生成模型 SHAP Excel：", file.path(model_outdir, paste0(model_name, "_SHAP.xlsx")))
    log_message("=== 模型处理完成：", model_name, " ===")
  } else {
    log_message("模型没有可写出的 SHAP 结果：", model_name, level = "WARNING")
  }
}

summary_df <- bind_rows(summary_rows)
readr::write_tsv(summary_df, file.path(opts$outdir, "step3_shap_status.tsv"))

skip_df <- bind_rows(skip_rows)
if (nrow(skip_df) > 0) {
  readr::write_tsv(skip_df, file.path(opts$outdir, "step3_shap_skipped.tsv"))
  log_message("已生成 SHAP 跳过说明表：", file.path(opts$outdir, "step3_shap_skipped.tsv"))
}

if (length(importance_summary_list) > 0) {
  write_xlsx_atomic(
    c(list(status = summary_df), importance_summary_list),
    file.path(opts$outdir, "step3_shap_summary.xlsx")
  )
  log_message("已生成 SHAP 汇总 Excel：", file.path(opts$outdir, "step3_shap_summary.xlsx"))
}

if (!("completed" %in% summary_df$Status)) {
  if (nrow(skip_df) > 0 && all(summary_df$Status %in% c("skipped_poor_model"))) {
    log_message("所有目标模型均因效果未达阈值而跳过 SHAP", level = "WARNING")
  } else {
    stop("Step3 未生成任何成功的 SHAP 结果，请检查 run_log")
  }
}

if (isTRUE(opts$require_all)) {
  completion_df <- expected_pairs %>%
    left_join(summary_df, by = c("Model", "Dataset"))

  incomplete_df <- completion_df %>%
    filter(is.na(Status) | !(Status %in% c("completed", "skipped_poor_model")))

  if (nrow(incomplete_df) > 0) {
    incomplete_labels <- paste0(incomplete_df$Model, "/", incomplete_df$Dataset)
    stop(
      "Step3 严格模式失败，以下组合未成功完成：",
      paste(incomplete_labels, collapse = ", ")
    )
  }
}

log_message("已生成 SHAP 状态表：", file.path(opts$outdir, "step3_shap_status.tsv"))
log_message("=== Step3 SHAP 分析完成 ===")
