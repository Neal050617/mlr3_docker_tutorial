#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required.", call. = FALSE)
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required.", call. = FALSE)
  }
})

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) {
    return(default)
  }
  sub(prefix, "", hit[[1]], fixed = TRUE)
}

root_arg <- get_arg("root", NULL)
if (is.null(root_arg)) {
  stop("Missing required argument: --root=/path/to/result_root", call. = FALSE)
}
root <- normalizePath(root_arg, mustWork = TRUE)
project_name <- get_arg("project-name", basename(root))
analysis_title <- get_arg("title", project_name)
branch_map_file <- get_arg("branch-map", NULL)
script_label <- get_arg("script-label", "scripts/summarize_mlr3_results.R")
tasks <- strsplit(get_arg("tasks", "all"), ",", fixed = TRUE)[[1]]
tasks <- trimws(tasks)
if ("all" %in% tasks) {
  tasks <- c("performance", "step2", "shap", "manual")
}
shap_dir_name <- get_arg("shap-dir-name", "")

out_performance <- get_arg("out-performance", file.path(root, "01_model_performance_summary"))
out_step2 <- get_arg("out-step2", file.path(root, "02_step2_technical_summary"))
out_shap <- get_arg("out-shap", file.path(root, "03_shap_output_summary"))
manual_file <- get_arg("manual", file.path(root, "RESULT_INTERPRETATION_MANUAL.md"))

load_branch_map <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(data.frame())
  }
  if (!file.exists(path)) {
    stop("Branch map not found: ", path, call. = FALSE)
  }
  x <- utils::read.delim(path, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)
  if ("model_id" %in% names(x) && !"Branch_ID" %in% names(x)) {
    x$Branch_ID <- as.character(x$model_id)
  }
  if ("label_cn" %in% names(x) && !"Branch_Label" %in% names(x)) {
    x$Branch_Label <- as.character(x$label_cn)
  }
  if ("modality" %in% names(x) && !"Input_Type" %in% names(x)) {
    x$Input_Type <- as.character(x$modality)
  }
  if (!"Branch_ID" %in% names(x)) {
    stop("Branch map must include Branch_ID or model_id column: ", path, call. = FALSE)
  }
  x$Branch_ID <- as.character(x$Branch_ID)
  x
}

branch_map <- load_branch_map(branch_map_file)

clean_step2_label <- function(step2_dir) {
  x <- basename(step2_dir)
  x <- sub("^run_model_[^_]+_", "", x)
  x <- sub("_step2$", "", x)
  if (!nzchar(x) || identical(x, basename(step2_dir))) {
    x <- basename(step2_dir)
  }
  x
}

lookup_branch_field <- function(branch_id, field, fallback) {
  if (nrow(branch_map) && field %in% names(branch_map)) {
    hit <- branch_map[branch_map$Branch_ID == branch_id, field, drop = TRUE]
    hit <- hit[!is.na(hit) & nzchar(as.character(hit))]
    if (length(hit)) {
      return(as.character(hit[[1]]))
    }
  }
  fallback
}

branch_label <- function(branch_id, step2_dir = NULL) {
  fallback <- if (!is.null(step2_dir)) clean_step2_label(step2_dir) else branch_id
  lookup_branch_field(branch_id, "Branch_Label", fallback)
}

branch_input_type <- function(branch_id, step2_dir = NULL) {
  fallback <- if (!is.null(step2_dir)) clean_step2_label(step2_dir) else branch_id
  lookup_branch_field(branch_id, "Input_Type", fallback)
}

extract_branch_id <- function(path) {
  base <- basename(path)
  out <- sub("^run_model_([0-9]+\\.[0-9]+).*_step2$", "\\1", base)
  if (identical(out, base)) {
    out <- sub("^run_model_([^_]+)_step2$", "\\1", base)
  }
  out
}

discover_step2_dirs <- function(root) {
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  dirs <- dirs[grepl("^run_model_.+_step2$", basename(dirs))]
  dirs[order(basename(dirs))]
}

step2_dirs <- discover_step2_dirs(root)
if (!length(step2_dirs)) {
  stop("No run_model_*_step2 directories found under: ", root, call. = FALSE)
}

write_tsv <- function(df, path) {
  utils::write.table(df, file = path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = "")
}

fmt_num <- function(x) {
  ifelse(is.na(x), "", sprintf("%.4f", as.numeric(x)))
}

md_table <- function(df, cols) {
  df <- df[, cols, drop = FALSE]
  out <- c(
    paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  )
  for (i in seq_len(nrow(df))) {
    vals <- vapply(df[i, , drop = FALSE], as.character, character(1))
    out <- c(out, paste0("| ", paste(vals, collapse = " | "), " |"))
  }
  out
}

format_display <- function(df) {
  out <- df
  num_cols <- intersect(
    c(
      "Balanced_Score", "Test_AUC", "Test_AUPRC", "Test_ACC", "Test_Recall",
      "Test_Specificity", "Test_Precision", "Test_F1", "Test_MCC", "Test_Threshold"
    ),
    names(out)
  )
  for (col in num_cols) {
    out[[col]] <- fmt_num(out[[col]])
  }
  out
}

summarize_performance <- function() {
  dir.create(out_performance, recursive = TRUE, showWarnings = FALSE)

  read_perf <- function(step2_dir) {
    perf_file <- file.path(step2_dir, "model_performance_comparison.xlsx")
    if (!file.exists(perf_file)) {
      warning("Missing performance file, skipped: ", perf_file, call. = FALSE)
      return(data.frame())
    }
    branch_id <- extract_branch_id(step2_dir)
    label <- branch_label(branch_id, step2_dir)
    df <- as.data.frame(readxl::read_excel(perf_file, sheet = "模型比较"), stringsAsFactors = FALSE)
    cm_test <- as.data.frame(readxl::read_excel(perf_file, sheet = "混淆矩阵_Test"), stringsAsFactors = FALSE)
    cm_test <- cm_test[, c("Model", "Threshold", "TP", "FP", "TN", "FN", "ACC", "Precision", "Recall", "F1"), drop = FALSE]
    cm_test$Test_Specificity <- with(cm_test, ifelse((TN + FP) > 0, TN / (TN + FP), NA_real_))
    cm_test$Test_MCC_threshold <- with(
      cm_test,
      ifelse(
        (TP + FP) > 0 & (TP + FN) > 0 & (TN + FP) > 0 & (TN + FN) > 0,
        ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)),
        NA_real_
      )
    )
    names(cm_test)[names(cm_test) == "Threshold"] <- "Test_Threshold"
    names(cm_test)[names(cm_test) == "TP"] <- "Test_TP"
    names(cm_test)[names(cm_test) == "FP"] <- "Test_FP"
    names(cm_test)[names(cm_test) == "TN"] <- "Test_TN"
    names(cm_test)[names(cm_test) == "FN"] <- "Test_FN"
    names(cm_test)[names(cm_test) == "ACC"] <- "Test_ACC_threshold"
    names(cm_test)[names(cm_test) == "Precision"] <- "Test_Precision_threshold"
    names(cm_test)[names(cm_test) == "Recall"] <- "Test_Recall_threshold"
    names(cm_test)[names(cm_test) == "F1"] <- "Test_F1_threshold"
    df <- df[, setdiff(
      names(df),
      c("Test_Threshold", "Test_ACC", "Test_Precision", "Test_Recall", "Test_F1", "Test_MCC", "Test_Specificity")
    ), drop = FALSE]
    df <- merge(df, cm_test, by = "Model", all.x = TRUE, sort = FALSE)
    df$Test_ACC <- df$Test_ACC_threshold
    df$Test_Precision <- df$Test_Precision_threshold
    df$Test_Recall <- df$Test_Recall_threshold
    df$Test_F1 <- df$Test_F1_threshold
    df$Test_MCC <- df$Test_MCC_threshold
    df$Test_ACC_threshold <- NULL
    df$Test_Precision_threshold <- NULL
    df$Test_Recall_threshold <- NULL
    df$Test_F1_threshold <- NULL
    df$Test_MCC_threshold <- NULL
    df$Branch_ID <- branch_id
    df$Branch_Label <- label
    df$Input_Type <- branch_input_type(branch_id, step2_dir)
    df$Step2_Dir <- basename(step2_dir)
    df
  }

  perf_rows <- lapply(step2_dirs, read_perf)
  perf_rows <- perf_rows[vapply(perf_rows, nrow, integer(1)) > 0]
  if (!length(perf_rows)) {
    stop("No model_performance_comparison.xlsx files could be read.", call. = FALSE)
  }
  all_perf <- do.call(rbind, perf_rows)
  all_perf$Balanced_Score <- with(
    all_perf,
    0.45 * Test_AUC + 0.35 * pmax(Test_MCC, -0.25) + 0.15 * Test_ACC + 0.05 * Test_AUPRC
  )

  keep_cols <- c(
    "Branch_ID", "Branch_Label", "Input_Type", "Model",
    "Test_AUC", "Test_AUPRC", "Test_Threshold", "Test_ACC", "Test_MCC",
    "Test_Recall", "Test_Specificity", "Test_Precision", "Test_F1",
    "Test_TP", "Test_FP", "Test_TN", "Test_FN",
    "Balanced_Score", "CV_AUC", "CV_MCC",
    "Train_AUC", "Train_ACC", "Train_MCC", "Step2_Dir"
  )
  all_perf <- all_perf[, intersect(keep_cols, names(all_perf)), drop = FALSE]

  sort_for_display <- function(df) {
    df[order(-df$Balanced_Score, -df$Test_AUC, -df$Test_MCC, df$Model), , drop = FALSE]
  }
  branch_sorted <- do.call(rbind, lapply(split(all_perf, all_perf$Branch_ID), sort_for_display))
  overall_sorted <- sort_for_display(all_perf)

  write_tsv(all_perf, file.path(out_performance, "all_model_metrics.tsv"))
  write_tsv(branch_sorted, file.path(out_performance, "branch_sorted_by_balanced_score.tsv"))
  write_tsv(overall_sorted, file.path(out_performance, "overall_sorted_by_balanced_score.tsv"))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "all_model_metrics")
  openxlsx::writeData(wb, "all_model_metrics", all_perf)
  openxlsx::addWorksheet(wb, "branch_sorted_by_balanced")
  openxlsx::writeData(wb, "branch_sorted_by_balanced", branch_sorted)
  openxlsx::addWorksheet(wb, "overall_sorted_by_balanced")
  openxlsx::writeData(wb, "overall_sorted_by_balanced", overall_sorted)
  openxlsx::saveWorkbook(wb, file.path(out_performance, "model_performance_summary.xlsx"), overwrite = TRUE)

  display_cols <- c(
    "Branch_ID", "Branch_Label", "Model",
    "Test_AUC", "Test_AUPRC", "Test_Threshold", "Test_ACC", "Test_MCC",
    "Test_Recall", "Test_Specificity", "Test_TP", "Test_FP", "Test_TN", "Test_FN",
    "Balanced_Score"
  )
  model_methods <- c(
    "| 方法 | 中文说明 | 解读注意 |",
    "|---|---|---|",
    "| `glmnet` | 正则化广义线性模型 | 可解释性较好，主要捕捉线性关系 |",
    "| `xgboost` | 极端梯度提升树 | 小样本下需关注过拟合 |",
    "| `rf` | 随机森林 | 稳定性较好，但单个规则不直观 |",
    "| `svm` | 支持向量机 | 对尺度和参数敏感 |",
    "| `naive_bayes` | 朴素贝叶斯 | 组学特征相关性强时需谨慎 |",
    "| `kknn` | 加权 k 近邻 | 受距离度量和局部噪声影响 |",
    "| `rpart` | 单棵决策树 | 解释直观但稳定性较弱 |",
    "| `adaboost` | 自适应提升树 | 对噪声和异常样本敏感 |",
    "| `nnet` | 单隐层神经网络 | 小样本下不稳定且解释性弱 |"
  )
  md <- c(
    paste0("# ", analysis_title, " 模型性能总汇总"),
    "",
    paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    paste0("分析目录：`", root, "`"),
    "",
    paste0("本文件由 `", script_label, " --tasks=performance` 自动生成。"),
    "",
    "## 读取原则",
    "",
    "- 本汇总自动扫描指定结果目录下所有 `run_model_*_step2` 文件夹。",
    "- 本汇总不声明“最优模型”。所有模型按指标完整展示，由使用者结合研究目的、验证策略和生物学合理性判断。",
    "- AUC 和 AUPRC 为无阈值指标；ACC、MCC、Sensitivity、Specificity、Precision、F1 和混淆矩阵均使用 Step2 记录的分类阈值，当前正式分析为 Youden 阈值。",
    "- 本正式汇总不补充固定 `0.5` 分类阈值结果，也不保留 `_thr` 后缀列；阈值相关指标已作为正式分类指标展示。",
    "- `Balanced_Score` 仅作为一个工程化综合展示列，缺乏独立统计学或临床共识依据，不能作为正式科学结论的唯一依据。",
    "- 当前 `Balanced_Score = 0.45 * Test_AUC + 0.35 * max(Test_MCC, -0.25) + 0.15 * Test_ACC + 0.05 * Test_AUPRC`。",
    "- 正式解读应同时看 AUC、AUPRC、ACC、MCC、Sensitivity、Specificity 和混淆矩阵。",
    "",
    "## 9 种建模方法简要说明",
    "",
    model_methods,
    "",
    "## 全部模型指标",
    "",
    md_table(format_display(all_perf), display_cols),
    "",
    "## 按分支用 Balanced_Score 排序展示",
    "",
    md_table(format_display(branch_sorted), display_cols),
    "",
    "## 全部分支 Balanced_Score 排序前 20",
    "",
    md_table(format_display(overall_sorted[seq_len(min(20, nrow(overall_sorted))), , drop = FALSE]), display_cols),
    "",
    "## 输出文件",
    "",
    "- `all_model_metrics.tsv`：全部模型指标记录，包含 AUC、AUPRC、ACC、MCC、Sensitivity、Specificity、TP、FP、TN、FN 和 Balanced_Score。",
    "- `branch_sorted_by_balanced_score.tsv`：分支内按 Balanced_Score 排序的展示表，不代表最优模型结论。",
    "- `overall_sorted_by_balanced_score.tsv`：全部模型按 Balanced_Score 排序的展示表，不代表最优模型结论。",
    "- `model_performance_summary.xlsx`：以上内容的 Excel 汇总版。"
  )
  writeLines(md, file.path(out_performance, "MODEL_PERFORMANCE_SUMMARY.md"), useBytes = TRUE)
  cat("Performance rows:", nrow(all_perf), "\n")
  cat("Performance outdir:", out_performance, "\n")
  invisible(all_perf)
}

extract_after <- function(lines, label) {
  pattern <- paste0("^", label, ":\\s*")
  hit <- grep(pattern, lines, value = TRUE)
  if (!length(hit)) {
    return(NA_character_)
  }
  trimws(sub(pattern, "", hit[[1]]))
}

extract_param <- function(lines, name) {
  pattern <- paste0("^\\|\\s*", name, "\\s*\\|")
  hit <- grep(pattern, lines, value = TRUE)
  if (!length(hit)) {
    return(NA_character_)
  }
  parts <- strsplit(hit[[1]], "\\|", fixed = FALSE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) < 2) {
    return(NA_character_)
  }
  parts[[2]]
}

summarize_step2 <- function() {
  dir.create(out_step2, recursive = TRUE, showWarnings = FALSE)

  read_flowchart <- function(step2_dir) {
    branch_id <- extract_branch_id(step2_dir)
    flow_file <- file.path(step2_dir, "analysis_flowchart.md")
    if (!file.exists(flow_file)) {
      return(data.frame(
        Branch_ID = branch_id,
        Branch_Label = branch_label(branch_id, step2_dir),
        Branch_Dir = basename(step2_dir),
        Flowchart = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    lines <- readLines(flow_file, warn = FALSE, encoding = "UTF-8")
    data.frame(
      Branch_ID = branch_id,
      Branch_Label = branch_label(branch_id, step2_dir),
      Branch_Dir = basename(step2_dir),
      Flowchart = TRUE,
      Input_File = extract_after(lines, "输入文件"),
      Map_File = extract_after(lines, "分组文件"),
      Samples_Total = extract_after(lines, "总样本数"),
      Features_Original = extract_after(lines, "原始特征数"),
      Split_Ratio = extract_after(lines, "划分比例"),
      Train_N = extract_after(lines, "训练集样本数"),
      Test_N = extract_after(lines, "测试集样本数"),
      Feature_Method = extract_after(lines, "方法"),
      Knee_Method = extract_after(lines, "膝点检测方法"),
      Selected_Features = extract_after(lines, "选择的特征数"),
      Use_LOOCV = extract_after(lines, "使用LOOCV"),
      LOOCV_Scope = extract_after(lines, "LOOCV范围"),
      Inner_CV = extract_after(lines, "内层交叉验证"),
      Outer_CV = extract_after(lines, "外层交叉验证"),
      Models = extract_after(lines, "训练的模型"),
      Best_Model_By_CE = extract_after(lines, "最佳模型"),
      Param_n_features = extract_param(lines, "n_features"),
      Param_feature_fraction = extract_param(lines, "feature_fraction"),
      Param_tune_evals = extract_param(lines, "tune_evals"),
      Param_tune_batch_size = extract_param(lines, "tune_batch_size"),
      Param_feature_combine_method = extract_param(lines, "feature_combine_method"),
      Param_model_subset = extract_param(lines, "model_subset"),
      Param_use_smote = extract_param(lines, "use_smote"),
      Param_smote_k = extract_param(lines, "smote_k"),
      Param_smote_dup_size = extract_param(lines, "smote_dup_size"),
      Param_seed = extract_param(lines, "seed"),
      stringsAsFactors = FALSE
    )
  }

  extract_hyperparams <- function(step2_dir) {
    workbook <- file.path(step2_dir, "model_performance_comparison.xlsx")
    if (!file.exists(workbook)) {
      return(data.frame())
    }
    branch_id <- extract_branch_id(step2_dir)
    sheets <- openxlsx::getSheetNames(workbook)
    model_sheets <- setdiff(sheets, c("模型比较", "混淆矩阵_Train", "混淆矩阵_Test", "外部验证比较", "混淆矩阵_Valid"))
    rows <- lapply(model_sheets, function(sheet) {
      x <- openxlsx::read.xlsx(workbook, sheet = sheet, colNames = FALSE)
      if (nrow(x) == 0 || ncol(x) < 2) {
        return(data.frame())
      }
      start <- which(x[[1]] == "Parameter" & x[[2]] == "Value")
      if (!length(start)) {
        return(data.frame())
      }
      i <- start[[1]] + 1L
      out <- list()
      while (i <= nrow(x)) {
        key <- x[[1]][[i]]
        val <- x[[2]][[i]]
        if (is.na(key) || !nzchar(trimws(as.character(key))) || identical(as.character(key), "特征重要性")) {
          break
        }
        out[[length(out) + 1L]] <- data.frame(
          Branch_ID = branch_id,
          Branch_Label = branch_label(branch_id, step2_dir),
          Branch_Dir = basename(step2_dir),
          Model = sheet,
          Parameter = as.character(key),
          Value = as.character(val),
          stringsAsFactors = FALSE
        )
        i <- i + 1L
      }
      if (!length(out)) {
        return(data.frame())
      }
      do.call(rbind, out)
    })
    rows <- rows[vapply(rows, nrow, integer(1)) > 0]
    if (!length(rows)) {
      return(data.frame())
    }
    do.call(rbind, rows)
  }

  summary_df <- do.call(rbind, lapply(step2_dirs, read_flowchart))
  hyper_rows <- lapply(step2_dirs, extract_hyperparams)
  hyper_rows <- hyper_rows[vapply(hyper_rows, nrow, integer(1)) > 0]
  hyper_df <- if (length(hyper_rows)) do.call(rbind, hyper_rows) else data.frame()

  write_tsv(summary_df, file.path(out_step2, "step2_technical_summary.tsv"))
  write_tsv(hyper_df, file.path(out_step2, "step2_best_hyperparameters.tsv"))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "step2_summary")
  openxlsx::writeData(wb, "step2_summary", summary_df)
  openxlsx::addWorksheet(wb, "best_hyperparameters")
  openxlsx::writeData(wb, "best_hyperparameters", hyper_df)
  openxlsx::saveWorkbook(wb, file.path(out_step2, "step2_technical_summary.xlsx"), overwrite = TRUE)

  md <- c(
    "# Step2 技术设置汇总",
    "",
    paste0("分析目录：`", root, "`"),
    "",
    paste0("本文件由 `", script_label, " --tasks=step2` 自动生成。"),
    "",
    "## 这个汇总回答什么问题",
    "",
    "- 每个 `run_model_*_step2` 的最终特征数是多少。",
    "- 内层和外层交叉验证如何设置。",
    "- 超参数优化使用多少次评估。",
    "- 每个模型最终采用的超参数是什么。",
    "",
    "## 输出文件",
    "",
    "- `step2_technical_summary.tsv`：每个分支一行，汇总特征选择、CV 和调参设置。",
    "- `step2_best_hyperparameters.tsv`：每个分支、每个模型、每个超参数一行。",
    "- `step2_technical_summary.xlsx`：以上两个表的 Excel 版本。",
    "",
    "## 注意",
    "",
    "`Best_Model_By_CE` 来自 Step2 内部按交叉验证 classification error 记录的模型，不等同于本项目最终推荐模型。当前正式交付不声明唯一最优模型。"
  )
  writeLines(md, file.path(out_step2, "README.md"), useBytes = TRUE)
  cat("Step2 rows:", nrow(summary_df), "\n")
  cat("Hyperparameter rows:", nrow(hyper_df), "\n")
  cat("Step2 outdir:", out_step2, "\n")
  invisible(summary_df)
}

rel_path <- function(path) {
  sub(paste0("^", normalizePath(root, mustWork = TRUE), "/?"), "", normalizePath(path, mustWork = FALSE))
}

read_tsv <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  utils::read.delim(path, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)
}

summarize_shap <- function() {
  dir.create(out_shap, recursive = TRUE, showWarnings = FALSE)
  status_files <- list.files(root, pattern = "^step3_shap_status\\.tsv$", recursive = TRUE, full.names = TRUE)
  status_files <- status_files[grepl("run_model_.+_step2/step3_shap", status_files)]
  if (nzchar(shap_dir_name)) {
    status_files <- status_files[basename(dirname(status_files)) == shap_dir_name]
  }
  status_files <- sort(status_files)
  if (!length(status_files)) {
    stop("No per-branch step3_shap_status.tsv files found under: ", root, call. = FALSE)
  }

  status_rows <- lapply(status_files, function(status_file) {
    step2_dir <- dirname(dirname(status_file))
    branch_id <- extract_branch_id(step2_dir)
    shap_dir <- dirname(status_file)
    x <- read_tsv(status_file)
    models <- sort(unique(x$Model))
    non_completed <- if ("Status" %in% names(x)) sum(x$Status != "completed") else NA_integer_
    shap_files <- list.files(shap_dir, pattern = "_SHAP\\.xlsx$", recursive = TRUE, full.names = TRUE)
    summary_file <- file.path(shap_dir, "step3_shap_summary.xlsx")
    data.frame(
      Branch_ID = branch_id,
      Branch_Label = branch_label(branch_id, step2_dir),
      SHAP_Status = ifelse(is.na(non_completed) || non_completed == 0, "completed", "partial_or_failed"),
      Model_Count = length(models),
      Status_Rows = nrow(x),
      SHAP_Excel_Count = length(shap_files),
      Non_Completed = non_completed,
      Models = paste(models, collapse = ","),
      Status_File = rel_path(status_file),
      Summary_File = ifelse(file.exists(summary_file), rel_path(summary_file), ""),
      stringsAsFactors = FALSE
    )
  })

  file_rows <- lapply(status_files, function(status_file) {
    step2_dir <- dirname(dirname(status_file))
    branch_id <- extract_branch_id(step2_dir)
    shap_dir <- dirname(status_file)
    shap_files <- sort(list.files(shap_dir, pattern = "_SHAP\\.xlsx$", recursive = TRUE, full.names = TRUE))
    if (!length(shap_files)) {
      return(data.frame())
    }
    data.frame(
      Branch_ID = branch_id,
      Branch_Label = branch_label(branch_id, step2_dir),
      Model = basename(dirname(shap_files)),
      SHAP_File = vapply(shap_files, rel_path, character(1)),
      stringsAsFactors = FALSE
    )
  })

  status_df <- do.call(rbind, status_rows)
  files_df <- do.call(rbind, file_rows[vapply(file_rows, nrow, integer(1)) > 0])
  write_tsv(status_df, file.path(out_shap, "shap_status_summary.tsv"))
  write_tsv(files_df, file.path(out_shap, "shap_model_files.tsv"))
  openxlsx::write.xlsx(
    list(shap_status_summary = status_df, shap_model_files = files_df),
    file = file.path(out_shap, "shap_output_summary.xlsx"),
    overwrite = TRUE
  )
  md <- c(
    "# SHAP 输出汇总",
    "",
    paste0("分析目录：`", root, "`"),
    "",
    paste0("本文件由 `", script_label, " --tasks=shap` 自动生成。"),
    "",
    "## 读取原则",
    "",
    "- SHAP 用于模型解释，不用于声明“最优模型”。",
    "- 当前汇总按每个 Step2 分支读取 `step3_shap_status.tsv` 和各模型 `*_SHAP.xlsx` 文件。",
    "- `AUC <= 0.5` 是 SHAP 跳过低性能模型的技术门槛，不是分类临界点。",
    "- SHAP PDF 图只对较长特征名自动换行，默认超过 28 个字符时优先在靠近中点的 `-` 后断为最多两行；短特征名不换行，importance、beeswarm、waterfall 和 force 均使用该规则以减少图形区域被挤压。",
    "- waterfall/force PDF 是单样本局部解释图；waterfall 以多行条形展示 Top 10 `|SHAP|` 特征；force 以较低的箭头形状单行正负累积贡献条展示同一批 Top 10 特征，SHAP 值标在箭头内，Positive 特征名按箭头起点对齐，Negative 特征名按箭头尾部右对齐，完整 SHAP 数值仍以各模型 `*_SHAP.xlsx` 为准。",
    "",
    "## 输出文件",
    "",
    "- `shap_status_summary.tsv`：每个分支一行，记录 SHAP 完成状态。",
    "- `shap_model_files.tsv`：每个模型一行，记录 SHAP Excel 文件路径。",
    "- `shap_output_summary.xlsx`：以上两个表的 Excel 版本。",
    "",
    "## 当前汇总",
    "",
    paste0("- 分支数：", nrow(status_df)),
    paste0("- SHAP 模型文件数：", nrow(files_df)),
    paste0("- 未完成状态行数：", sum(status_df$Non_Completed, na.rm = TRUE))
  )
  writeLines(md, file.path(out_shap, "SHAP_OUTPUT_SUMMARY.md"), useBytes = TRUE)
  cat("SHAP branch rows:", nrow(status_df), "\n")
  cat("SHAP model file rows:", nrow(files_df), "\n")
  cat("SHAP outdir:", out_shap, "\n")
  invisible(status_df)
}

write_performance_readme <- function() {
  dir.create(out_performance, recursive = TRUE, showWarnings = FALSE)
  md <- c(
    paste0("# ", analysis_title, " 结果解读总入口"),
    "",
    "本目录作为当前正式结果的集中解读入口。性能汇总文件物理存放在本目录；Step2 技术设置和 SHAP 汇总仍保留在同级目录中，以保持脚本输出路径和审计规则稳定。",
    "",
    "## 建议阅读顺序",
    "",
    "1. `MODEL_PERFORMANCE_SUMMARY.md`",
    "2. `all_model_metrics.tsv`",
    "3. `../02_step2_technical_summary/README.md`",
    "4. `../02_step2_technical_summary/step2_technical_summary.tsv`",
    "5. `../02_step2_technical_summary/step2_best_hyperparameters.tsv`",
    "6. `../03_shap_output_summary/SHAP_OUTPUT_SUMMARY.md`",
    "7. `../03_shap_output_summary/shap_model_files.tsv`",
    "",
    "## 解读边界",
    "",
    "- 本目录不声明唯一最优模型。",
    "- `branch_sorted_by_balanced_score.tsv` 和 `overall_sorted_by_balanced_score.tsv` 仅用于展示排序，不代表正式最优模型结论。",
    "- `Balanced_Score` 是工程化辅助展示列，不作为独立统计学或临床结论依据。",
    "- 分类阈值以各 Step2 输出记录为准，不默认使用固定 `0.5` 作为最优临界点。",
    "- SHAP 用于解释模型，不用于模型选择。"
  )
  writeLines(md, file.path(out_performance, "README.md"), useBytes = TRUE)
}

write_manual <- function() {
  write_performance_readme()
  quote_arg <- function(x) {
    if (grepl("[[:space:]]", x)) shQuote(x) else x
  }
  command_lines <- c(
    paste0("Rscript ", script_label, " \\"),
    paste0("  --root=", quote_arg(root), " \\")
  )
  if (!is.null(branch_map_file) && nzchar(branch_map_file)) {
    command_lines <- c(command_lines, paste0("  --branch-map=", quote_arg(branch_map_file), " \\"))
  }
  if (!is.null(analysis_title) && nzchar(analysis_title)) {
    command_lines <- c(command_lines, paste0("  --title=", quote_arg(analysis_title), " \\"))
  }
  command_lines <- c(command_lines, "  --tasks=all")
  md <- c(
    paste0("# ", analysis_title, " 统一结果说明书"),
    "",
    paste0("本说明书由 `", script_label, " --tasks=manual` 或 `--tasks=all` 生成。"),
    "",
    "## 一次性总汇总命令",
    "",
    "```bash",
    command_lines,
    "```",
    "",
    "## 输出目录",
    "",
    "| 模块 | 输出目录 | 作用 |",
    "|---|---|---|",
    "| 模型性能 | `01_model_performance_summary/` | 汇总所有 `run_model_*_step2` 的 AUC、AUPRC、ACC、MCC、Sensitivity、Specificity 和混淆矩阵 |",
    "| Step2 技术设置 | `02_step2_technical_summary/` | 汇总每套模型的样本数、特征数、CV、调参次数、最终超参数 |",
    "| SHAP 输出 | `03_shap_output_summary/` | 汇总每个分支、每个模型的 SHAP 完成状态和 SHAP Excel 路径 |",
    "",
    "## 结果解读原则",
    "",
    "- 不声明唯一最优模型。",
    "- `Balanced_Score` 只作为工程化辅助展示列。",
    "- 正式表格展示 AUC、AUPRC、ACC、MCC、Sensitivity、Specificity、TP、FP、TN、FN 和 Balanced_Score；其中 AUC/AUPRC 为无阈值指标，其余分类指标使用 Step2 记录的分类阈值。",
    "- 分类阈值以各 Step2 输出记录为准；当前正式分析使用 Youden 阈值，不补充固定 `0.5` 分类阈值结果。",
    "- SHAP 用于解释模型，不用于模型选择。",
    "- AUC=1 模型应作为强分离信号处理，并在正式结论前排查样本泄漏、批次效应和外部验证条件。"
  )
  writeLines(md, manual_file, useBytes = TRUE)
  cat("Manual:", manual_file, "\n")
}

valid_tasks <- c("performance", "step2", "shap", "manual")
bad_tasks <- setdiff(tasks, valid_tasks)
if (length(bad_tasks)) {
  stop("Unknown task(s): ", paste(bad_tasks, collapse = ", "), call. = FALSE)
}

cat("Result root:", root, "\n")
cat("Discovered Step2 dirs:", length(step2_dirs), "\n")
cat("Tasks:", paste(tasks, collapse = ","), "\n")

if ("performance" %in% tasks) summarize_performance()
if ("step2" %in% tasks) summarize_step2()
if ("shap" %in% tasks) summarize_shap()
if ("manual" %in% tasks) write_manual()
