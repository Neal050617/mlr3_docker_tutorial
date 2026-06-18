#!/usr/bin/env Rscript

suppressPackageStartupMessages({
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

as_flag <- function(x) {
  tolower(as.character(x)) %in% c("1", "true", "yes", "y")
}

root_arg <- get_arg("root", NULL)
if (is.null(root_arg)) {
  stop("Missing required argument: --root=/path/to/result_root", call. = FALSE)
}

root <- normalizePath(root_arg, mustWork = TRUE)
dry_run <- as_flag(get_arg("dry-run", "false"))
backup <- as_flag(get_arg("backup", "false"))

discover_workbooks <- function(root) {
  files <- list.files(root, pattern = "^model_performance_comparison\\.xlsx$", recursive = TRUE, full.names = TRUE)
  files <- files[grepl("run_model_.+_step2/model_performance_comparison\\.xlsx$", files)]
  sort(files)
}

read_sheet <- function(path, sheet) {
  sheets <- openxlsx::getSheetNames(path)
  if (!sheet %in% sheets) {
    return(data.frame())
  }
  openxlsx::read.xlsx(path, sheet = sheet)
}

calc_sensitivity <- function(tp, fn) {
  denom <- tp + fn
  ifelse(denom > 0, tp / denom, NA_real_)
}

calc_specificity <- function(tn, fp) {
  denom <- tn + fp
  ifelse(denom > 0, tn / denom, NA_real_)
}

enrich_confusion_matrix <- function(x) {
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

threshold_map <- function(path, sheet, prefix) {
  x <- enrich_confusion_matrix(read_sheet(path, sheet))
  if (!nrow(x) || !"Model" %in% names(x) || !"Threshold" %in% names(x)) {
    return(data.frame(Model = character(0), stringsAsFactors = FALSE))
  }
  out <- x[, intersect(c("Model", "Threshold", "TP", "FP", "TN", "FN", "Sensitivity", "Specificity"), names(x)), drop = FALSE]
  if (all(c("TP", "FP", "TN", "FN") %in% names(out))) {
    out[[paste0(prefix, "_MCC_from_cm")]] <- with(
      out,
      ifelse(
        (TP + FP) > 0 & (TP + FN) > 0 & (TN + FP) > 0 & (TN + FN) > 0,
        ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)),
        NA_real_
      )
    )
    keep_cols <- c("Model", "Threshold", "Sensitivity", "Specificity", paste0(prefix, "_MCC_from_cm"))
    out <- out[, intersect(keep_cols, names(out)), drop = FALSE]
  } else {
    out <- out[, c("Model", "Threshold"), drop = FALSE]
  }
  names(out)[names(out) == "Threshold"] <- paste0(prefix, "_Threshold")
  names(out)[names(out) == "Sensitivity"] <- paste0(prefix, "_Sensitivity_from_cm")
  names(out)[names(out) == "Specificity"] <- paste0(prefix, "_Specificity_from_cm")
  out
}

merge_threshold <- function(df, path, sheet, prefix) {
  th <- threshold_map(path, sheet, prefix)
  if (!nrow(th)) {
    return(df)
  }
  refresh_cols <- c(
    paste0(prefix, "_Threshold"),
    paste0(prefix, "_Sensitivity_from_cm"),
    paste0(prefix, "_Specificity_from_cm"),
    paste0(prefix, "_MCC_from_cm")
  )
  refresh_cols <- unique(c(refresh_cols, paste0(refresh_cols, ".x"), paste0(refresh_cols, ".y")))
  df <- df[, setdiff(names(df), refresh_cols), drop = FALSE]
  merge(df, th, by = "Model", all.x = TRUE, sort = FALSE)
}

move_after <- function(cols, col, after) {
  if (!col %in% cols || !after %in% cols) {
    return(cols)
  }
  cols <- cols[cols != col]
  pos <- match(after, cols)
  append(cols, col, after = pos)
}

clean_model_sheet <- function(x, path, sheet) {
  if (!nrow(x) || ncol(x) < 4) {
    return(x)
  }
  labels <- as.character(x[[1]])
  thr_rows <- grep("_thr$", labels)

  if (length(thr_rows)) {
    for (metric in c("ACC", "CE", "Precision", "Recall", "F1")) {
      base_row <- which(labels == metric)
      thr_row <- which(labels == paste0(metric, "_thr"))
      if (length(base_row) && length(thr_row)) {
        x[base_row[[1]], 3:ncol(x)] <- x[thr_row[[1]], 3:ncol(x)]
      }
    }
  }

  mcc_row <- which(labels == "MCC")
  cm_sheet <- if ("Train" %in% as.character(x[2, ])) "混淆矩阵_Train" else "混淆矩阵_Test"
  if (length(mcc_row) && all(c("混淆矩阵_Train", "混淆矩阵_Test") %in% openxlsx::getSheetNames(path))) {
    cm_train <- threshold_map(path, "混淆矩阵_Train", "Train")
    cm_test <- threshold_map(path, "混淆矩阵_Test", "Test")
    train_mcc_col <- "Train_MCC_from_cm"
    test_mcc_col <- "Test_MCC_from_cm"
    if (train_mcc_col %in% names(cm_train)) {
      hit <- cm_train[cm_train$Model == sheet, train_mcc_col, drop = TRUE]
      if (length(hit)) x[mcc_row[[1]], 3] <- hit[[1]]
    }
    if (test_mcc_col %in% names(cm_test)) {
      hit <- cm_test[cm_test$Model == sheet, test_mcc_col, drop = TRUE]
      if (length(hit)) x[mcc_row[[1]], 4] <- hit[[1]]
    }
  }

  if (!"Threshold" %in% labels) {
    cm_train <- threshold_map(path, "混淆矩阵_Train", "Train")
    cm_test <- threshold_map(path, "混淆矩阵_Test", "Test")
    train_threshold <- if ("Train_Threshold" %in% names(cm_train)) cm_train[cm_train$Model == sheet, "Train_Threshold", drop = TRUE] else NA
    test_threshold <- if ("Test_Threshold" %in% names(cm_test)) cm_test[cm_test$Model == sheet, "Test_Threshold", drop = TRUE] else NA
    threshold_row <- x[1, , drop = FALSE]
    threshold_row[1, ] <- NA
    threshold_row[1, 1] <- "Threshold"
    if (ncol(threshold_row) >= 3 && length(train_threshold)) threshold_row[1, 3] <- train_threshold[[1]]
    if (ncol(threshold_row) >= 4 && length(test_threshold)) threshold_row[1, 4] <- test_threshold[[1]]
    insert_after <- which(labels == "Metric")
    if (length(insert_after)) {
      x <- rbind(
        x[seq_len(insert_after[[1]]), , drop = FALSE],
        threshold_row,
        x[(insert_after[[1]] + 1):nrow(x), , drop = FALSE]
      )
      labels <- as.character(x[[1]])
      thr_rows <- grep("_thr$", labels)
    }
  }

  cm_train <- threshold_map(path, "混淆矩阵_Train", "Train")
  cm_test <- threshold_map(path, "混淆矩阵_Test", "Test")
  add_metric_row <- function(x, label, train_value, test_value, after_label) {
    labels <- as.character(x[[1]])
    if (label %in% labels) {
      row <- which(labels == label)[[1]]
      if (ncol(x) >= 3 && length(train_value)) x[row, 3] <- train_value[[1]]
      if (ncol(x) >= 4 && length(test_value)) x[row, 4] <- test_value[[1]]
      return(x)
    }
    new_row <- x[1, , drop = FALSE]
    new_row[1, ] <- NA
    new_row[1, 1] <- label
    if (ncol(new_row) >= 3 && length(train_value)) new_row[1, 3] <- train_value[[1]]
    if (ncol(new_row) >= 4 && length(test_value)) new_row[1, 4] <- test_value[[1]]
    insert_after <- which(labels == after_label)
    if (!length(insert_after)) {
      insert_after <- which(labels == "Recall")
    }
    if (!length(insert_after)) {
      insert_after <- which(labels == "Precision")
    }
    if (!length(insert_after)) {
      return(rbind(x, new_row))
    }
    rbind(
      x[seq_len(insert_after[[1]]), , drop = FALSE],
      new_row,
      x[(insert_after[[1]] + 1):nrow(x), , drop = FALSE]
    )
  }
  train_sens <- if ("Train_Sensitivity_from_cm" %in% names(cm_train)) cm_train[cm_train$Model == sheet, "Train_Sensitivity_from_cm", drop = TRUE] else NA
  test_sens <- if ("Test_Sensitivity_from_cm" %in% names(cm_test)) cm_test[cm_test$Model == sheet, "Test_Sensitivity_from_cm", drop = TRUE] else NA
  train_spec <- if ("Train_Specificity_from_cm" %in% names(cm_train)) cm_train[cm_train$Model == sheet, "Train_Specificity_from_cm", drop = TRUE] else NA
  test_spec <- if ("Test_Specificity_from_cm" %in% names(cm_test)) cm_test[cm_test$Model == sheet, "Test_Specificity_from_cm", drop = TRUE] else NA
  x <- add_metric_row(x, "Sensitivity", train_sens, test_sens, "Recall")
  x <- add_metric_row(x, "Specificity", train_spec, test_spec, "Sensitivity")

  labels <- as.character(x[[1]])
  thr_rows <- grep("_thr$", labels)
  if (length(thr_rows)) {
    x[-thr_rows, , drop = FALSE]
  } else {
    x
  }
}

repair_model_comparison <- function(path) {
  sheets <- openxlsx::getSheetNames(path)
  if (!"模型比较" %in% sheets) {
    warning("Skipped workbook without 模型比较 sheet: ", path, call. = FALSE)
    return(data.frame(
      Workbook = path,
      Status = "skipped_no_model_comparison",
      Removed_Columns = "",
      stringsAsFactors = FALSE
    ))
  }

  df <- openxlsx::read.xlsx(path, sheet = "模型比较")
  original_cols <- names(df)
  thr_cols <- grep("_thr$", original_cols, value = TRUE)
  duplicate_merge_cols <- grep("\\.(x|y)$", original_cols, value = TRUE)

  metric_names <- c("ACC", "CE", "Precision", "Recall", "F1")
  prefixes <- c("Train", "Test", "Valid")
  for (prefix in prefixes) {
    for (metric in metric_names) {
      base_col <- paste(prefix, metric, sep = "_")
      thr_col <- paste0(base_col, "_thr")
      if (thr_col %in% names(df)) {
        df[[base_col]] <- df[[thr_col]]
      }
    }
  }

  if (length(thr_cols)) {
    df <- df[, setdiff(names(df), thr_cols), drop = FALSE]
  }

  df <- merge_threshold(df, path, "混淆矩阵_Train", "Train")
  df <- merge_threshold(df, path, "混淆矩阵_Test", "Test")
  df <- merge_threshold(df, path, "混淆矩阵_Valid", "Valid")

  for (prefix in prefixes) {
    mcc_col <- paste0(prefix, "_MCC")
    mcc_from_cm <- paste0(prefix, "_MCC_from_cm")
    if (mcc_from_cm %in% names(df)) {
      df[[mcc_col]] <- df[[mcc_from_cm]]
      df[[mcc_from_cm]] <- NULL
    }
  }

  cols <- names(df)
  for (prefix in prefixes) {
    threshold_col <- paste0(prefix, "_Threshold")
    auc_col <- paste0(prefix, "_AUC")
    if (threshold_col %in% cols && auc_col %in% cols) {
      cols <- move_after(cols, threshold_col, auc_col)
    }
  }
  df <- df[, cols, drop = FALSE]

  has_threshold_sheets <- any(c("混淆矩阵_Train", "混淆矩阵_Test", "混淆矩阵_Valid") %in% sheets)
  model_sheets <- setdiff(sheets, c("模型比较", "混淆矩阵_Train", "混淆矩阵_Test", "外部验证比较", "混淆矩阵_Valid"))
  model_sheet_has_thr <- vapply(model_sheets, function(sheet) {
    x <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE)
    any(grepl("_thr$", as.character(x[[1]])))
  }, logical(1))

  status <- if (length(thr_cols) || length(duplicate_merge_cols) || any(model_sheet_has_thr)) {
    "repaired"
  } else if (has_threshold_sheets) {
    "refreshed_threshold_metrics"
  } else {
    "already_clean"
  }
  if (!dry_run && (length(thr_cols) || any(model_sheet_has_thr) || has_threshold_sheets)) {
    if (backup) {
      backup_path <- sub("\\.xlsx$", ".before_threshold_column_repair.xlsx", path)
      if (!file.exists(backup_path)) {
        file.copy(path, backup_path)
      }
    }
    wb <- openxlsx::loadWorkbook(path)
    openxlsx::removeWorksheet(wb, "模型比较")
    openxlsx::addWorksheet(wb, "模型比较")
    openxlsx::writeData(wb, "模型比较", df)
    openxlsx::setColWidths(wb, "模型比较", cols = seq_len(ncol(df)), widths = "auto")
    for (sheet in intersect(c("混淆矩阵_Train", "混淆矩阵_Test", "混淆矩阵_Valid"), sheets)) {
      cm_df <- enrich_confusion_matrix(openxlsx::read.xlsx(path, sheet = sheet))
      openxlsx::removeWorksheet(wb, sheet)
      openxlsx::addWorksheet(wb, sheet)
      openxlsx::writeData(wb, sheet, cm_df)
      openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(cm_df)), widths = "auto")
    }
    for (sheet in model_sheets) {
      sheet_df <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE)
      cleaned_sheet <- clean_model_sheet(sheet_df, path, sheet)
      if (!identical(sheet_df, cleaned_sheet)) {
        openxlsx::removeWorksheet(wb, sheet)
        openxlsx::addWorksheet(wb, sheet)
        openxlsx::writeData(wb, sheet, cleaned_sheet, colNames = FALSE)
        openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(cleaned_sheet)), widths = "auto")
      }
    }
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  }

  data.frame(
    Workbook = path,
    Status = status,
    Removed_Columns = paste(c(thr_cols, duplicate_merge_cols), collapse = ","),
    stringsAsFactors = FALSE
  )
}

workbooks <- discover_workbooks(root)
if (!length(workbooks)) {
  stop("No run_model_*_step2/model_performance_comparison.xlsx files found under: ", root, call. = FALSE)
}

cat("Result root:", root, "\n")
cat("Workbooks:", length(workbooks), "\n")
cat("Dry run:", dry_run, "\n")
cat("Backup:", backup, "\n")

summary_rows <- do.call(rbind, lapply(workbooks, repair_model_comparison))
summary_path <- file.path(root, "threshold_column_repair_summary.tsv")
utils::write.table(summary_rows, summary_path, sep = "\t", quote = FALSE, row.names = FALSE, na = "")

print(summary_rows[, c("Workbook", "Status"), drop = FALSE], row.names = FALSE)
cat("Summary:", summary_path, "\n")
