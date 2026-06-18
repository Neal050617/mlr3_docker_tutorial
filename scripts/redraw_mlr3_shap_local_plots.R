#!/usr/bin/env Rscript

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)][1])
script_dir <- if (!is.na(script_file) && nzchar(script_file)) dirname(normalizePath(script_file)) else getwd()
local_lib_candidates <- unique(c(file.path(getwd(), ".r_libs"), file.path(script_dir, ".r_libs")))
existing_local_libs <- local_lib_candidates[dir.exists(local_lib_candidates)]
if (length(existing_local_libs) > 0) {
  .libPaths(c(existing_local_libs, .libPaths()))
}

required_pkgs <- c("optparse", "openxlsx", "ggplot2")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("缺少依赖包：", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(optparse)
  library(openxlsx)
  library(ggplot2)
})

option_list <- list(
  make_option(c("--root"), type = "character", default = "", help = "结果根目录"),
  make_option(c("--shap-dir-name"), type = "character", default = "", help = "指定 Step3 SHAP 输出目录名"),
  make_option(c("--local-display-features"), type = "integer", default = 10, help = "waterfall/force 单样本图展示 Top N 特征"),
  make_option(c("--main-display-features"), type = "integer", default = 30, help = "importance/beeswarm 主图展示 Top N 特征"),
  make_option(c("--feature-label-width"), type = "integer", default = 28, help = "特征名超过该字符数后按 '-' 优先断为最多两行"),
  make_option(c("--plots"), type = "character", default = "waterfall,force", help = "要重画的图，逗号分隔：waterfall,force,importance,beeswarm,all")
)

opts <- parse_args(OptionParser(option_list = option_list))
if (!nzchar(opts$root) || !dir.exists(opts$root)) {
  stop("--root 必须指向已存在的结果根目录", call. = FALSE)
}
if (!nzchar(opts$`shap-dir-name`)) {
  stop("--shap-dir-name 必须指定，避免混入历史 SHAP 目录", call. = FALSE)
}

plot_types <- strsplit(opts$plots, ",", fixed = TRUE)[[1]]
plot_types <- trimws(plot_types[nzchar(trimws(plot_types))])
if ("all" %in% plot_types) {
  plot_types <- c("waterfall", "force", "importance", "beeswarm")
}
plot_types <- intersect(plot_types, c("waterfall", "force", "importance", "beeswarm"))
if (length(plot_types) == 0) {
  stop("--plots 至少包含 waterfall、force、importance 或 beeswarm", call. = FALSE)
}

sanitize_filename <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
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

write_plot <- function(filename, plot_obj, width = 11, height = 7) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  cleanup_numbered_pdf_siblings(filename)
  tmp_file <- paste0(filename, ".tmp")
  if (file.exists(tmp_file)) unlink(tmp_file)
  ggplot2::ggsave(tmp_file, plot_obj, width = width, height = height, units = "in", device = "pdf")
  if (file.exists(filename)) unlink(filename)
  if (!file.rename(tmp_file, filename)) {
    stop("无法写出 PDF：", filename, call. = FALSE)
  }
  cleanup_numbered_pdf_siblings(filename)
}

wrap_one_label <- function(one, width) {
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

wrap_label <- function(x, width = 18) {
  width <- max(4L, as.integer(width))
  vapply(as.character(x), function(one) {
    wrap_one_label(one, width = width)
  }, character(1), USE.NAMES = FALSE)
}

label_text_size <- function(raw_labels, base = 8.5, min_size = 6.2) {
  max_width <- max(nchar(as.character(raw_labels), type = "width"), na.rm = TRUE)
  if (!is.finite(max_width)) max_width <- 0
  if (max_width > 70) return(min_size)
  if (max_width > 52) return(max(min_size, base - 1.4))
  if (max_width > 38) return(max(min_size, base - 0.8))
  base
}

select_top_shap <- function(shap_row, n) {
  feature_names <- names(shap_row)
  values <- as.numeric(shap_row)
  keep <- is.finite(values) & !is.na(values)
  feature_names <- feature_names[keep]
  values <- values[keep]
  if (length(values) == 0) {
    return(data.frame(Feature = character(), SHAP = numeric()))
  }
  ord <- order(abs(values), decreasing = TRUE)
  ord <- ord[seq_len(min(n, length(ord)))]
  data.frame(
    Feature = feature_names[ord],
    SHAP = values[ord],
    stringsAsFactors = FALSE
  )
}

make_force_plot <- function(df, title, subtitle) {
  df <- df[order(abs(df$SHAP), decreasing = TRUE), , drop = FALSE]
  df$Direction <- ifelse(df$SHAP >= 0, "Positive", "Negative")

  make_segments <- function(x) {
    if (nrow(x) == 0) return(x)
    running <- cumsum(x$SHAP)
    previous <- c(0, head(running, -1))
    x$xstart <- previous
    x$xend <- running
    x
  }

  segments <- rbind(
    make_segments(df[df$SHAP >= 0, , drop = FALSE]),
    make_segments(df[df$SHAP < 0, , drop = FALSE])
  )
  segments$Segment <- seq_len(nrow(segments))
  segments$mid <- (segments$xstart + segments$xend) / 2
  segments$width <- abs(segments$xend - segments$xstart)
  label_width <- max(8L, as.integer(opts$`feature-label-width`))
  segments$FeatureLabel <- wrap_label(segments$Feature, width = label_width)
  segments$ValueLabel <- sprintf("%.3g", segments$SHAP)
  segments$FeatureY <- ifelse(segments$Direction == "Positive", 1.12, 0.88)
  segments$FeatureX <- segments$xstart
  segments$FeatureHJust <- ifelse(segments$Direction == "Positive", 0, 1)
  feature_text_size <- label_text_size(segments$Feature, base = 1.9, min_size = 1.45)

  x_range <- range(c(segments$xstart, segments$xend, 0), finite = TRUE)
  total_width <- diff(x_range)
  if (!is.finite(total_width) || total_width <= 0) {
    total_width <- sum(abs(segments$SHAP))
  }
  if (!is.finite(total_width) || total_width <= 0) {
    total_width <- 1
  }

  make_arrow <- function(i) {
    row <- segments[i, , drop = FALSE]
    y_mid <- 1
    y_bottom <- 0.972
    y_top <- 1.028
    x0 <- row$xstart[[1]]
    x1 <- row$xend[[1]]
    width <- abs(x1 - x0)
    head <- min(width * 0.45, total_width * 0.035)
    if (!is.finite(head) || head <= 0) head <- width * 0.45

    if (row$Direction[[1]] == "Positive") {
      x <- c(x0, x1 - head, x1, x1 - head, x0)
    } else {
      x <- c(x0, x1 + head, x1, x1 + head, x0)
    }
    data.frame(
      Segment = row$Segment[[1]],
      Direction = row$Direction[[1]],
      x = x,
      y = c(y_bottom, y_bottom, y_mid, y_top, y_top),
      stringsAsFactors = FALSE
    )
  }

  polygon_df <- do.call(rbind, lapply(seq_len(nrow(segments)), make_arrow))
  value_df <- segments[segments$width >= max(total_width * 0.045, .Machine$double.eps), , drop = FALSE]
  feature_df <- segments[segments$width >= max(total_width * 0.025, .Machine$double.eps), , drop = FALSE]

  ggplot() +
    geom_polygon(data = polygon_df, aes(x = x, y = y, group = Segment, fill = Direction),
      color = "white", linewidth = 0.35
    ) +
    geom_vline(xintercept = 0, color = "grey30", linewidth = 0.45) +
    geom_text(data = value_df, aes(x = mid, y = 1, label = ValueLabel),
      size = 1.95, color = "white", fontface = "bold"
    ) +
    geom_text(data = feature_df, aes(x = FeatureX, y = FeatureY, label = FeatureLabel, hjust = FeatureHJust),
      size = feature_text_size, color = "grey20", angle = 90, lineheight = 0.86
    ) +
    scale_y_continuous(NULL, breaks = NULL, limits = c(0.42, 1.58), expand = c(0, 0)) +
    scale_fill_manual(values = c(Positive = "#C7505A", Negative = "#2F6F9F")) +
    labs(title = title, subtitle = subtitle, x = "Cumulative SHAP contribution") +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

make_waterfall_plot <- function(df, title, subtitle) {
  df <- df[order(abs(df$SHAP), decreasing = TRUE), , drop = FALSE]
  df$Step <- seq_len(nrow(df))
  df$Direction <- ifelse(df$SHAP >= 0, "Positive", "Negative")
  df$FeaturePlot <- factor(df$Step, levels = rev(df$Step))
  feature_labels <- stats::setNames(wrap_label(df$Feature, width = opts$`feature-label-width`), df$Step)
  axis_size <- label_text_size(df$Feature, base = 8.8, min_size = 6.4)
  ggplot(df, aes(x = FeaturePlot, y = SHAP, fill = Direction)) +
    geom_col(width = 0.72) +
    coord_flip() +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.4) +
    scale_x_discrete(labels = feature_labels) +
    scale_fill_manual(values = c(Positive = "#C7505A", Negative = "#2F6F9F")) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "SHAP value") +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = axis_size, lineheight = 0.9),
      plot.title = element_text(face = "bold")
    )
}

make_importance_plot <- function(importance_df, title, display_features) {
  if (!all(c("Feature", "SHAP_Importance") %in% names(importance_df))) {
    return(NULL)
  }
  x <- importance_df[is.finite(importance_df$SHAP_Importance), , drop = FALSE]
  if (nrow(x) == 0) return(NULL)
  x <- x[order(x$SHAP_Importance, decreasing = TRUE), , drop = FALSE]
  x <- x[seq_len(min(display_features, nrow(x))), , drop = FALSE]
  x$FeaturePlot <- factor(seq_len(nrow(x)), levels = rev(seq_len(nrow(x))))
  feature_labels <- stats::setNames(wrap_label(x$Feature, width = opts$`feature-label-width`), seq_len(nrow(x)))
  axis_size <- label_text_size(x$Feature, base = 8.5, min_size = 5.8)

  ggplot(x, aes(x = FeaturePlot, y = SHAP_Importance)) +
    geom_col(width = 0.72, fill = "#3B6EA8") +
    coord_flip() +
    scale_x_discrete(labels = feature_labels) +
    labs(title = title, x = NULL, y = "Mean |SHAP|") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = axis_size, lineheight = 0.9),
      plot.title = element_text(face = "bold")
    )
}

make_beeswarm_plot <- function(shap_df, importance_df, title, display_features) {
  if (!("SampleID" %in% names(shap_df)) || nrow(shap_df) == 0) return(NULL)
  feature_cols <- setdiff(names(shap_df), "SampleID")
  if (length(feature_cols) == 0) return(NULL)

  if (!is.null(importance_df) && all(c("Feature", "SHAP_Importance") %in% names(importance_df))) {
    feature_order <- importance_df$Feature[order(importance_df$SHAP_Importance, decreasing = TRUE)]
    feature_order <- feature_order[feature_order %in% feature_cols]
  } else {
    feature_order <- feature_cols[order(colMeans(abs(shap_df[, feature_cols, drop = FALSE]), na.rm = TRUE), decreasing = TRUE)]
  }
  feature_order <- feature_order[seq_len(min(display_features, length(feature_order)))]
  if (length(feature_order) == 0) return(NULL)

  long_df <- do.call(rbind, lapply(feature_order, function(feature) {
    data.frame(
      Feature = feature,
      SHAP = as.numeric(shap_df[[feature]]),
      stringsAsFactors = FALSE
    )
  }))
  long_df <- long_df[is.finite(long_df$SHAP), , drop = FALSE]
  if (nrow(long_df) == 0) return(NULL)

  long_df$FeaturePlot <- factor(long_df$Feature, levels = rev(feature_order))
  feature_labels <- stats::setNames(wrap_label(feature_order, width = opts$`feature-label-width`), feature_order)
  axis_size <- label_text_size(feature_order, base = 8.3, min_size = 5.8)

  ggplot(long_df, aes(x = SHAP, y = FeaturePlot)) +
    geom_point(aes(color = SHAP), position = position_jitter(height = 0.18, width = 0), alpha = 0.68, size = 1.2) +
    geom_vline(xintercept = 0, color = "grey35", linewidth = 0.35) +
    scale_y_discrete(labels = feature_labels) +
    scale_color_gradient2(low = "#2F6F9F", mid = "grey80", high = "#C7505A", midpoint = 0) +
    labs(title = title, x = "SHAP value", y = NULL, color = "SHAP") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = axis_size, lineheight = 0.9),
      plot.title = element_text(face = "bold")
    )
}

find_xlsx <- function(root, shap_dir_name) {
  all_files <- list.files(root, pattern = "_SHAP\\.xlsx$", recursive = TRUE, full.names = TRUE)
  all_files[grepl(paste0("/", shap_dir_name, "/"), all_files, fixed = TRUE)]
}

xlsx_files <- find_xlsx(normalizePath(opts$root), opts$`shap-dir-name`)
if (length(xlsx_files) == 0) {
  stop("没有找到 SHAP Excel：", opts$root, call. = FALSE)
}

written <- 0L
skipped <- 0L
top_n <- max(1L, as.integer(opts$`local-display-features`))
main_n <- max(1L, as.integer(opts$`main-display-features`))
main_plot_height <- function(n, base, per_feature, max_height) {
  min(max_height, max(base, 3.5 + per_feature * max(1L, as.integer(n))))
}

for (xlsx in xlsx_files) {
  sheets <- openxlsx::getSheetNames(xlsx)
  model_name <- sub("_SHAP\\.xlsx$", "", basename(xlsx))
  model_outdir <- dirname(xlsx)
  for (dataset in c("train", "test", "valid")) {
    shap_sheet <- paste0(dataset, "_shap")
    importance_sheet <- paste0(dataset, "_importance")
    manifest_sheet <- paste0(dataset, "_waterfall_manifest")
    shap_df <- NULL
    importance_df <- NULL

    if (shap_sheet %in% sheets) {
      shap_df <- openxlsx::read.xlsx(xlsx, sheet = shap_sheet, check.names = FALSE)
    }
    if (importance_sheet %in% sheets) {
      importance_df <- openxlsx::read.xlsx(xlsx, sheet = importance_sheet, check.names = FALSE)
    }

    if ("importance" %in% plot_types && !is.null(importance_df)) {
      plot_obj <- make_importance_plot(
        importance_df,
        paste0(model_name, " - ", dataset, " SHAP Importance"),
        main_n
      )
      if (!is.null(plot_obj)) {
        plot_n <- min(main_n, nrow(importance_df))
        write_plot(
          file.path(model_outdir, paste0(dataset, "_SHAP_importance.pdf")),
          plot_obj,
          width = 10,
          height = main_plot_height(plot_n, base = 6, per_feature = 0.22, max_height = 12)
        )
        written <- written + 1L
      }
    }

    if ("beeswarm" %in% plot_types && !is.null(shap_df)) {
      plot_obj <- make_beeswarm_plot(
        shap_df,
        importance_df,
        paste0(model_name, " - ", dataset, " SHAP Beeswarm"),
        main_n
      )
      if (!is.null(plot_obj)) {
        plot_n <- min(main_n, length(setdiff(names(shap_df), "SampleID")))
        write_plot(
          file.path(model_outdir, paste0(dataset, "_SHAP_beeswarm.pdf")),
          plot_obj,
          width = 10,
          height = main_plot_height(plot_n, base = 8, per_feature = 0.25, max_height = 14)
        )
        written <- written + 1L
      }
    }

    if (!(("waterfall" %in% plot_types) || ("force" %in% plot_types))) next
    if (is.null(shap_df) || !(manifest_sheet %in% sheets)) next

    manifest <- openxlsx::read.xlsx(xlsx, sheet = manifest_sheet, check.names = FALSE)
    if (!("SampleID" %in% names(shap_df)) || nrow(shap_df) == 0 || nrow(manifest) == 0) next

    manifest <- manifest[as.logical(manifest$Matched), , drop = FALSE]
    if (nrow(manifest) == 0) next

    for (i in seq_len(nrow(manifest))) {
      sample_id <- as.character(manifest$SampleID[[i]])
      plot_label <- as.character(manifest$PlotLabel[[i]])
      row_idx <- match(sample_id, as.character(shap_df$SampleID))
      if (is.na(row_idx)) {
        skipped <- skipped + 1L
        next
      }

      shap_row <- shap_df[row_idx, setdiff(names(shap_df), "SampleID"), drop = FALSE]
      top_df <- select_top_shap(shap_row[1, ], top_n)
      if (nrow(top_df) == 0) {
        skipped <- skipped + 1L
        next
      }

      safe_sample_id <- sanitize_filename(sample_id)
      if (!nzchar(safe_sample_id)) safe_sample_id <- plot_label
      subtitle <- paste0("Sample: ", sample_id, " | Top ", nrow(top_df), " features by |SHAP|")

      if ("waterfall" %in% plot_types) {
        filename <- file.path(model_outdir, paste0(dataset, "_SHAP_waterfall_", plot_label, "_", safe_sample_id, ".pdf"))
        write_plot(
          filename,
          make_waterfall_plot(top_df, paste0(model_name, " - ", dataset, " SHAP Waterfall"), subtitle)
        )
        written <- written + 1L
      }

      if ("force" %in% plot_types) {
        filename <- file.path(model_outdir, paste0(dataset, "_SHAP_force_", plot_label, "_", safe_sample_id, ".pdf"))
        write_plot(
          filename,
          make_force_plot(top_df, paste0(model_name, " - ", dataset, " SHAP Force"), subtitle)
        )
        written <- written + 1L
      }
    }
  }
}

cat("SHAP plots redrawn:", written, "\n")
cat("Skipped targets:", skipped, "\n")
cat("Top N:", top_n, "\n")
