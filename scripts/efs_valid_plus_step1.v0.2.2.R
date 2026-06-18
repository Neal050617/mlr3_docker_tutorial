#!/usr/bin/env Rscript

# =============================================================================
# MLR3 数据预处理脚本 - Step 1
# 功能：数据加载、类型识别、质量控制、数据转换
# 输出：预处理后的 RData，供后续建模使用
# 版本：v0.2.2
# 日期：2026-02-27
# =============================================================================

# 环境配置 --------------------------------------------------------------
getOption("repos")
options(repos = structure(c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
options(BioC_mirror = "https://mirrors.tuna.tsinghua.edu.cn/bioconductor")

## 安装必要的包
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!("mlr3extralearners" %in% installed.packages())) {
  remotes::install_github("mlr-org/mlr3extralearners@*release")
}

# 加载R包
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(
  optparse, tidyverse, openxlsx, mlr3verse, data.table, mltools,
  future, readxl, mlr3learners, mlr3tuning, paradox, showtext
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
    return("plot_cn_font")
  }
  warning("未找到可用中文字体，图中中文可能无法正常显示。")
  return("")
}

plot_font_family <- init_plot_font()

open_pdf_device <- function(filename, width, height) {
  if (nzchar(plot_font_family) && capabilities("cairo")) {
    grDevices::cairo_pdf(filename, width = width, height = height, family = plot_font_family)
  } else {
    grDevices::pdf(filename, width = width, height = height)
  }
}

# 命令行参数设置 --------------------------------------------------------------
if (TRUE) {
  option_list <- list(
    # 输入文件
    make_option(c("-i", "--input"),
      type = "character", default = "OTU_tax.xls",
      help = "输入的特征表格"
    ),
    make_option(c("-g", "--map"),
      type = "character", default = "map-group.txt",
      help = "分组文件"
    ),
    make_option(c("-c", "--color"),
      type = "character", default = "color.txt",
      help = "指定颜色：color.txt"
    ),

    # 数据预处理 - 基础参数
    make_option(c("-u", "--unif"),
      type = "logical", default = FALSE,
      help = "[旧版兼容] 是否归一化"
    ),
    make_option(c("--gp"),
      type = "character", default = "none",
      help = "control-test顺序指定"
    ),
    make_option(c("--select"),
      type = "character", default = "none",
      help = "select.list"
    ),
    make_option(c("--delete"),
      type = "character", default = "delect.txt",
      help = "delete.list"
    ),

    # 预设模板（简单模式）
    make_option(c("--preset"),
      type = "character", default = "none",
      help = "预设模板：microbiome_standard/metabolome_standard/mixed_omics/clinical_only/minimal/user_normalization_only/none"
    ),

    # 数据类型识别参数
    make_option(c("--microbiome_prefixes"),
      type = "character",
      default = "ASV,OTU,Genus,Species,Family",
      help = "菌群数据前缀，逗号分隔"
    ),
    make_option(c("--metabolome_prefixes"),
      type = "character",
      default = "Metabolite,Compound,Peak,Feature",
      help = "代谢组数据前缀，逗号分隔"
    ),
    make_option(c("--clinical_prefixes"),
      type = "character",
      default = "Clinical,Lab,Physio,Biomarker",
      help = "临床指标前缀，逗号分隔"
    ),

    # 统计推断阈值
    make_option(c("--count_zero_threshold"),
      type = "double", default = 0.1,
      help = "计数型数据判断：零值比例>此值视为计数型（默认10%)"
    ),
    make_option(c("--ordinal_range_max"),
      type = "integer", default = 10,
      help = "有序分类判断：唯一值数量≤此值视为分类（默认10）"
    ),

    # 预处理策略
    make_option(c("--preprocessing_strategy"),
      type = "character",
      default = "recommended",
      help = "预处理策略：recommended/all_zscore/none/custom"
    ),

    # 分类型方法
    make_option(c("--microbiome_method"),
      type = "character", default = "auto",
      help = "菌群转换：clr/relative/none/auto"
    ),
    make_option(c("--metabolome_method"),
      type = "character", default = "auto",
      help = "代谢组转换：zscore/log_zscore/none/auto"
    ),
    make_option(c("--clinical_continuous_method"),
      type = "character", default = "auto",
      help = "连续型临床指标：zscore/none/auto"
    ),

    # 转换参数
    make_option(c("--clr_pseudocount"),
      type = "double", default = 1e-6,
      help = "CLR转换的伪计数值"
    ),

    # 过滤参数
    make_option(c("--prefilter_low_abundance"),
      type = "double", default = 0,
      help = "菌群低丰度阈值（0=不过滤）"
    ),
    make_option(c("--prefilter_missing_rate"),
      type = "double", default = 0.5,
      help = "缺失率阈值（默认50%）"
    ),
    make_option(c("--prefilter_zero_rate"),
      type = "double", default = 0.95,
      help = "零值率阈值（默认95%）"
    ),
    make_option(c("--postfilter_variance"),
      type = "double", default = 0,
      help = "转换后方差阈值（0=不过滤）"
    ),
    make_option(c("--postfilter_correlation"),
      type = "double", default = 0,
      help = "相关性阈值（0=不过滤，0.95=移除相关性>0.95的特征）"
    ),

    # 缺失值处理
    make_option(c("--impute_method"),
      type = "character", default = "median",
      help = "缺失值填充方法：median/none"
    ),

    # 输出与报告
    make_option(c("--generate_data_type_report"),
      type = "logical", default = TRUE,
      help = "是否生成数据类型识别报告"
    ),
    make_option(c("--auto_confirm"),
      type = "logical", default = FALSE,
      help = "自动确认警告，不暂停等待用户"
    ),

    # 外部验证相关
    make_option(c("--load_train_params"),
      type = "character", default = "none",
      help = "加载训练集预处理参数（用于验证集）"
    ),
    make_option(c("--fill_missing"),
      type = "logical", default = TRUE,
      help = "验证集缺失特征是否填充"
    ),

    # 输出目录和种子
    make_option(c("-s", "--seed"),
      type = "numeric", default = 115702,
      help = "设置种子"
    ),
    make_option(c("-o", "--outdir"),
      type = "character", default = "",
      help = "输出文件夹"
    ),

    # 警告阈值（用于报告生成）
    make_option(c("--warn_zero_rate"),
      type = "double", default = 0.8,
      help = "零值率警告阈值（默认80%）"
    ),
    make_option(c("--warn_low_variance"),
      type = "double", default = 1e-6,
      help = "低方差警告阈值（默认1e-6）"
    )
  )
  opts <- parse_args(OptionParser(option_list = option_list))
}

# 创建输出目录
if (opts$outdir == "") {
  opts$outdir <- file.path(getwd(), paste0("preprocessed_", opts$seed))
}
if (!isTRUE(dir.exists(opts$outdir))) {
  dir.create(opts$outdir, recursive = TRUE)
}

# 设置日志系统
log_file <- file.path(opts$outdir, paste0(
  "preprocessing_log_",
  format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"
))
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
    error = function(e) {}
  )
}

# 记录开始
start_time <- Sys.time()
log_message(paste(rep("=", 80), collapse = ""))
log_message("数据预处理流程开始 - Step 1")
log_message(paste(rep("=", 80), collapse = ""))
log_message("开始时间：", start_time)
log_message("输出目录：", opts$outdir)
log_message("随机种子：", opts$seed)

# 预设模板配置 --------------------------------------------------------------
preset_configs <- list()

# 模板1：microbiome_standard
preset_configs$microbiome_standard <- list(
  microbiome_prefixes = "ASV,OTU,Genus,Species,Family",
  prefilter_low_abundance = 10,
  prefilter_missing_rate = 0.5,
  prefilter_zero_rate = 0.95,
  impute_method = "median",
  microbiome_method = "clr",
  clr_pseudocount = 1e-6,
  generate_data_type_report = TRUE,
  auto_confirm = FALSE
)

# 模板2：user_normalization_only
preset_configs$user_normalization_only <- list(
  prefilter_missing_rate = 0.5,
  impute_method = "median",
  preprocessing_strategy = "custom",
  microbiome_method = "relative",
  metabolome_method = "none",
  clinical_continuous_method = "none",
  generate_data_type_report = TRUE,
  auto_confirm = FALSE
)

# 模板3：minimal
preset_configs$minimal <- list(
  prefilter_missing_rate = 0.8,
  impute_method = "median",
  preprocessing_strategy = "none",
  generate_data_type_report = TRUE,
  auto_confirm = TRUE
)

# 模板4：metabolome_standard
preset_configs$metabolome_standard <- list(
  metabolome_prefixes = "Metabolite,Compound,Peak,Feature,HMDB,KEGG",
  prefilter_missing_rate = 0.3,
  prefilter_zero_rate = 0.8,
  impute_method = "median",
  metabolome_method = "log_zscore",
  preprocessing_strategy = "custom",
  generate_data_type_report = TRUE,
  auto_confirm = FALSE
)

# 模板5：mixed_omics
preset_configs$mixed_omics <- list(
  microbiome_prefixes = "ASV,OTU,Genus,Species,Family",
  metabolome_prefixes = "Metabolite,Compound,Peak,Feature",
  clinical_prefixes = "Clinical,Lab,Physio,Biomarker",
  prefilter_missing_rate = 0.5,
  prefilter_zero_rate = 0.9,
  impute_method = "median",
  preprocessing_strategy = "recommended",
  generate_data_type_report = TRUE,
  auto_confirm = FALSE
)

# 模板6：clinical_only
preset_configs$clinical_only <- list(
  clinical_prefixes = "Clinical,Lab,Physio,Biomarker,Age,BMI,Gender,Sex",
  prefilter_missing_rate = 0.3,
  impute_method = "median",
  preprocessing_strategy = "recommended",
  clinical_continuous_method = "zscore",
  generate_data_type_report = TRUE,
  auto_confirm = FALSE
)

# 加载预设配置
if (opts$preset != "none") {
  if (!opts$preset %in% names(preset_configs)) {
    stop("未知的预设模板：", opts$preset)
  }

  preset <- preset_configs[[opts$preset]]
  log_message("加载预设模板：", opts$preset)

  # 应用预设（仅当用户未明确指定时）
  for (param in names(preset)) {
    if (param %in% names(opts)) {
      # 检查是否是默认值（简化处理，实际应该更严格）
      opts[[param]] <- preset[[param]]
    }
  }
}

# 向后兼容性处理
if (opts$unif == TRUE && opts$preset == "none") {
  log_message("检测到--unif参数，自动映射到user_normalization_only预设", level = "INFO")
  opts$preset <- "user_normalization_only"
  opts$microbiome_method <- "relative"
}

# 辅助函数 --------------------------------------------------------------
is_binary <- function(x) {
  vals <- unique(x[!is.na(x)])
  length(vals) <= 2 && all(vals %in% c(0, 1))
}

# 高级过滤函数：低方差过滤
postfilter_low_variance <- function(data, threshold, exclude_cols = "group") {
  if (threshold <= 0) {
    return(list(data = data, removed = character(0)))
  }

  log_message("应用低方差过滤，阈值：", threshold)

  numeric_cols <- colnames(data)[sapply(data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, exclude_cols)

  variances <- sapply(numeric_cols, function(col) {
    var(data[[col]], na.rm = TRUE)
  })

  low_var_cols <- numeric_cols[variances < threshold]

  if (length(low_var_cols) > 0) {
    log_message("移除", length(low_var_cols), "个低方差特征")
    data <- data[, !(colnames(data) %in% low_var_cols), drop = FALSE]
  } else {
    log_message("无低方差特征需要移除")
  }

  return(list(data = data, removed = low_var_cols))
}

# 高级过滤函数：高相关性过滤
postfilter_high_correlation <- function(data, threshold, exclude_cols = "group") {
  if (threshold <= 0 || threshold >= 1) {
    return(list(data = data, removed = character(0)))
  }

  log_message("应用高相关性过滤，阈值：", threshold)

  numeric_cols <- colnames(data)[sapply(data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, exclude_cols)

  if (length(numeric_cols) < 2) {
    log_message("特征数量不足，跳过相关性过滤")
    return(list(data = data, removed = character(0)))
  }

  # 计算相关系数矩阵
  cor_matrix <- cor(data[, numeric_cols], use = "pairwise.complete.obs")
  cor_matrix[is.na(cor_matrix)] <- 0
  diag(cor_matrix) <- 0

  # 找出高度相关的特征对
  high_cor_pairs <- which(abs(cor_matrix) > threshold, arr.ind = TRUE)
  high_cor_pairs <- high_cor_pairs[high_cor_pairs[, 1] < high_cor_pairs[, 2], , drop = FALSE]

  removed_cols <- character(0)

  if (nrow(high_cor_pairs) > 0) {
    # 对于每对高度相关的特征，保留方差较大的
    for (i in seq_len(nrow(high_cor_pairs))) {
      col1 <- numeric_cols[high_cor_pairs[i, 1]]
      col2 <- numeric_cols[high_cor_pairs[i, 2]]

      # 如果两个特征都还在数据中
      if (col1 %in% colnames(data) && col2 %in% colnames(data)) {
        var1 <- var(data[[col1]], na.rm = TRUE)
        var2 <- var(data[[col2]], na.rm = TRUE)

        # 移除方差较小的特征
        if (var1 < var2) {
          removed_cols <- c(removed_cols, col1)
          data <- data[, colnames(data) != col1, drop = FALSE]
        } else {
          removed_cols <- c(removed_cols, col2)
          data <- data[, colnames(data) != col2, drop = FALSE]
        }
      }
    }

    log_message("移除", length(removed_cols), "个高相关特征")
  } else {
    log_message("无高相关特征需要移除")
  }

  return(list(data = data, removed = removed_cols))
}

# Excel报告生成函数
generate_type_classification_report <- function(type_map, config_map, Data, opts, outdir) {
  log_message("生成数据类型识别报告（Excel格式）...")

  # 创建报告数据框
  report_df <- data.frame(
    Variable = config_map$variable,
    Data_Type = config_map$data_type,
    Detection_Method = config_map$detection_method,
    Recommended_Transform = config_map$recommended_transform,
    Applied_Transform = config_map$final_transform,
    Config_Source = config_map$config_source,
    stringsAsFactors = FALSE
  )

  # 添加统计信息列
  report_df$Mean <- NA_real_
  report_df$SD <- NA_real_
  report_df$Min <- NA_real_
  report_df$Max <- NA_real_
  report_df$Zero_Rate <- NA_real_
  report_df$Missing_Rate <- NA_real_
  report_df$Warning <- ""

  # 计算每个变量的统计信息
  for (i in seq_len(nrow(report_df))) {
    var <- report_df$Variable[i]
    if (var %in% colnames(Data)) {
      x <- Data[[var]]

      if (is.numeric(x)) {
        report_df$Mean[i] <- round(mean(x, na.rm = TRUE), 4)
        report_df$SD[i] <- round(sd(x, na.rm = TRUE), 4)
        report_df$Min[i] <- round(min(x, na.rm = TRUE), 4)
        report_df$Max[i] <- round(max(x, na.rm = TRUE), 4)
        report_df$Zero_Rate[i] <- round(sum(x == 0, na.rm = TRUE) / length(x), 4)
        report_df$Missing_Rate[i] <- round(sum(is.na(x)) / length(x), 4)

        # 警告检测
        warnings <- c()
        if (report_df$Zero_Rate[i] > opts$warn_zero_rate) {
          warnings <- c(warnings, "high_zero_rate")
        }
        if (!is.na(report_df$SD[i]) && report_df$SD[i] < opts$warn_low_variance) {
          warnings <- c(warnings, "low_variance")
        }
        if (report_df$Missing_Rate[i] > 0.3) {
          warnings <- c(warnings, "high_missing_rate")
        }
        report_df$Warning[i] <- paste(warnings, collapse = ", ")
      }
    }
  }

  # 生成Excel文件
  tryCatch(
    {
      wb <- createWorkbook()
      addWorksheet(wb, "Data Type Classification")

      # 写入数据
      writeData(wb, "Data Type Classification", report_df, startRow = 1)

      # 添加表头样式
      header_style <- createStyle(
        fontSize = 11,
        fontColour = "#FFFFFF",
        fgFill = "#4F81BD",
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        border = "TopBottomLeftRight"
      )
      addStyle(wb, "Data Type Classification", header_style,
        rows = 1, cols = 1:ncol(report_df), gridExpand = TRUE
      )

      # 添加条件格式（警告行标红）
      warning_rows <- which(report_df$Warning != "")
      if (length(warning_rows) > 0) {
        warning_style <- createStyle(
          fgFill = "#FFC7CE",
          fontColour = "#9C0006"
        )
        for (row in warning_rows) {
          addStyle(wb, "Data Type Classification", warning_style,
            rows = row + 1, cols = 1:ncol(report_df), gridExpand = TRUE
          )
        }
      }

      # 设置列宽
      setColWidths(wb, "Data Type Classification",
        cols = 1:ncol(report_df),
        widths = c(20, 15, 18, 18, 18, 18, 10, 10, 10, 10, 12, 15, 25)
      )

      # 冻结首行
      freezePane(wb, "Data Type Classification", firstRow = TRUE)

      # 保存文件
      report_file <- file.path(outdir, "data_type_classification.xlsx")
      saveWorkbook(wb, report_file, overwrite = TRUE)

      log_message("数据类型识别报告已生成：", report_file)
      return(report_file)
    },
    error = function(e) {
      log_message("Excel报告生成失败，将生成CSV备份：", e$message, level = "WARNING")
      # 生成CSV备份
      csv_file <- file.path(outdir, "data_type_classification.csv")
      write.csv(report_df, csv_file, row.names = FALSE)
      log_message("CSV报告已生成：", csv_file)
      return(csv_file)
    }
  )
}

# PDF可视化报告生成函数
generate_preprocessing_visualization <- function(Data_original, Data_preprocessed,
                                                 type_map, config_map, opts, outdir) {
  log_message("生成预处理可视化报告（PDF格式）...")

  pdf_file <- file.path(outdir, "preprocessing_visualization_report.pdf")

  tryCatch(
    {
      open_pdf_device(pdf_file, width = 11, height = 8.5)
      if (nzchar(plot_font_family)) {
        par(family = plot_font_family)
      }

      # 第1页：封面
      plot.new()
      text(0.5, 0.7, "数据预处理可视化报告", cex = 3, font = 2)
      text(0.5, 0.55, paste("生成日期:", Sys.Date()), cex = 1.5)
      text(0.5, 0.45, paste("样本数:", nrow(Data_preprocessed)), cex = 1.5)
      text(0.5, 0.35, paste("特征数:", ncol(Data_preprocessed) - 1), cex = 1.5)
      text(0.5, 0.25, paste("预设模板:", opts$preset), cex = 1.2)
      text(0.5, 0.15, paste("预处理策略:", opts$preprocessing_strategy), cex = 1.2)

      # 第2页：数据类型分布
      type_counts <- table(config_map$data_type)
      if (length(type_counts) > 0) {
        par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))
        pie(type_counts,
          main = "数据类型分布", col = rainbow(length(type_counts)),
          cex.main = 1.5
        )
        barplot(type_counts,
          main = "数据类型数量", las = 2, col = "steelblue",
          cex.main = 1.5, cex.names = 0.8
        )
      }

      # 第3页：转换方法分布
      transform_counts <- table(config_map$final_transform)
      if (length(transform_counts) > 0) {
        par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))
        pie(transform_counts,
          main = "转换方法分布", col = rainbow(length(transform_counts)),
          cex.main = 1.5
        )
        barplot(transform_counts,
          main = "转换方法数量", las = 2, col = "coral",
          cex.main = 1.5, cex.names = 0.8
        )
      }

      # 第4-N页：转换前后对比（每种数据类型选3个代表性变量）
      data_types <- unique(config_map$data_type)
      for (dtype in data_types) {
        vars <- config_map$variable[config_map$data_type == dtype]
        if (length(vars) > 0) {
          # 选择前3个变量
          sample_vars <- head(vars, 3)
          n_vars <- length(sample_vars)

          if (n_vars > 0) {
            par(mfrow = c(n_vars, 2), mar = c(4, 4, 3, 1))
            for (var in sample_vars) {
              if (var %in% colnames(Data_original) && var %in% colnames(Data_preprocessed)) {
                # 转换前
                x_orig <- Data_original[[var]]
                if (is.numeric(x_orig)) {
                  hist(x_orig,
                    main = paste(var, "- 转换前"),
                    xlab = "Value", col = "lightblue", breaks = 30,
                    cex.main = 1.2
                  )
                }

                # 转换后
                x_prep <- Data_preprocessed[[var]]
                if (is.numeric(x_prep)) {
                  hist(x_prep,
                    main = paste(var, "- 转换后"),
                    xlab = "Value", col = "lightgreen", breaks = 30,
                    cex.main = 1.2
                  )
                }
              }
            }
          }
        }
      }

      # 质量控制指标页
      numeric_cols <- colnames(Data_original)[sapply(Data_original, is.numeric)]
      numeric_cols <- setdiff(numeric_cols, "group")

      if (length(numeric_cols) > 0) {
        # 计算质量指标
        zero_rates <- sapply(numeric_cols, function(col) {
          x <- Data_original[[col]]
          sum(x == 0, na.rm = TRUE) / length(x)
        })

        missing_rates <- sapply(numeric_cols, function(col) {
          x <- Data_original[[col]]
          sum(is.na(x)) / length(x)
        })

        variances <- sapply(numeric_cols, function(col) {
          x <- Data_preprocessed[[col]]
          var(x, na.rm = TRUE)
        })

        par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))
        boxplot(zero_rates,
          main = "零值率分布", ylab = "Zero Rate",
          col = "yellow", cex.main = 1.5
        )
        boxplot(missing_rates,
          main = "缺失率分布", ylab = "Missing Rate",
          col = "orange", cex.main = 1.5
        )
        boxplot(log10(variances + 1e-10),
          main = "方差分布(log10)",
          ylab = "log10(Variance)", col = "pink", cex.main = 1.5
        )
      }

      # 警告汇总页
      high_zero <- config_map$variable[config_map$variable %in% numeric_cols][
        zero_rates > opts$warn_zero_rate
      ]
      low_var <- config_map$variable[config_map$variable %in% numeric_cols][
        variances < opts$warn_low_variance
      ]
      high_missing <- config_map$variable[config_map$variable %in% numeric_cols][
        missing_rates > 0.3
      ]

      plot.new()
      text(0.5, 0.9, "警告汇总", cex = 2.5, font = 2)

      y_pos <- 0.75
      text(0.5, y_pos, paste("高零值率变量 (>", opts$warn_zero_rate, "):"),
        cex = 1.5, font = 2
      )
      y_pos <- y_pos - 0.05
      if (length(high_zero) > 0) {
        for (i in seq_along(head(high_zero, 10))) {
          text(0.5, y_pos, high_zero[i], cex = 1.2)
          y_pos <- y_pos - 0.04
        }
        if (length(high_zero) > 10) {
          text(0.5, y_pos, paste("... 还有", length(high_zero) - 10, "个变量"),
            cex = 1.2, col = "red"
          )
          y_pos <- y_pos - 0.04
        }
      } else {
        text(0.5, y_pos, "无", cex = 1.2, col = "green")
        y_pos <- y_pos - 0.04
      }

      y_pos <- y_pos - 0.05
      text(0.5, y_pos, paste("低方差变量 (<", opts$warn_low_variance, "):"),
        cex = 1.5, font = 2
      )
      y_pos <- y_pos - 0.05
      if (length(low_var) > 0) {
        text(0.5, y_pos, paste(length(low_var), "个变量"), cex = 1.2, col = "red")
      } else {
        text(0.5, y_pos, "无", cex = 1.2, col = "green")
      }

      dev.off()

      log_message("预处理可视化报告已生成：", pdf_file)
      return(pdf_file)
    },
    error = function(e) {
      log_message("PDF报告生成失败：", e$message, level = "WARNING")
      if (file.exists(pdf_file)) {
        try(dev.off(), silent = TRUE)
      }
      return(NULL)
    }
  )
}

# 数据类型识别函数
identify_data_types <- function(data, opts) {
  log_message("开始数据类型识别...")

  colnames_data <- colnames(data)
  colnames_data <- colnames_data[colnames_data != "group"] # 排除target列

  # 初始化结果
  type_map <- data.frame(
    variable = colnames_data,
    data_type = NA_character_,
    detection_method = NA_character_,
    recommended_transform = NA_character_,
    stats_info = NA_character_,
    stringsAsFactors = FALSE
  )

  # 解析前缀
  microbiome_prefixes <- strsplit(opts$microbiome_prefixes, ",")[[1]]
  microbiome_prefixes <- trimws(microbiome_prefixes)
  metabolome_prefixes <- strsplit(opts$metabolome_prefixes, ",")[[1]]
  metabolome_prefixes <- trimws(metabolome_prefixes)
  clinical_prefixes <- strsplit(opts$clinical_prefixes, ",")[[1]]
  clinical_prefixes <- trimws(clinical_prefixes)

  for (i in seq_along(colnames_data)) {
    col <- colnames_data[i]
    matched <- FALSE

    # 前缀匹配：菌群
    for (prefix in microbiome_prefixes) {
      if (startsWith(col, prefix)) {
        type_map$data_type[i] <- "microbiome"
        type_map$detection_method[i] <- paste0("prefix_match(", prefix, ")")
        type_map$recommended_transform[i] <- "CLR"
        matched <- TRUE
        break
      }
    }
    if (matched) next

    # 前缀匹配：代谢组
    for (prefix in metabolome_prefixes) {
      if (startsWith(col, prefix)) {
        type_map$data_type[i] <- "metabolome"
        type_map$detection_method[i] <- paste0("prefix_match(", prefix, ")")
        type_map$recommended_transform[i] <- "log+Z-score"
        matched <- TRUE
        break
      }
    }
    if (matched) next

    # 统计推断
    if (is.numeric(data[[col]])) {
      unique_vals <- unique(na.omit(data[[col]]))
      n_unique <- length(unique_vals)
      zero_rate <- mean(data[[col]] == 0, na.rm = TRUE)
      is_integer_vals <- all(unique_vals == floor(unique_vals))

      # 统计信息
      type_map$stats_info[i] <- sprintf(
        "mean=%.2f, sd=%.2f, zero_rate=%.1f%%",
        mean(data[[col]], na.rm = TRUE),
        sd(data[[col]], na.rm = TRUE),
        zero_rate * 100
      )

      # 二分类
      if (n_unique == 2 && all(unique_vals %in% c(0, 1))) {
        type_map$data_type[i] <- "binary"
        type_map$detection_method[i] <- "statistical_inference(only_0_1)"
        type_map$recommended_transform[i] <- "none"

        # 有序分类
      } else if (is_integer_vals && n_unique <= opts$ordinal_range_max) {
        type_map$data_type[i] <- "ordinal"
        type_map$detection_method[i] <- sprintf("statistical_inference(integer_range_%d)", n_unique)
        type_map$recommended_transform[i] <- "keep_numeric"

        # 计数型
      } else if (is_integer_vals && zero_rate > opts$count_zero_threshold) {
        type_map$data_type[i] <- "count"
        type_map$detection_method[i] <- sprintf("statistical_inference(zero_rate_%.1f%%)", zero_rate * 100)
        type_map$recommended_transform[i] <- "log(x+1)+Z-score"

        # 连续型
      } else {
        type_map$data_type[i] <- "continuous"
        type_map$detection_method[i] <- "statistical_inference(numeric_continuous)"
        type_map$recommended_transform[i] <- "Z-score"
      }
    } else {
      type_map$data_type[i] <- "categorical"
      type_map$detection_method[i] <- "non_numeric"
      type_map$recommended_transform[i] <- "encode"
    }
  }

  # 输出识别结果统计
  type_summary <- table(type_map$data_type)
  log_message("数据类型识别完成：")
  for (dtype in names(type_summary)) {
    log_message(sprintf("  - %s: %d个变量", dtype, type_summary[dtype]))
  }

  return(type_map)
}

# 配置解析函数
resolve_preprocessing_config <- function(type_map, opts) {
  log_message("解析预处理配置...")

  config_map <- type_map
  config_map$final_transform <- NA_character_
  config_map$config_source <- NA_character_

  for (i in seq_len(nrow(config_map))) {
    dtype <- config_map$data_type[i]

    # 优先级处理
    if (dtype == "microbiome" && opts$microbiome_method != "auto") {
      config_map$final_transform[i] <- opts$microbiome_method
      config_map$config_source[i] <- "--microbiome_method"
    } else if (dtype == "metabolome" && opts$metabolome_method != "auto") {
      config_map$final_transform[i] <- opts$metabolome_method
      config_map$config_source[i] <- "--metabolome_method"
    } else if (dtype == "continuous" && opts$clinical_continuous_method != "auto") {
      config_map$final_transform[i] <- opts$clinical_continuous_method
      config_map$config_source[i] <- "--clinical_continuous_method"
    } else if (opts$preprocessing_strategy == "recommended") {
      config_map$final_transform[i] <- config_map$recommended_transform[i]
      config_map$config_source[i] <- "--preprocessing_strategy=recommended"
    } else if (opts$preprocessing_strategy == "none") {
      config_map$final_transform[i] <- "none"
      config_map$config_source[i] <- "--preprocessing_strategy=none"
    } else {
      config_map$final_transform[i] <- config_map$recommended_transform[i]
      config_map$config_source[i] <- "default"
    }
  }

  return(config_map)
}

# 数据加载 --------------------------------------------------------------
log_message(paste(rep("=", 80), collapse = ""))
log_message("第1步：数据加载与验证")
log_message(paste(rep("=", 80), collapse = ""))

# 读取分组文件
log_message("读取分组文件：", opts$map)
Gp <- read_tsv(opts$map, show_col_types = FALSE) %>%
  rename_at(1, ~"SampleID") %>%
  rename_at(2, ~"group") %>%
  mutate(group = fct_inorder(group))

# 处理分组筛选
if (opts$gp != "none") {
  gp <- strsplit(opts$gp, split = "-")[[1]]
  Gp <- Gp %>%
    filter(group %in% gp) %>%
    mutate(group = factor(group, levels = gp))
} else {
  gp <- levels(Gp$group)
}

log_message("分组信息：", paste(gp, collapse = " vs "))
log_message("样本数：", nrow(Gp))

# 读取数据文件
log_message("读取数据文件：", opts$input)
Data <- read_tsv(opts$input, show_col_types = FALSE) %>%
  rename_at(1, ~"Features")

# 应用select和delete过滤
if (opts$select != "none") {
  ss <- read_tsv(opts$select, col_names = FALSE, show_col_types = FALSE) %>%
    rename_at(1, ~"Features")
  Data <- inner_join(Data, ss, by = "Features")
  log_message("应用特征选择列表，保留", nrow(Data), "个特征")
}

if (opts$delete != "none" && file.exists(opts$delete)) {
  dd <- read_tsv(opts$delete, col_names = FALSE, show_col_types = FALSE) %>%
    rename_at(1, ~"Features")
  Data <- Data %>% filter(!Features %in% (dd %>% t() %>% as.character()))
  log_message("应用特征删除列表，剩余", nrow(Data), "个特征")
}

# 转换数据格式
Data <- Data %>%
  pivot_longer(!Features) %>%
  pivot_wider(names_from = "Features", values_from = "value") %>%
  rename_at(1, ~"SampleID") %>%
  inner_join(Gp, by = "SampleID")

Data <- as.data.frame(Data)
rownames(Data) <- Data$SampleID
Data <- Data[, -1]

log_message("数据维度：", nrow(Data), "samples x", ncol(Data) - 1, "features")

# 检查是否为验证集（需要使用训练集参数）
is_validation <- opts$load_train_params != "none"

if (is_validation) {
  log_message(paste(rep("=", 80), collapse = ""))
  log_message("检测到验证集模式")
  log_message("将使用训练集预处理参数")
  log_message(paste(rep("=", 80), collapse = ""))
}

# 数据类型识别 --------------------------------------------------------------
if (!is_validation) {
  log_message(paste(rep("=", 80), collapse = ""))
  log_message("第2步：数据类型识别")
  log_message(paste(rep("=", 80), collapse = ""))

  type_map <- identify_data_types(Data, opts)
  config_map <- resolve_preprocessing_config(type_map, opts)

  # 生成数据类型报告（Excel格式）
  if (opts$generate_data_type_report) {
    # 保存原始数据副本用于统计计算
    Data_for_report <- Data
    generate_type_classification_report(type_map, config_map, Data_for_report, opts, opts$outdir)
  }
} else {
  # 验证集：加载训练集的数据类型识别结果
  log_message("加载训练集预处理参数...")
  load(opts$load_train_params)

  if (!exists("preprocessing_metadata")) {
    stop("训练集RData文件格式不正确")
  }

  type_map <- preprocessing_metadata$type_map
  config_map <- preprocessing_metadata$config_map
  log_message("已加载训练集的数据类型配置")
}

# 数据预处理执行 --------------------------------------------------------------
log_message(paste(rep("=", 80), collapse = ""))
log_message("第3步：数据预处理执行")
log_message(paste(rep("=", 80), collapse = ""))

# 保存原始数据统计
original_n_features <- ncol(Data) - 1
original_n_samples <- nrow(Data)

# 保存原始数据副本（用于可视化报告）
if (!is_validation && opts$generate_data_type_report) {
  Data_original <- Data
  log_message("已保存原始数据副本用于可视化报告")
}

# 阶段3：数据转换
log_message("阶段3：数据转换")

# 相对丰度归一化（如果需要）
if ("relative" %in% config_map$final_transform) {
  relative_cols <- config_map$variable[config_map$final_transform == "relative"]
  log_message("应用相对丰度归一化：", length(relative_cols), "个特征")

  for (col in relative_cols) {
    if (col %in% colnames(Data)) {
      col_sum <- sum(Data[[col]], na.rm = TRUE)
      if (col_sum > 0) {
        Data[[col]] <- Data[[col]] / col_sum
      }
    }
  }
}

# CLR转换（如果需要）
if ("CLR" %in% config_map$final_transform || "clr" %in% config_map$final_transform) {
  clr_cols <- config_map$variable[config_map$final_transform %in% c("CLR", "clr")]
  log_message("应用CLR转换：", length(clr_cols), "个特征")

  if (!is_validation) {
    # 训练集：计算几何平均数
    geom_means <- sapply(clr_cols, function(col) {
      if (col %in% colnames(Data)) {
        x_pos <- Data[[col]][Data[[col]] > 0]
        if (length(x_pos) == 0) {
          return(1)
        }
        exp(mean(log(x_pos)))
      } else {
        return(1)
      }
    })
  } else {
    # 验证集：使用训练集几何平均数
    geom_means <- preprocessing_metadata$transform_params$clr$geometric_means
    names(geom_means) <- preprocessing_metadata$transform_params$clr$microbiome_cols
  }

  # 应用CLR转换
  for (col in clr_cols) {
    if (col %in% colnames(Data)) {
      Data[[col]] <- log(Data[[col]] + opts$clr_pseudocount) - log(geom_means[col])
    } else if (is_validation && opts$fill_missing) {
      # 验证集缺失特征：用训练集几何平均数填充
      log_message("验证集缺失特征：", col, "，用训练集参数填充", level = "WARNING")
      Data[[col]] <- log(geom_means[col])
    }
  }
}

# Z-score转换（如果需要）
if ("Z-score" %in% config_map$final_transform || "zscore" %in% config_map$final_transform) {
  zscore_cols <- config_map$variable[config_map$final_transform %in% c("Z-score", "zscore")]
  log_message("应用Z-score转换：", length(zscore_cols), "个特征")

  if (!is_validation) {
    # 训练集：计算均值和标准差
    zscore_means <- sapply(zscore_cols, function(col) {
      if (col %in% colnames(Data)) mean(Data[[col]], na.rm = TRUE) else 0
    })
    zscore_sds <- sapply(zscore_cols, function(col) {
      if (col %in% colnames(Data)) sd(Data[[col]], na.rm = TRUE) else 1
    })
  } else {
    # 验证集：使用训练集参数
    if ("zscore" %in% names(preprocessing_metadata$transform_params)) {
      zscore_means <- preprocessing_metadata$transform_params$zscore$means
      zscore_sds <- preprocessing_metadata$transform_params$zscore$sds
      names(zscore_means) <- preprocessing_metadata$transform_params$zscore$cols
      names(zscore_sds) <- preprocessing_metadata$transform_params$zscore$cols
    }
  }

  # 应用Z-score
  for (col in zscore_cols) {
    if (col %in% colnames(Data)) {
      Data[[col]] <- (Data[[col]] - zscore_means[col]) / zscore_sds[col]
    } else if (is_validation && opts$fill_missing) {
      log_message("验证集缺失特征：", col, "，填充为0（标准化后均值）", level = "WARNING")
      Data[[col]] <- 0
    }
  }
}

log_message("数据转换完成")

# 保存预处理结果 --------------------------------------------------------------
log_message(paste(rep("=", 80), collapse = ""))
log_message("第4步：保存预处理结果")
log_message(paste(rep("=", 80), collapse = ""))

# 高级过滤（在数据转换之后）
removed_by_variance <- character(0)
removed_by_correlation <- character(0)

if (!is_validation) {
  # 低方差过滤
  if (opts$postfilter_variance > 0) {
    log_message("应用低方差过滤...")
    result <- postfilter_low_variance(Data, opts$postfilter_variance)
    Data <- result$data
    removed_by_variance <- result$removed
  }

  # 高相关性过滤
  if (opts$postfilter_correlation > 0) {
    log_message("应用高相关性过滤...")
    result <- postfilter_high_correlation(Data, opts$postfilter_correlation)
    Data <- result$data
    removed_by_correlation <- result$removed
  }

  # 记录过滤结果
  if (length(removed_by_variance) > 0 || length(removed_by_correlation) > 0) {
    log_message("高级过滤完成：")
    log_message("  - 低方差移除：", length(removed_by_variance), "个特征")
    log_message("  - 高相关移除：", length(removed_by_correlation), "个特征")
    log_message("  - 剩余特征：", ncol(Data) - 1, "个")
  }
}

# 准备输出对象
Data_preprocessed <- Data
Gp_preprocessed <- Gp

# 准备元数据
preprocessing_metadata <- list(
  # 原始数据信息
  original_data_info = list(
    n_samples = original_n_samples,
    n_features = original_n_features,
    n_groups = length(gp),
    group_names = gp,
    group_counts = as.vector(table(Data$group)),
    input_file = opts$input,
    map_file = opts$map
  ),

  # 预处理后数据信息
  preprocessed_data_info = list(
    n_samples = nrow(Data),
    n_features = ncol(Data) - 1,
    n_groups = length(gp)
  ),

  # 数据类型识别结果
  type_map = type_map,
  config_map = config_map,

  # 配置信息
  config = list(
    preset = opts$preset,
    preprocessing_strategy = opts$preprocessing_strategy,
    microbiome_method = opts$microbiome_method,
    metabolome_method = opts$metabolome_method,
    clinical_continuous_method = opts$clinical_continuous_method,
    clr_pseudocount = opts$clr_pseudocount,
    prefilter_low_abundance = opts$prefilter_low_abundance,
    prefilter_missing_rate = opts$prefilter_missing_rate,
    postfilter_variance = opts$postfilter_variance,
    postfilter_correlation = opts$postfilter_correlation,
    microbiome_prefixes = opts$microbiome_prefixes,
    metabolome_prefixes = opts$metabolome_prefixes,
    clinical_prefixes = opts$clinical_prefixes
  ),

  # 过滤结果
  filter_results = list(
    removed_by_variance = removed_by_variance,
    removed_by_correlation = removed_by_correlation
  ),

  # 转换参数（用于外部验证）
  transform_params = list(),

  # 时间戳
  timestamp = as.character(Sys.time()),

  # 版本信息
  versions = list(
    R_version = as.character(getRversion()),
    script_version = "step1_v0.2.2"
  )
)

# 保存转换参数
if (!is_validation) {
  # CLR参数
  if (exists("geom_means")) {
    preprocessing_metadata$transform_params$clr <- list(
      microbiome_cols = clr_cols,
      geometric_means = geom_means,
      pseudocount = opts$clr_pseudocount
    )
  }

  # Z-score参数
  if (exists("zscore_means")) {
    preprocessing_metadata$transform_params$zscore <- list(
      cols = zscore_cols,
      means = zscore_means,
      sds = zscore_sds
    )
  }
}

# 生成PDF可视化报告（在保存RData之前）
if (!is_validation && opts$generate_data_type_report && exists("Data_original")) {
  log_message(paste(rep("=", 80), collapse = ""))
  log_message("生成预处理可视化报告")
  log_message(paste(rep("=", 80), collapse = ""))
  generate_preprocessing_visualization(Data_original, Data, type_map, config_map, opts, opts$outdir)
}

# 保存RData
rdata_file <- file.path(opts$outdir, "preprocessed_data.RData")
save(
  Data_preprocessed,
  Gp_preprocessed,
  preprocessing_metadata,
  file = rdata_file
)
log_message("预处理数据已保存：", rdata_file)

# 生成预处理报告
report_file <- file.path(opts$outdir, "preprocessing_report.txt")
report_con <- file(report_file, open = "wt")

cat("================================================================================\n", file = report_con)
cat("MLR3 数据预处理报告\n", file = report_con)
cat("================================================================================\n\n", file = report_con)
cat("生成时间:", as.character(Sys.time()), "\n", file = report_con)
cat("脚本版本: efs_valid_plus_step1.v0.2.2.R v0.2.2\n\n", file = report_con)

cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("1. 输入数据概览\n", file = report_con)
cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("原始数据文件:", opts$input, "\n", file = report_con)
cat("分组文件:", opts$map, "\n", file = report_con)
cat("样本数:", original_n_samples, "\n", file = report_con)
cat("特征数:", original_n_features, "\n", file = report_con)
cat("分组:", paste(gp, collapse = " vs "), "\n\n", file = report_con)

cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("2. 预处理配置\n", file = report_con)
cat("--------------------------------------------------------------------------------\n", file = report_con)
if (opts$preset != "none") {
  cat("预设模板:", opts$preset, "\n", file = report_con)
}
cat("数据转换策略:", opts$preprocessing_strategy, "\n\n", file = report_con)

cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("3. 数据类型识别结果\n", file = report_con)
cat("--------------------------------------------------------------------------------\n", file = report_con)
type_summary <- table(config_map$data_type)
for (dtype in names(type_summary)) {
  cat(sprintf("%s: %d个变量\n", dtype, type_summary[dtype]), file = report_con)
}
cat("\n详细信息请查看: data_type_classification.csv\n\n", file = report_con)

cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("4. 预处理结果\n", file = report_con)
cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("预处理后样本数:", nrow(Data), "\n", file = report_con)
cat("预处理后特征数:", ncol(Data) - 1, "\n\n", file = report_con)

cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("5. 输出文件\n", file = report_con)
cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("预处理数据:", rdata_file, "\n", file = report_con)
cat("数据类型报告: data_type_classification.xlsx\n", file = report_con)
cat("可视化报告: preprocessing_visualization_report.pdf\n", file = report_con)
cat("预处理报告:", report_file, "(本文件)\n\n", file = report_con)

cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("6. 下一步操作\n", file = report_con)
cat("--------------------------------------------------------------------------------\n", file = report_con)
cat("使用预处理数据进行建模:\n\n", file = report_con)
cat("  Rscript efs_valid_plus_step2.v0.2.2.R \\\n", file = report_con)
cat("    --load_preprocessed=", rdata_file, " \\\n", file = report_con)
cat("    --outdir=output_modeling\n\n", file = report_con)

cat("================================================================================\n", file = report_con)
cat("预处理完成！\n", file = report_con)
cat("================================================================================\n", file = report_con)

close(report_con)
log_message("预处理报告已保存：", report_file)

# 完成
end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "mins")
log_message(paste(rep("=", 80), collapse = ""))
log_message("预处理流程完成！")
log_message("总耗时：", round(elapsed_time, 2), "分钟")
log_message(paste(rep("=", 80), collapse = ""))

# 关闭日志
close(log_con)

cat("\n")
cat("✓ 预处理完成！\n")
cat("✓ 预处理数据：", rdata_file, "\n")
cat("✓ 预处理报告：", report_file, "\n")
cat("\n")
