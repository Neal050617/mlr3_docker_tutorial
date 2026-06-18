#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
  library(tidyverse)
  library(mlr3verse)
  library(mlr3fselect)
  library(mlr3extralearners)
  library(paradox)
  library(patchwork)
  library(progressr)
  library(lightgbm)
  library(xgboost)
})

if (requireNamespace("lgr", quietly = TRUE)) {
  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")
  lgr::get_logger("mlr3fselect")$set_threshold("warn")
}

option_list <- list(
  make_option("--rdata", type = "character", help = "Path to an existing Step2 efs.RData"),
  make_option("--outdir", type = "character", default = NULL, help = "Step2 output directory; defaults to dirname(--rdata)"),
  make_option("--max_nrounds", type = "integer", default = 500, help = "xgboost/lightgbm rounds for the diagnostic EFS redraw")
)
opts_cli <- parse_args(OptionParser(option_list = option_list))

if (is.null(opts_cli$rdata) || !file.exists(opts_cli$rdata)) {
  stop("--rdata must point to an existing efs.RData", call. = FALSE)
}

outdir <- opts_cli$outdir
if (is.null(outdir) || !nzchar(outdir)) {
  outdir <- dirname(normalizePath(opts_cli$rdata, mustWork = TRUE))
}
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

load(opts_cli$rdata, envir = .GlobalEnv)
opts$outdir <- outdir

log_message <- function(..., level = "INFO") {
  cat(sprintf("[%s] [%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, paste(..., collapse = "")))
}

if (exists("patch_mlr3learners_xgboost_compat", mode = "function")) {
  patch_mlr3learners_xgboost_compat()
}

make_efs_diagnostic_learners <- function(max_nrounds) {
  learners <- list(
    lrn("classif.xgboost", id = "xgb", nrounds = max_nrounds, verbose = 0),
    lrn("classif.ranger", id = "rf", importance = "permutation"),
    lrn("classif.svm", id = "svm", type = "C-classification", kernel = "linear"),
    lrn(
      "classif.lightgbm",
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
  )
  learners
}

task_for_efs <- if (exists("task_train_efs")) {
  task_train_efs
} else if (exists("task_train")) {
  task_train$clone()
} else {
  stop("efs.RData does not contain task_train_efs or task_train", call. = FALSE)
}

fselector <- if (exists("rfe_adaptive")) {
  rfe_adaptive
} else {
  fs("rfe", n_features = opts$n_features, feature_fraction = opts$feature_fraction)
}

init_rsmp <- if (exists("init_resampling_adaptive")) init_resampling_adaptive else init_resampling
inner_rsmp <- if (exists("inner_resampling_adaptive")) inner_resampling_adaptive else inner_resampling

svm_rfe <- clbk("mlr3fselect.svm_rfe")
one_se_clbk <- clbk("mlr3fselect.one_se_rule")
callbacks <- list(
  xgb = list(one_se_clbk),
  rf = list(one_se_clbk),
  svm = list(one_se_clbk, svm_rfe),
  lgbm = list(one_se_clbk)
)

learners_efs_diagnostic <- make_efs_diagnostic_learners(opts_cli$max_nrounds)
log_message("EFS diagnostic learners: ", paste(vapply(learners_efs_diagnostic, function(x) x$id, character(1)), collapse = ","))

efs_diagnostic <- safe_ensemble_fselect(
  fselector = fselector,
  task = task_for_efs,
  learners = learners_efs_diagnostic,
  init_resampling = init_rsmp,
  inner_resampling = inner_rsmp,
  inner_measure = msr("classif.ce"),
  measure = msr("classif.acc"),
  terminator = trm("none"),
  callbacks = callbacks,
  store_benchmark_result = TRUE,
  store_models = FALSE
)

p1 <- autoplot(efs_diagnostic, type = "performance", theme = theme_minimal(base_size = 14, base_family = plot_font_family)) +
  ggtitle("EFS Performance Comparison")
save_pdf_plot(file.path(outdir, "step1.1.efs.performance.pdf"), p1, width = 12.5, height = 12, units = "in", dpi = 300)

p2 <- autoplot(efs_diagnostic, type = "n_features", theme = theme_minimal(base_size = 14, base_family = plot_font_family)) +
  ggtitle("Number of Selected Features")
save_pdf_plot(file.path(outdir, "step1.2.efs.n_features.pdf"), p2, width = 12.5, height = 12, units = "in", dpi = 300)

p3 <- autoplot(efs_diagnostic, type = "pareto", theme = theme_minimal(base_size = 14, base_family = plot_font_family)) +
  ggtitle("Empirical Pareto Front")
save_pdf_plot(file.path(outdir, "step1.3.efs.pareto.pdf"), p3, width = 12.5, height = 12, units = "in", dpi = 300)

p4 <- autoplot(
  efs_diagnostic,
  type = "pareto",
  estimate = TRUE,
  theme = theme_minimal(base_size = 14, base_family = plot_font_family)
) +
  ggtitle("Estimated Pareto Front")
save_pdf_plot(file.path(outdir, "step1.4.efs.pareto_estimated.pdf"), p4, width = 12.5, height = 12, units = "in", dpi = 300)

p5 <- autoplot(efs_diagnostic, type = "stability", theme = theme_minimal(base_size = 14, base_family = plot_font_family)) +
  ggtitle("Feature Selection Stability")
save_pdf_plot(file.path(outdir, "step1.5.efs.stability.pdf"), p5, width = 12.5, height = 12, units = "in", dpi = 300)

diagnostic_summary <- data.frame(
  Learner = vapply(learners_efs_diagnostic, function(x) x$id, character(1)),
  Source = "redraw_mlr3_efs_diagnostics",
  stringsAsFactors = FALSE
)
write.table(
  diagnostic_summary,
  file.path(outdir, "efs_diagnostic_learners.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

saveRDS(efs_diagnostic, file.path(outdir, "efs_diagnostic_4learner.rds"))
log_message("Saved EFS diagnostic plots and efs_diagnostic_4learner.rds to: ", outdir)
