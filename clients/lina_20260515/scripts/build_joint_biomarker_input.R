#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) {
    return(default)
  }
  sub(paste0("^", name, "="), "", hit[[1]])
}

input_dir <- normalizePath(get_arg("--input_dir", "lina_20260515/v0.2.2_inputs_gp01_hc_af"), mustWork = TRUE)
run_root <- normalizePath(get_arg("--run_root"), mustWork = TRUE)
out_dir <- get_arg("--out_dir", input_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_tsv <- function(df, path) {
  write.table(df, file = path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = "")
}

read_feature_table <- function(path) {
  df <- read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"Features" %in% names(df)) {
    names(df)[1] <- "Features"
  }
  df
}

extract_selected_features <- function(step2_dir) {
  rdata <- file.path(step2_dir, "efs.RData")
  if (!file.exists(rdata)) {
    stop("Missing efs.RData: ", rdata, call. = FALSE)
  }
  env <- new.env(parent = emptyenv())
  load(rdata, envir = env)
  if (!exists("selected_features", envir = env, inherits = FALSE)) {
    stop("No selected_features object in: ", rdata, call. = FALSE)
  }
  selected <- get("selected_features", envir = env, inherits = FALSE)
  selected <- unique(as.character(selected))
  selected[nzchar(selected)]
}

models <- data.frame(
  model_id = c("0.1", "0.2", "0.3", "0.4", "0.5"),
  modality = c("metabolome", "oral_bacteria", "oral_fungi", "gut_bacteria", "gut_fungi"),
  label_cn = c("非靶代谢", "口腔细菌属", "口腔真菌属", "肠道细菌属", "肠道真菌属"),
  input_file = c(
    "model_0.1_gp01_hc_af_metabolome_145.tsv",
    "model_0.2_gp01_hc_af_oral_bacteria_145.tsv",
    "model_0.3_gp01_hc_af_oral_fungi_145.tsv",
    "model_0.4_gp01_hc_af_gut_bacteria_145.tsv",
    "model_0.5_gp01_hc_af_gut_fungi_145.tsv"
  ),
  step2_tag = c(
    "run_model_0.1_metabolome_step2",
    "run_model_0.2_oral_bacteria_step2",
    "run_model_0.3_oral_fungi_step2",
    "run_model_0.4_gut_bacteria_step2",
    "run_model_0.5_gut_fungi_step2"
  ),
  stringsAsFactors = FALSE
)

manifest_path <- file.path(input_dir, "feature_manifest.tsv")
feature_manifest <- if (file.exists(manifest_path)) {
  read.delim(manifest_path, check.names = FALSE, stringsAsFactors = FALSE)
} else {
  data.frame(feature_id = character(0), raw_feature = character(0), stringsAsFactors = FALSE)
}

selected_tables <- list()
selected_manifest <- list()

for (i in seq_len(nrow(models))) {
  model <- models[i, ]
  selected <- extract_selected_features(file.path(run_root, model$step2_tag))
  if (!length(selected)) {
    stop("No selected features for ", model$step2_tag, call. = FALSE)
  }

  input_tab <- read_feature_table(file.path(input_dir, model$input_file))
  found <- selected[selected %in% input_tab$Features]
  missing <- setdiff(selected, input_tab$Features)
  if (length(missing)) {
    warning("Selected features not found in input for ", model$modality, ": ", paste(missing, collapse = ", "))
  }
  if (!length(found)) {
    stop("No selected features could be matched for ", model$modality, call. = FALSE)
  }

  selected_tables[[model$modality]] <- input_tab[match(found, input_tab$Features), , drop = FALSE]
  raw_match <- feature_manifest[match(found, feature_manifest$feature_id), , drop = FALSE]
  selected_manifest[[model$modality]] <- data.frame(
    model_id = model$model_id,
    modality = model$modality,
    label_cn = model$label_cn,
    feature_id = found,
    raw_feature = raw_match$raw_feature,
    step2_tag = model$step2_tag,
    stringsAsFactors = FALSE
  )
}

shared_samples <- Reduce(intersect, lapply(selected_tables, function(x) names(x)[-1]))
if (!length(shared_samples)) {
  stop("No shared samples in selected biomarker tables", call. = FALSE)
}

joint <- do.call(rbind, lapply(selected_tables, function(x) x[, c("Features", shared_samples), drop = FALSE]))
rownames(joint) <- NULL

out_file <- file.path(out_dir, "model_0.6_gp01_hc_af_joint_selected_biomarkers_145.tsv")
write_tsv(joint, out_file)

selected_manifest_df <- do.call(rbind, selected_manifest)
write_tsv(selected_manifest_df, file.path(out_dir, "joint_selected_biomarkers_manifest.tsv"))

summary_df <- data.frame(
  model_id = "0.6",
  modality = "joint_selected_biomarkers",
  label_cn = "5组学单模型入选biomarkers联合",
  input_file = basename(out_file),
  preset = "mixed_omics",
  samples = length(shared_samples),
  features = nrow(joint),
  group_file = "map-group_gp01_hc_af_common_145.txt",
  groups = {
    input_summary_path <- file.path(out_dir, "input_summary.tsv")
    if (file.exists(input_summary_path)) {
      input_summary <- read.delim(input_summary_path, check.names = FALSE, stringsAsFactors = FALSE)
      if ("groups" %in% names(input_summary) && nrow(input_summary)) input_summary$groups[[1]] else NA_character_
    } else {
      NA_character_
    }
  },
  stringsAsFactors = FALSE
)
write_tsv(summary_df, file.path(out_dir, "joint_selected_biomarkers_summary.tsv"))

input_summary_path <- file.path(out_dir, "input_summary.tsv")
if (file.exists(input_summary_path)) {
  input_summary <- read.delim(input_summary_path, check.names = FALSE, stringsAsFactors = FALSE)
  all_cols <- union(names(input_summary), names(summary_df))
  for (col in setdiff(all_cols, names(input_summary))) input_summary[[col]] <- NA_character_
  for (col in setdiff(all_cols, names(summary_df))) summary_df[[col]] <- NA_character_
  input_summary <- input_summary[input_summary$model_id != "0.6", all_cols, drop = FALSE]
  summary_df <- summary_df[, all_cols, drop = FALSE]
  write_tsv(rbind(input_summary, summary_df), input_summary_path)
}

cat("Prepared joint selected biomarker input:\n")
print(summary_df, row.names = FALSE)
