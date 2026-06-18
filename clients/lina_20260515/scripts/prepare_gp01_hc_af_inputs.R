#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) {
    return(default)
  }
  sub(paste0("^", name, "="), "", hit[[1]])
}

project_dir <- normalizePath(get_arg("--project_dir", "lina_20260515"), mustWork = TRUE)
out_dir <- get_arg("--out_dir", file.path(project_dir, "v0.2.2_inputs_gp01_hc_af"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sanitize_token <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- "NA"
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x[x == ""] <- "NA"
  x
}

prefix_features <- function(values, prefix) {
  make.unique(paste0(prefix, sanitize_token(values)), sep = "_dup_")
}

write_tsv <- function(df, path) {
  write.table(df, file = path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = "")
}

feature_table <- function(feature_names, sample_matrix) {
  data.frame(Features = feature_names, as.data.frame(sample_matrix, check.names = FALSE), check.names = FALSE)
}

read_map <- function(path) {
  map <- read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(map) < 2) {
    stop("map-group.txt must contain at least two columns", call. = FALSE)
  }
  names(map)[1:2] <- c("sample_id", "group")
  map$sample_id <- trimws(map$sample_id)
  map$group <- trimws(map$group)
  map <- map[map$sample_id != "" & map$group != "", c("sample_id", "group"), drop = FALSE]
  map <- map[!duplicated(map$sample_id), , drop = FALSE]
  map
}

read_feature_by_row <- function(path, feature_prefix) {
  df <- read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(df) < 2) {
    stop("Feature table has no sample columns: ", path, call. = FALSE)
  }
  feature_col <- names(df)[1]
  mat <- df[, -1, drop = FALSE]
  rownames(mat) <- df[[feature_col]]
  mat[] <- lapply(mat, function(x) as.numeric(as.character(x)))
  feature_names <- prefix_features(rownames(mat), feature_prefix)
  list(raw_features = rownames(mat), feature_names = feature_names, mat = mat)
}

read_sample_by_row <- function(path, feature_prefix) {
  df <- read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(df) < 2) {
    stop("Sample-row table has no feature columns: ", path, call. = FALSE)
  }
  sample_ids <- trimws(df[[1]])
  feature_raw <- names(df)[-1]
  mat <- as.data.frame(t(df[, -1, drop = FALSE]), check.names = FALSE)
  names(mat) <- sample_ids
  mat[] <- lapply(mat, function(x) as.numeric(as.character(x)))
  rownames(mat) <- feature_raw
  feature_names <- prefix_features(feature_raw, feature_prefix)
  list(raw_features = feature_raw, feature_names = feature_names, mat = mat)
}

subset_to_samples <- function(obj, sample_ids) {
  missing_samples <- setdiff(sample_ids, colnames(obj$mat))
  if (length(missing_samples)) {
    stop("Internal sample mismatch: ", paste(missing_samples, collapse = ", "), call. = FALSE)
  }
  feature_table(obj$feature_names, obj$mat[, sample_ids, drop = FALSE])
}

modality_defs <- data.frame(
  model_id = c("0.1", "0.2", "0.3", "0.4", "0.5"),
  modality = c("metabolome", "oral_bacteria", "oral_fungi", "gut_bacteria", "gut_fungi"),
  label_cn = c("非靶代谢", "口腔细菌属", "口腔真菌属", "肠道细菌属", "肠道真菌属"),
  input_rel = c(
    "非靶代谢/metabo_diff.txt",
    "口腔细菌/genus_oral_bacteria.xls",
    "口腔真菌/genus_oral_fungi.xls",
    "肠道细菌/genus_gut_bacteria.xls",
    "肠道真菌/genus_gut_fungi.xls"
  ),
  feature_prefix = c(
    "Metabolite_",
    "Genus_OralBacteria_",
    "Genus_OralFungi_",
    "Genus_GutBacteria_",
    "Genus_GutFungi_"
  ),
  preset = c("metabolome_standard", "microbiome_standard", "microbiome_standard", "microbiome_standard", "microbiome_standard"),
  stringsAsFactors = FALSE
)

map <- read_map(file.path(project_dir, "map-group.txt"))
if (!setequal(unique(map$group), c("HC", "AF"))) {
  stop("Expected exactly HC and AF groups, got: ", paste(unique(map$group), collapse = ", "), call. = FALSE)
}

objects <- list()
sample_sets <- list()
feature_manifest <- list()

for (i in seq_len(nrow(modality_defs))) {
  def <- modality_defs[i, ]
  path <- file.path(project_dir, def$input_rel)
  obj <- if (def$modality == "metabolome") {
    read_sample_by_row(path, def$feature_prefix)
  } else {
    read_feature_by_row(path, def$feature_prefix)
  }
  objects[[def$modality]] <- obj
  sample_sets[[def$modality]] <- intersect(colnames(obj$mat), map$sample_id)
  feature_manifest[[def$modality]] <- data.frame(
    model_id = def$model_id,
    modality = def$modality,
    label_cn = def$label_cn,
    feature_id = obj$feature_names,
    raw_feature = obj$raw_features,
    source_file = def$input_rel,
    stringsAsFactors = FALSE
  )
}

common_samples <- Reduce(intersect, sample_sets)
common_samples <- map$sample_id[map$sample_id %in% common_samples]
if (length(common_samples) == 0) {
  stop("No shared samples across the five omics tables and map-group.txt", call. = FALSE)
}

missing_summary <- do.call(rbind, lapply(names(sample_sets), function(modality) {
  missing_samples <- setdiff(map$sample_id, sample_sets[[modality]])
  if (!length(missing_samples)) {
    return(data.frame(modality = character(0), missing_sample = character(0), group = character(0)))
  }
  data.frame(
    modality = modality,
    missing_sample = missing_samples,
    group = map$group[match(missing_samples, map$sample_id)],
    stringsAsFactors = FALSE
  )
}))

map_common <- map[match(common_samples, map$sample_id), , drop = FALSE]
map_common_out <- data.frame(`#SampleID` = map_common$sample_id, group = map_common$group, check.names = FALSE)
write_tsv(map_common_out, file.path(out_dir, "map-group_gp01_hc_af_common_145.txt"))

outputs <- list()
for (i in seq_len(nrow(modality_defs))) {
  def <- modality_defs[i, ]
  tab <- subset_to_samples(objects[[def$modality]], common_samples)
  out_name <- sprintf("model_%s_gp01_hc_af_%s_145.tsv", def$model_id, def$modality)
  outputs[[out_name]] <- tab
  write_tsv(tab, file.path(out_dir, out_name))
}

manifest <- do.call(rbind, feature_manifest)
write_tsv(manifest, file.path(out_dir, "feature_manifest.tsv"))
write_tsv(missing_summary, file.path(out_dir, "missing_samples_by_modality.tsv"))

summary_df <- data.frame(
  model_id = modality_defs$model_id,
  modality = modality_defs$modality,
  label_cn = modality_defs$label_cn,
  input_file = names(outputs),
  preset = modality_defs$preset,
  samples = vapply(outputs, function(x) ncol(x) - 1L, integer(1)),
  features = vapply(outputs, nrow, integer(1)),
  group_file = "map-group_gp01_hc_af_common_145.txt",
  groups = paste(names(table(map_common$group)), as.integer(table(map_common$group)), sep = "=", collapse = ";"),
  stringsAsFactors = FALSE
)
write_tsv(summary_df, file.path(out_dir, "input_summary.tsv"))

readme <- c(
  "# GP01.HC-AF v0.2.2 inputs",
  "",
  "本目录由 `scripts/prepare_gp01_hc_af_inputs.R` 生成。",
  "",
  "## 样本口径",
  "",
  "- 原始 `map-group.txt`: HC=48, AF=99, 合计 147。",
  "- 5 套组学共同样本: HC=46, AF=99, 合计 145。",
  "- `HC20` 缺少口腔真菌，`HC37` 缺少肠道真菌，因此正式 5 组学分析统一排除这 2 个样本。",
  "",
  "## 文件摘要",
  "",
  paste(capture.output(print(summary_df, row.names = FALSE)), collapse = "\n")
)
writeLines(readme, file.path(out_dir, "README.md"), useBytes = TRUE)

cat("Prepared GP01.HC-AF input tables in:", out_dir, "\n")
print(summary_df, row.names = FALSE)
