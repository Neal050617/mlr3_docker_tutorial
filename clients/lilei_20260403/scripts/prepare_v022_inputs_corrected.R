#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
})

base_dir <- "20260403_lilei"
out_dir <- file.path(base_dir, "v0.2.2_inputs_corrected")
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

coerce_numeric_matrix <- function(df) {
  out <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))), check.names = FALSE)
  rownames(out) <- rownames(df)
  out
}

subset_matrix_by_samples <- function(df, sample_ids) {
  missing_ids <- setdiff(sample_ids, colnames(df))
  if (length(missing_ids) > 0) {
    stop("Missing sample IDs: ", paste(missing_ids, collapse = ", "), call. = FALSE)
  }
  df[, sample_ids, drop = FALSE]
}

combine_feature_tables <- function(...) {
  tabs <- list(...)
  shared_samples <- Reduce(intersect, lapply(tabs, function(x) names(x)[-1]))
  tabs <- lapply(tabs, function(x) x[, c("Features", shared_samples), drop = FALSE])
  out <- do.call(rbind, tabs)
  rownames(out) <- NULL
  out
}

read_genus_table <- function(path) {
  df <- read.delim(path, check.names = FALSE)
  feature_col <- names(df)[1]
  sample_df <- df[, -1, drop = FALSE]
  rownames(sample_df) <- df[[feature_col]]
  sample_df
}

read_sheet1_mapping <- function(path) {
  raw <- read_excel(path, sheet = "Sheet1")
  data.frame(
    sample_name = as.character(raw$`样本名称`),
    remark = as.character(raw$备注),
    feces = as.character(raw$`粪便菌群分析编号`),
    serum = as.character(raw$`血清代谢分析编号`),
    sheet_group = as.character(raw$分组),
    stringsAsFactors = FALSE
  )
}

read_response_mapping <- function(path) {
  raw <- read_excel(path, sheet = "预测模型建模样本及对应临床指标", col_names = FALSE)
  dat <- as.data.frame(raw[-c(1, 2), 1:23], stringsAsFactors = FALSE)
  colnames(dat) <- c(
    "sample_name", "remark",
    "feces_pre", "serum_pre", "group_pre",
    "feces_post", "serum_post", "group_post",
    "Sex", "Age", "BMI", "Smoking", "Drinking", "ECOG",
    "T_category", "N_category", "M_category",
    "NLR", "PLR", "LMR", "SII", "SIRI", "PIV"
  )
  dat
}

read_metabolite_data <- function(path) {
  met <- read_excel(path)
  meta_end <- match("posneg", names(met))
  if (is.na(meta_end)) {
    stop("Could not find 'posneg' column in metabolite workbook", call. = FALSE)
  }
  feature_meta <- as.data.frame(met[, seq_len(meta_end), drop = FALSE], stringsAsFactors = FALSE)
  sample_df <- as.data.frame(met[, (meta_end + 1):ncol(met), drop = FALSE], check.names = FALSE)
  feature_keys <- prefix_features(feature_meta$ID, "Metabolite_")
  rownames(sample_df) <- feature_keys
  feature_map <- data.frame(
    feature_id = feature_keys,
    raw_id = feature_meta$ID,
    raw_name = feature_meta$name,
    mz = feature_meta$mz,
    rt = feature_meta$rt,
    class = feature_meta$class,
    sub_class = feature_meta$sub_class,
    stringsAsFactors = FALSE
  )
  list(feature_map = feature_map, sample_df = sample_df)
}

build_microbiome_table <- function(genus_df, sample_ids, feature_prefix = "Genus_") {
  mat <- subset_matrix_by_samples(genus_df, sample_ids)
  feature_names <- prefix_features(rownames(mat), feature_prefix)
  feature_table(feature_names, mat)
}

build_metabolome_table <- function(met_sample_df, serum_ids, output_ids, prefix = NULL) {
  mat <- coerce_numeric_matrix(subset_matrix_by_samples(met_sample_df, serum_ids))
  colnames(mat) <- output_ids
  if (!is.null(prefix)) {
    rownames(mat) <- prefix_features(rownames(mat), prefix)
  }
  feature_table(rownames(mat), mat)
}

build_clinical_table <- function(pair_df, output_ids) {
  clinical_cols <- c("Sex", "Age", "BMI", "Smoking", "Drinking", "ECOG",
                     "T_category", "N_category", "M_category",
                     "NLR", "PLR", "LMR", "SII", "SIRI", "PIV")
  row_idx <- match(output_ids, pair_df$feces_pre)
  if (any(is.na(row_idx))) {
    stop("Clinical mapping missing samples: ", paste(output_ids[is.na(row_idx)], collapse = ", "), call. = FALSE)
  }
  mat <- t(as.matrix(pair_df[row_idx, clinical_cols, drop = FALSE]))
  colnames(mat) <- output_ids
  rownames(mat) <- prefix_features(clinical_cols, "Clinical_")
  feature_table(rownames(mat), mat)
}

build_dynamic_microbiome_table <- function(pre_df, post_df, pair_df) {
  shared_features <- intersect(rownames(pre_df), rownames(post_df))
  pre_mat <- coerce_numeric_matrix(subset_matrix_by_samples(pre_df[shared_features, , drop = FALSE], pair_df$feces_pre))
  post_mat <- coerce_numeric_matrix(subset_matrix_by_samples(post_df[shared_features, , drop = FALSE], pair_df$feces_post))
  delta_mat <- as.matrix(post_mat) - as.matrix(pre_mat)
  colnames(delta_mat) <- pair_df$feces_pre
  rownames(delta_mat) <- prefix_features(shared_features, "GenusDelta_")
  feature_table(rownames(delta_mat), delta_mat)
}

build_dynamic_metabolome_table <- function(met_sample_df, pair_df) {
  pre_mat <- coerce_numeric_matrix(subset_matrix_by_samples(met_sample_df, pair_df$serum_pre))
  post_mat <- coerce_numeric_matrix(subset_matrix_by_samples(met_sample_df, pair_df$serum_post))
  delta_mat <- as.matrix(post_mat) - as.matrix(pre_mat)
  colnames(delta_mat) <- pair_df$feces_pre
  rownames(delta_mat) <- prefix_features(rownames(pre_mat), "MetaboliteDelta_")
  feature_table(rownames(delta_mat), delta_mat)
}

mapping_path <- file.path(base_dir, "李磊-食管癌项目-多组学样本及临床信息表20251223.xlsx")
met_path <- list.files(base_dir, pattern = "代谢物.*xlsx$", full.names = TRUE)[1]

pre_genus <- read_genus_table(file.path(base_dir, "2GP.1.NP_NRT-P_NRT", "genus.xls"))
post_genus <- read_genus_table(file.path(base_dir, "2GP.2.NP_RT-P_RT", "genus.xls"))
con_genus <- read_genus_table(file.path(base_dir, "1Gp01.Con-LAEC", "genus.xls"))
con_map <- read.delim(file.path(base_dir, "1Gp01.Con-LAEC", "map-group.txt"), check.names = FALSE)
colnames(con_map)[1:2] <- c("feces", "group")

sheet1_df <- read_sheet1_mapping(mapping_path)
response_df <- read_response_mapping(mapping_path)
met_obj <- read_metabolite_data(met_path)
met_cols <- colnames(met_obj$sample_df)

con_pair <- merge(sheet1_df, con_map, by = "feces", all = FALSE)
con_pair <- con_pair[
  con_pair$feces %in% colnames(con_genus) &
    con_pair$serum %in% met_cols &
    con_pair$group %in% c("Con", "LAEC"),
  ,
  drop = FALSE
]
con_pair <- con_pair[order(match(con_pair$feces, colnames(con_genus))), , drop = FALSE]
con_pair <- con_pair[!duplicated(con_pair$feces), , drop = FALSE]

response_pair <- response_df[
  response_df$feces_pre %in% colnames(pre_genus) &
    response_df$serum_pre %in% met_cols &
    response_df$feces_post %in% colnames(post_genus) &
    response_df$serum_post %in% met_cols &
    response_df$group_pre %in% c("NP_NRT", "P_NRT"),
  ,
  drop = FALSE
]
response_pair <- response_pair[order(match(response_pair$feces_pre, colnames(pre_genus))), , drop = FALSE]
response_pair <- response_pair[!duplicated(response_pair$feces_pre), , drop = FALSE]

stopifnot(nrow(con_pair) == 246)
stopifnot(identical(as.integer(table(con_pair$group)[c("Con", "LAEC")]), c(118L, 128L)))
stopifnot(nrow(response_pair) == 77)
stopifnot(identical(as.integer(table(response_pair$group_pre)[c("NP_NRT", "P_NRT")]), c(63L, 14L)))

write_tsv(con_pair[, c("feces", "serum", "group", "sheet_group", "sample_name", "remark")],
          file.path(out_dir, "cohort_con_laec_246_mapping.tsv"))
write_tsv(response_pair,
          file.path(out_dir, "cohort_nrt_response_77_mapping.tsv"))
write_tsv(met_obj$feature_map, file.path(out_dir, "metabolite_feature_map.tsv"))

map_con_laec <- data.frame(`#SampleID` = con_pair$feces, group = con_pair$group, check.names = FALSE)
map_nrt_response <- data.frame(`#SampleID` = response_pair$feces_pre, group = response_pair$group_pre, check.names = FALSE)
write_tsv(map_con_laec, file.path(out_dir, "map-group_con_laec_246.txt"))
write_tsv(map_nrt_response, file.path(out_dir, "map-group_nrt_response_77.txt"))

micro_con_246 <- build_microbiome_table(con_genus, con_pair$feces, "Genus_")
met_con_246 <- build_metabolome_table(met_obj$sample_df, con_pair$serum, con_pair$feces)
multi_con_246 <- combine_feature_tables(micro_con_246, met_con_246)

clinical_77 <- build_clinical_table(response_pair, response_pair$feces_pre)
micro_pre_77 <- build_microbiome_table(pre_genus, response_pair$feces_pre, "Genus_")
met_pre_77 <- build_metabolome_table(met_obj$sample_df, response_pair$serum_pre, response_pair$feces_pre)
micro_delta_77 <- build_dynamic_microbiome_table(pre_genus, post_genus, response_pair)
met_delta_77 <- build_dynamic_metabolome_table(met_obj$sample_df, response_pair)

clin_micro_77 <- combine_feature_tables(clinical_77, micro_pre_77)
clin_met_77 <- combine_feature_tables(clinical_77, met_pre_77)
clin_multi_77 <- combine_feature_tables(clinical_77, micro_pre_77, met_pre_77)
clin_micro_delta_77 <- combine_feature_tables(clinical_77, micro_delta_77)
clin_met_delta_77 <- combine_feature_tables(clinical_77, met_delta_77)
clin_multi_delta_77 <- combine_feature_tables(clinical_77, micro_delta_77, met_delta_77)

outputs <- list(
  "model_0.1_con_laec_microbiome_246.tsv" = micro_con_246,
  "model_0.2_con_laec_metabolome_246.tsv" = met_con_246,
  "model_0.3_con_laec_multiomics_246.tsv" = multi_con_246,
  "model_1.0_clinical_77.tsv" = clinical_77,
  "model_1.1_clinical_microbiome_77.tsv" = clin_micro_77,
  "model_1.2_clinical_metabolome_77.tsv" = clin_met_77,
  "model_1.3_clinical_multiomics_77.tsv" = clin_multi_77,
  "model_1.4_clinical_microbiome_delta_77.tsv" = clin_micro_delta_77,
  "model_1.5_clinical_metabolome_delta_77.tsv" = clin_met_delta_77,
  "model_1.6_clinical_multiomics_delta_77.tsv" = clin_multi_delta_77
)

for (nm in names(outputs)) {
  write_tsv(outputs[[nm]], file.path(out_dir, nm))
}

summary_df <- data.frame(
  file = names(outputs),
  samples = vapply(outputs, function(x) ncol(x) - 1L, integer(1)),
  features = vapply(outputs, nrow, integer(1)),
  group_file = c(rep("map-group_con_laec_246.txt", 3), rep("map-group_nrt_response_77.txt", 7)),
  groups = c(rep("Con=118;LAEC=128", 3), rep("NP_NRT=63;P_NRT=14", 7)),
  stringsAsFactors = FALSE
)
write_tsv(summary_df, file.path(out_dir, "input_summary.tsv"))

readme <- c(
  "# v0.2.2 corrected inputs",
  "",
  "本目录按 2026-05-15 修正版建模计划生成。",
  "",
  "- 所有样本均来自映射表中粪便菌群和血清代谢均存在的交集。",
  "- 所有矩阵列名均使用粪便菌群样本名。",
  "- 0.1-0.3 使用 Con vs LAEC，n=246。",
  "- 1.0-1.6 使用 NP_NRT vs P_NRT，n=77。",
  "- 1.4-1.6 的组学特征为 RT - NRT 配对差值。",
  "",
  "## 文件摘要",
  "",
  paste(capture.output(print(summary_df, row.names = FALSE)), collapse = "\n")
)
writeLines(readme, file.path(out_dir, "README.md"), useBytes = TRUE)

cat("Prepared corrected input tables in:", out_dir, "\n")
print(summary_df, row.names = FALSE)
