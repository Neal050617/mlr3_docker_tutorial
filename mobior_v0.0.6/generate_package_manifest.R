#!/usr/bin/env Rscript

root <- normalizePath(file.path(getwd(), ".."), mustWork = TRUE)
out_dir <- file.path(root, "R_packages_mobio")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_description <- function(path, source_name) {
  desc_files <- list.files(path, pattern = "^DESCRIPTION$", recursive = TRUE, full.names = TRUE)
  desc_files <- desc_files[dirname(desc_files) != path]
  rows <- lapply(desc_files, function(desc) {
    d <- tryCatch(read.dcf(desc), error = function(e) NULL)
    if (is.null(d) || !"Package" %in% colnames(d)) return(NULL)
    data.frame(
      package = unname(d[1, "Package"]),
      load_name = unname(d[1, "Package"]),
      source_type = "existing_library",
      source = source_name,
      version = if ("Version" %in% colnames(d)) unname(d[1, "Version"]) else "",
      install_ref = "",
      profile = "library",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

existing_libraries <- list(
  R_package_mobio = file.path(root, "R_package_mobio", "site-library"),
  chaoliu_R_package_mobio = file.path(root, "chaoliu_R_package_mobio", "site-library"),
  lvyan_R_package_mobio = file.path(root, "lvyan_R_package_mobio", "library")
)

existing_rows <- do.call(rbind, Map(read_description, existing_libraries, names(existing_libraries)))

pkg_rows <- function(packages, source_type, source, profile = "core", install_ref = "") {
  data.frame(
    package = packages,
    load_name = packages,
    source_type = source_type,
    source = source,
    version = "",
    install_ref = install_ref,
    profile = profile,
    stringsAsFactors = FALSE
  )
}

cran_core <- c(
  "BiocManager", "remotes", "pacman",
  "optparse", "argparse", "dplyr", "forcats", "ggplot2", "stringr",
  "tibble", "tidyr", "writexl",
  "reshape2", "readr", "openxlsx", "readxl", "data.table", "mltools",
  "Matrix", "xfun", "XML", "R.utils",
  "pheatmap", "cowplot", "ggpubr", "patchwork", "showtext", "sysfonts",
  "RColorBrewer", "ragg", "textshaping", "ggrepel", "plotly", "ggfortify",
  "FactoMineR", "factoextra", "gghalves", "scatterplot3d", "gmodels",
  "VennDiagram", "ggvenn", "UpSetR", "rstatix", "ggstatsplot", "psych",
  "quarto", "kableExtra", "pander", "tinytex",
  "future", "future.apply", "progressr", "lgr",
  "mlr3", "mlr3verse", "mlr3learners", "mlr3tuning", "mlr3tuningspaces",
  "mlr3measures", "mlr3pipelines", "mlr3filters", "mlr3fselect", "paradox", "bbotk",
  "glmnet", "ranger", "e1071", "xgboost", "lightgbm", "randomForest",
  "nnet", "naivebayes", "kknn", "rpart", "RWeka", "rJava", "smotefamily",
  "Boruta", "pROC", "ROCR",
  "fastshap", "kernelshap", "shapviz", "treeshap",
  "afex", "crosstalk", "ggraph", "ggside", "kSamples", "rprojroot",
  "robustbase", "broom", "httr", "qs",
  "tidymodels"
)

cran_full <- c(
  "harmony", "clustree", "SoupX", "hdf5r",
  "pak", "devtools", "tidyverse", "roxygen2", "usethis"
)

bioc_core <- c(
  "ANCOMBC", "maaslin3", "phyloseq", "mia", "ALDEx2", "bluster",
  "DESeq2", "SingleCellExperiment", "Biostrings", "biomaRt", "rBLAST",
  "KEGGREST", "qvalue", "ComplexHeatmap", "ropls", "impute", "pcaMethods",
  "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore", "genefilter",
  "sva", "limma", "KEGGgraph", "siggenes", "BiocParallel", "MSnbase",
  "multtest", "RBGL", "edgeR", "fgsea", "clusterProfiler", "pathview",
  "org.Hs.eg.db", "org.Mm.eg.db", "tidybulk", "tidyomics", "plyinteractions",
  "tidySpatialExperiment", "miloR", "MMUPHin"
)

github <- data.frame(
  package = c(
    "mlr3cmprsk", "mlr3extralearners", "mlr3proba", "mimosa", "aplot",
    "MetaboAnalystR", "createKEGGdb", "SeuratWrappers", "tidyseurat",
    "DoubletFinder", "GseaVis"
  ),
  load_name = c(
    "mlr3cmprsk", "mlr3extralearners", "mlr3proba", "mimosa", "aplot",
    "MetaboAnalystR", "createKEGGdb", "SeuratWrappers", "tidyseurat",
    "DoubletFinder", "GseaVis"
  ),
  source_type = "github",
  source = "manual_manifest",
  version = "",
  install_ref = c(
    "mlr-org/mlr3cmprsk",
    "https://github.com/mlr-org/mlr3extralearners/archive/refs/tags/v1.5.2.tar.gz",
    "mlr-org/mlr3proba",
    "borenstein-lab/mimosa2",
    "YuLab-SMU/aplot",
    "xia-lab/MetaboAnalystR",
    "YuLab-SMU/createKEGGdb",
    "satijalab/seurat-wrappers",
    "stemangiola/tidyseurat",
    "chris-mcginnis-ucsf/DoubletFinder",
    "junjunlab/GseaVis"
  ),
  profile = c("core", "core", "core", rep("full", 8)),
  stringsAsFactors = FALSE
)

local_or_generated <- data.frame(
  package = "KEGG.db",
  load_name = "KEGG.db",
  source_type = "local_or_generated",
  source = "metabo_R_packagess.md",
  version = "",
  install_ref = "createKEGGdb output or local KEGG.db_1.0.tar.gz",
  profile = "full",
  stringsAsFactors = FALSE
)

manual_rows <- rbind(
  pkg_rows(cran_core, "cran", "manual_manifest", "core"),
  pkg_rows(cran_full, "cran", "manual_manifest", "full"),
  pkg_rows(bioc_core, "bioconductor", "manual_manifest", "core"),
  github,
  local_or_generated
)

manifest <- rbind(existing_rows, manual_rows)
manifest <- manifest[manifest$package != "" & !is.na(manifest$package), ]
manifest <- manifest[!grepl("^@", manifest$package), ]

collapse_unique <- function(x) paste(sort(unique(x[nzchar(x) & !is.na(x)])), collapse = ";")
manifest <- do.call(
  rbind,
  lapply(split(manifest, manifest$package), function(rows) {
    data.frame(
      package = rows$package[[1]],
      load_name = collapse_unique(rows$load_name),
      source_type = collapse_unique(rows$source_type),
      source = collapse_unique(rows$source),
      version = collapse_unique(rows$version),
      install_ref = collapse_unique(rows$install_ref),
      profile = collapse_unique(rows$profile),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
)
manifest$load_name <- ifelse(nzchar(manifest$load_name), manifest$load_name, manifest$package)

manifest <- manifest[order(tolower(manifest$package)), ]
write.table(
  manifest,
  file.path(out_dir, "package_manifest.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = ""
)
write.table(
  manifest,
  file.path(root, "mobior_v0.0.6", "package_manifest.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = ""
)

script_files <- c(
  list.files(file.path(root, "mobior_v0.0.6", "R_scripts"), pattern = "\\.[Rr]$", full.names = TRUE),
  list.files(file.path(root, "scripts"), pattern = "\\.[Rr]$", full.names = TRUE),
  list.files(file.path(root, "lina_20260515", "scripts"), pattern = "\\.[Rr]$", full.names = TRUE),
  list.files(file.path(root, "lina_20251208", "20260403_lilei", "scripts"), pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
)

extract_deps <- function(path) {
  txt <- readLines(path, warn = FALSE)
  lib <- regmatches(txt, gregexpr("(library|require)\\s*\\(\\s*['\"]?[A-Za-z0-9_.]+['\"]?", txt, perl = TRUE))
  lib <- unlist(lib)
  lib <- sub("^(library|require)\\s*\\(\\s*['\"]?", "", lib)
  lib <- sub("['\"]$", "", lib)
  ns <- regmatches(txt, gregexpr("[A-Za-z0-9_.]+::", txt, perl = TRUE))
  ns <- sub("::$", "", unlist(ns))
  req <- character()
  req_start <- grep("required_pkgs\\s*<-\\s*c\\(", txt)
  if (length(req_start)) {
    block <- character()
    depth <- 0
    count_matches <- function(pattern, x) {
      m <- gregexpr(pattern, x, perl = TRUE)[[1]]
      if (identical(m, -1L)) 0L else length(m)
    }
    for (line in txt[seq(req_start[[1]], length(txt))]) {
      block <- c(block, line)
      depth <- depth + count_matches("\\(", line) - count_matches("\\)", line)
      if (depth <= 0) break
    }
    req <- regmatches(paste(block, collapse = "\n"), gregexpr("\"[A-Za-z0-9_.]+\"|'[A-Za-z0-9_.]+'", paste(block, collapse = "\n"), perl = TRUE))[[1]]
    req <- gsub("^[\"']|[\"']$", "", req)
  }
  pkgs <- sort(unique(c(lib, ns, req)))
  pkgs <- pkgs[!pkgs %in% c("", "TRUE", "FALSE", "pkg")]
  if (!length(pkgs)) return(NULL)
  data.frame(
    script = sub(paste0("^", root, "/?"), "", normalizePath(path)),
    load_name = pkgs,
    stringsAsFactors = FALSE
  )
}

script_deps <- do.call(rbind, lapply(script_files, extract_deps))
if (is.null(script_deps)) {
  script_deps <- data.frame(script = character(), load_name = character())
}

script_core <- setdiff(
  unique(script_deps$load_name),
  c(
    "devtools", "mimosa", "mimosa2", "roxygen2", "tidyverse", "usethis",
    "base", "datasets", "grDevices", "graphics", "methods", "stats", "utils"
  )
)
always_core <- c("BiocManager", "remotes", "mlr3cmprsk", "mlr3extralearners", "mlr3proba")
profile_to_full <- function(profile) {
  parts <- unique(unlist(strsplit(profile, ";", fixed = TRUE)))
  parts <- parts[nzchar(parts) & parts != "core"]
  parts <- unique(c("full", parts))
  paste(parts, collapse = ";")
}
profile_to_core <- function(profile) {
  parts <- unique(unlist(strsplit(profile, ";", fixed = TRUE)))
  parts <- parts[nzchar(parts)]
  parts <- unique(c("core", parts))
  paste(parts, collapse = ";")
}
core_from_script <- vapply(seq_len(nrow(manifest)), function(i) {
  loads <- unique(unlist(strsplit(manifest$load_name[[i]], ";", fixed = TRUE)))
  manifest$package[[i]] %in% always_core || any(loads %in% script_core)
}, logical(1))
core_rows <- grepl("(^|;)core(;|$)", manifest$profile)
manifest$profile[!core_rows & core_from_script] <- vapply(
  manifest$profile[!core_rows & core_from_script],
  profile_to_core,
  character(1)
)
core_rows <- grepl("(^|;)core(;|$)", manifest$profile)
manifest$profile[core_rows & !core_from_script] <- vapply(
  manifest$profile[core_rows & !core_from_script],
  profile_to_full,
  character(1)
)
write.table(
  manifest,
  file.path(out_dir, "package_manifest.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = ""
)
write.table(
  manifest,
  file.path(root, "mobior_v0.0.6", "package_manifest.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = ""
)
write.table(
  script_deps,
  file.path(out_dir, "script_dependency_manifest.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = ""
)

cat("package_manifest.tsv: ", nrow(manifest), " packages\n", sep = "")
cat("script_dependency_manifest.tsv: ", nrow(script_deps), " script-package rows\n", sep = "")
