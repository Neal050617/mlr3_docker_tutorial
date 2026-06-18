#!/usr/bin/env Rscript

options(timeout = as.integer(Sys.getenv("R_TIMEOUT", "10000")))
options(repos = c(CRAN = Sys.getenv("CRAN_REPO", "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
options(BioC_mirror = Sys.getenv("BIOC_MIRROR", "https://mirrors.tuna.tsinghua.edu.cn/bioconductor"))

manifest_path <- Sys.getenv("MOBIOR_PACKAGE_MANIFEST", "package_manifest.tsv")
check_path <- Sys.getenv("MOBIOR_FULL_CHECK", "../R_packages_mobio/package_check_full_before_install_v0.0.6.tsv")
out_path <- Sys.getenv("MOBIOR_FULL_AVAILABILITY", "../R_packages_mobio/full_missing_availability_v0.0.6.tsv")

manifest <- read.delim(manifest_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
check <- read.delim(check_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
manifest[is.na(manifest)] <- ""
check[is.na(check)] <- ""

missing_loads <- unique(check$load_name[check$status != "OK"])
split_values <- function(x) unique(unlist(strsplit(x, ";", fixed = TRUE)))

cran_available <- rownames(available.packages(contrib.url(getOption("repos")[["CRAN"]])))
bioc_repos <- BiocManager::repositories()
bioc_repos <- bioc_repos[grepl("bioconductor", bioc_repos, ignore.case = TRUE)]
bioc_available <- unique(unlist(lapply(bioc_repos, function(repo) {
  tryCatch(rownames(available.packages(contrib.url(repo))), error = function(e) character())
})))

rows <- do.call(rbind, lapply(seq_len(nrow(manifest)), function(i) {
  loads <- split_values(manifest$load_name[[i]])
  loads <- loads[nzchar(loads) & loads %in% missing_loads]
  if (!length(loads)) return(NULL)
  do.call(rbind, lapply(loads, function(load_name) {
    availability <- if (load_name %in% cran_available) {
      "cran"
    } else if (load_name %in% bioc_available) {
      "bioconductor"
    } else if (grepl("github", manifest$source_type[[i]], fixed = TRUE)) {
      "github_manifest"
    } else if (grepl("local_or_generated", manifest$source_type[[i]], fixed = TRUE)) {
      "local_or_generated"
    } else {
      "unknown"
    }
    data.frame(
      package = manifest$package[[i]],
      load_name = load_name,
      source_type = manifest$source_type[[i]],
      install_ref = manifest$install_ref[[i]],
      profile = manifest$profile[[i]],
      availability = availability,
      stringsAsFactors = FALSE
    )
  }))
}))

write.table(rows, out_path, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
cat("Full missing package availability\n")
print(table(rows$availability))
cat("Report: ", normalizePath(out_path, mustWork = FALSE), "\n", sep = "")
