#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(prefix, default = "") {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1]], fixed = TRUE) else default
}

site_lib <- get_arg("--site-lib=", Sys.getenv("R_SITE_LIBRARY", "/usr/local/lib/R/site-library"))
manifest_path <- get_arg("--manifest=", Sys.getenv("MOBIOR_PACKAGE_MANIFEST", "package_manifest.tsv"))
out_path <- get_arg("--out=", Sys.getenv("MOBIOR_CHECK_OUT", "package_check.tsv"))
profile <- get_arg("--profile=", Sys.getenv("MOBIOR_PROFILE", "core"))
strict <- tolower(get_arg("--strict=", Sys.getenv("STRICT", "true"))) %in% c("1", "true", "yes", "y")

if (nzchar(site_lib)) .libPaths(unique(c(site_lib, .libPaths())))
if (!file.exists(manifest_path)) stop("Package manifest not found: ", manifest_path)

manifest <- read.delim(manifest_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
manifest[is.na(manifest)] <- ""

include_profile <- function(x) {
  parts <- unique(unlist(strsplit(x, ";", fixed = TRUE)))
  include_library <- profile == "full" || tolower(Sys.getenv("MOBIOR_INCLUDE_LIBRARY", "")) %in% c("1", "true", "yes", "y")
  "core" %in% parts || (profile == "full" && "full" %in% parts) || (include_library && "library" %in% parts)
}
manifest <- manifest[vapply(manifest$profile, include_profile, logical(1)), ]

split_values <- function(x) unique(unlist(strsplit(x, ";", fixed = TRUE)))

rows <- do.call(rbind, lapply(seq_len(nrow(manifest)), function(i) {
  loads <- split_values(manifest$load_name[[i]])
  loads <- loads[nzchar(loads)]
  do.call(rbind, lapply(loads, function(load_name) {
    msg <- ""
    version <- ""
    lib <- ""
    status <- "OK"
    ok <- tryCatch({
      suppressPackageStartupMessages(loadNamespace(load_name))
      TRUE
    }, error = function(e) {
      msg <<- conditionMessage(e)
      FALSE
    })
    if (!ok) {
      status <- if (load_name %in% rownames(installed.packages())) "BROKEN" else "MISSING"
    } else {
      version <- as.character(packageVersion(load_name))
      lib <- tryCatch(dirname(system.file(package = load_name)), error = function(e) "")
    }
    data.frame(
      package = manifest$package[[i]],
      load_name = load_name,
      status = status,
      version = version,
      library_dir = lib,
      source_type = manifest$source_type[[i]],
      profile = manifest$profile[[i]],
      message = msg,
      stringsAsFactors = FALSE
    )
  }))
}))

write.table(rows, out_path, sep = "\t", quote = FALSE, row.names = FALSE, na = "")

cat("MoBior v0.0.6 package check\n")
cat("R: ", R.version.string, "\n", sep = "")
cat("Bioconductor: ", if (requireNamespace("BiocManager", quietly = TRUE)) as.character(BiocManager::version()) else "unavailable", "\n", sep = "")
cat("Profile: ", profile, "\n", sep = "")
cat("Report: ", normalizePath(out_path, mustWork = FALSE), "\n\n", sep = "")
print(table(rows$status))

bad <- rows[rows$status != "OK", ]
if (nrow(bad)) {
  cat("\nNon-OK packages:\n")
  apply(bad, 1, function(x) cat("  - ", x[["load_name"]], " [", x[["status"]], "]: ", x[["message"]], "\n", sep = ""))
}

if (strict && nrow(bad)) quit(status = 1)
quit(status = 0)
