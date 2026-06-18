#!/usr/bin/env Rscript

options(timeout = as.integer(Sys.getenv("R_TIMEOUT", "10000")))
options(repos = c(CRAN = Sys.getenv("CRAN_REPO", "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
options(BioC_mirror = Sys.getenv("BIOC_MIRROR", "https://mirrors.tuna.tsinghua.edu.cn/bioconductor"))

site_lib <- Sys.getenv("R_SITE_LIBRARY", "/usr/local/lib/R/site-library")
profile <- Sys.getenv("MOBIOR_PROFILE", "core")
manifest_path <- Sys.getenv("MOBIOR_PACKAGE_MANIFEST", "package_manifest.tsv")

dir.create(site_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(site_lib, .libPaths())))
Sys.setenv(
  R_LIBS = site_lib,
  R_LIBS_USER = site_lib,
  R_LIBS_SITE = paste(.libPaths(), collapse = .Platform$path.sep),
  R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true"
)

cat("MoBior v0.0.6 package installer\n")
cat("R: ", R.version.string, "\n", sep = "")
cat("Profile: ", profile, "\n", sep = "")
cat("Library: ", site_lib, "\n", sep = "")
cat("Manifest: ", manifest_path, "\n\n", sep = "")

if (!file.exists(manifest_path)) {
  stop("Package manifest not found: ", manifest_path)
}

is_r46 <- grepl("^R version 4\\.6\\.", R.version.string)
if (!is_r46 && identical(Sys.getenv("ALLOW_NON_R46"), "")) {
  stop("R 4.6.x is required. Set ALLOW_NON_R46=1 only for dry-run testing.")
}

manifest <- read.delim(manifest_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
manifest[is.na(manifest)] <- ""

include_profile <- function(x) {
  parts <- unique(unlist(strsplit(x, ";", fixed = TRUE)))
  include_library <- profile == "full" || tolower(Sys.getenv("MOBIOR_INCLUDE_LIBRARY", "")) %in% c("1", "true", "yes", "y")
  "core" %in% parts || (profile == "full" && "full" %in% parts) || (include_library && "library" %in% parts)
}
manifest <- manifest[vapply(manifest$profile, include_profile, logical(1)), ]

split_values <- function(x) unique(unlist(strsplit(x, ";", fixed = TRUE)))
has_type <- function(x, type) type %in% split_values(x)

missing_loads <- function(load_names) {
  load_names <- unique(load_names[nzchar(load_names)])
  load_names[!vapply(load_names, requireNamespace, logical(1), quietly = TRUE)]
}

install_remote_ref <- function(ref) {
  if (grepl("^https?://", ref)) {
    remotes::install_url(ref, dependencies = c("Depends", "Imports", "LinkingTo"), upgrade = "never")
    return(invisible(TRUE))
  }
  remotes::install_github(ref, dependencies = c("Depends", "Imports", "LinkingTo"), upgrade = "never")
  invisible(TRUE)
}

install_vendor_package <- function(package) {
  vendor_dir <- file.path(Sys.getenv("MOBIOR_VENDOR_PACKAGES", "/opt/mobior/vendor_packages"), package)
  if (!dir.exists(vendor_dir)) return(FALSE)
  target_dir <- file.path(site_lib, package)
  unlink(target_dir, recursive = TRUE, force = TRUE)
  ok <- file.copy(vendor_dir, site_lib, recursive = TRUE, copy.date = TRUE)
  if (isTRUE(ok)) {
    cat("Installed vendor package fallback: ", package, "\n", sep = "")
    return(TRUE)
  }
  FALSE
}

install_cran <- function(pkgs) {
  pkgs <- missing_loads(pkgs)
  if (!length(pkgs)) return(invisible(character()))
  cat("Installing CRAN packages: ", paste(pkgs, collapse = ", "), "\n", sep = "")
  install.packages(pkgs, dependencies = c("Depends", "Imports", "LinkingTo"), Ncpus = max(1, min(4, parallel::detectCores())))
  missing_loads(pkgs)
}

install_cran_archives <- function(pkgs) {
  archive_versions <- c(fastshap = "0.1.1")
  pkgs <- intersect(missing_loads(pkgs), names(archive_versions))
  if (!length(pkgs)) return(invisible(character()))
  for (pkg in pkgs) {
    cat("Installing CRAN archive package: ", pkg, " ", archive_versions[[pkg]], "\n", sep = "")
    tryCatch(
      remotes::install_version(
        pkg,
        version = archive_versions[[pkg]],
        repos = getOption("repos")[["CRAN"]],
        dependencies = c("Depends", "Imports", "LinkingTo"),
        upgrade = "never"
      ),
      error = function(e) warning("CRAN archive install failed for ", pkg, ": ", conditionMessage(e))
    )
  }
  missing_loads(pkgs)
}

install_bioc <- function(pkgs) {
  pkgs <- missing_loads(pkgs)
  if (!length(pkgs)) return(invisible(character()))
  cat("Installing Bioconductor packages: ", paste(pkgs, collapse = ", "), "\n", sep = "")
  BiocManager::install(pkgs, update = FALSE, ask = FALSE, dependencies = c("Depends", "Imports", "LinkingTo"), Ncpus = max(1, min(4, parallel::detectCores())))
  missing_loads(pkgs)
}

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

cat("Configuring Bioconductor 3.23\n")
BiocManager::install(version = "3.23", update = FALSE, ask = FALSE)

cran_pkgs <- manifest$load_name[vapply(manifest$source_type, has_type, logical(1), type = "cran")]
bioc_pkgs <- manifest$load_name[vapply(manifest$source_type, has_type, logical(1), type = "bioconductor")]

failed <- character()
cran_failed <- install_cran(cran_pkgs)
archive_failed <- install_cran_archives(cran_failed)
failed <- c(failed, setdiff(cran_failed, names(c(fastshap = "0.1.1"))), archive_failed)
failed <- c(failed, install_bioc(bioc_pkgs))

known_source <- function(x) {
  any(c("cran", "bioconductor", "github", "local_or_generated") %in% split_values(x))
}
library_fallback <- manifest[
  vapply(manifest$source_type, has_type, logical(1), type = "existing_library") &
    !vapply(manifest$source_type, known_source, logical(1)),
]
fallback_loads <- unique(unlist(strsplit(library_fallback$load_name, ";", fixed = TRUE)))
fallback_loads <- missing_loads(fallback_loads)
if (length(fallback_loads)) {
  cat("Installing unclassified existing-library packages via CRAN fallback: ", paste(fallback_loads, collapse = ", "), "\n", sep = "")
  tryCatch(
    install.packages(fallback_loads, dependencies = c("Depends", "Imports", "LinkingTo"), Ncpus = max(1, min(4, parallel::detectCores()))),
    error = function(e) warning("CRAN fallback failed: ", conditionMessage(e))
  )
  fallback_loads <- missing_loads(fallback_loads)
}
if (length(fallback_loads)) {
  cat("Installing remaining unclassified packages via Bioconductor fallback: ", paste(fallback_loads, collapse = ", "), "\n", sep = "")
  tryCatch(
    BiocManager::install(fallback_loads, update = FALSE, ask = FALSE, dependencies = c("Depends", "Imports", "LinkingTo"), Ncpus = max(1, min(4, parallel::detectCores()))),
    error = function(e) warning("Bioconductor fallback failed: ", conditionMessage(e))
  )
  failed <- c(failed, missing_loads(fallback_loads))
}

github_rows <- manifest[vapply(manifest$source_type, has_type, logical(1), type = "github"), ]
if (nrow(github_rows)) {
  for (package in unique(github_rows$package)) {
    install_vendor_package(package)
  }
  for (i in seq_len(nrow(github_rows))) {
    load_names <- split_values(github_rows$load_name[[i]])
    refs <- split_values(github_rows$install_ref[[i]])
    refs <- refs[nzchar(refs)]
    if (!length(refs) || !length(missing_loads(load_names))) next
    ref <- refs[[1]]
    cat("Installing GitHub package: ", github_rows$package[[i]], " from ", ref, "\n", sep = "")
    tryCatch(
      install_remote_ref(ref),
      error = function(e) {
        warning("GitHub install failed for ", github_rows$package[[i]], ": ", conditionMessage(e))
        if (!grepl("^https?://", ref) && !grepl("@", ref, fixed = TRUE)) {
          url <- paste0("https://github.com/", ref, "/archive/HEAD.tar.gz")
          tryCatch(
            remotes::install_url(url, dependencies = c("Depends", "Imports", "LinkingTo"), upgrade = "never"),
            error = function(e2) warning("GitHub tarball install failed for ", github_rows$package[[i]], ": ", conditionMessage(e2))
          )
        }
      }
    )
    if (length(missing_loads(load_names))) {
      install_vendor_package(github_rows$package[[i]])
    }
    failed <- c(failed, missing_loads(load_names))
  }
}

local_rows <- manifest[vapply(manifest$source_type, has_type, logical(1), type = "local_or_generated"), ]
if (nrow(local_rows)) {
  for (i in seq_len(nrow(local_rows))) {
    load_names <- split_values(local_rows$load_name[[i]])
    miss <- missing_loads(load_names)
    if (length(miss)) {
      cat("Optional local/generated package not installed: ", local_rows$package[[i]], " (", local_rows$install_ref[[i]], ")\n", sep = "")
    }
  }
}

final_loads <- unique(unlist(strsplit(manifest$load_name, ";", fixed = TRUE)))
final_loads <- final_loads[nzchar(final_loads)]
final_missing <- missing_loads(final_loads)

cat("\nInstall summary\n")
cat("Checked packages: ", length(final_loads), "\n", sep = "")
cat("Missing after install: ", length(final_missing), "\n", sep = "")
if (length(final_missing)) {
  cat(paste0("  - ", final_missing, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}

cat("All selected packages are loadable.\n")
