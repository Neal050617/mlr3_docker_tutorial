#!/usr/bin/env Rscript

options(timeout = as.integer(Sys.getenv("R_TIMEOUT", "10000")))
options(repos = c(CRAN = Sys.getenv("CRAN_REPO", "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
options(BioC_mirror = Sys.getenv("BIOC_MIRROR", "https://mirrors.tuna.tsinghua.edu.cn/bioconductor"))

site_lib <- Sys.getenv("R_SITE_LIBRARY", "/opt/mobior-full/site-library")
availability_path <- Sys.getenv("MOBIOR_FULL_AVAILABILITY", "../R_packages_mobio/full_missing_availability_v0.0.6.tsv")
manifest_path <- Sys.getenv("MOBIOR_PACKAGE_MANIFEST", "package_manifest.tsv")
ncpus <- max(1, min(as.integer(Sys.getenv("NCPUS", "1")), parallel::detectCores()))
install_passes <- max(1, as.integer(Sys.getenv("FULL_INSTALL_PASSES", "3")))
dependency_which <- c("Depends", "Imports", "LinkingTo")

dir.create(site_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(site_lib, .libPaths())))
Sys.setenv(
  R_LIBS = site_lib,
  R_LIBS_USER = site_lib,
  R_LIBS_SITE = paste(.libPaths(), collapse = .Platform$path.sep)
)

home <- Sys.getenv("HOME", unset = "")
home_ok <- nzchar(home) && dir.exists(home) && file.access(home, 2) == 0
if (!home_ok) {
  home <- file.path(site_lib, ".home")
  dir.create(home, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(HOME = home)
}
xdg_config <- file.path(home, ".config")
dir.create(xdg_config, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(
  XDG_CONFIG_HOME = xdg_config,
  GIT_CONFIG_GLOBAL = file.path(home, ".gitconfig"),
  R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true"
)
cargo_bin <- file.path(home, ".cargo", "bin")
Sys.setenv(
  PATH = paste(cargo_bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
  CARGO_HOME = file.path(home, ".cargo"),
  RUSTUP_HOME = file.path(home, ".rustup")
)

cat("MoBior v0.0.6 full available package installer\n")
cat("R: ", R.version.string, "\n", sep = "")
cat("Library: ", site_lib, "\n", sep = "")
cat("HOME: ", Sys.getenv("HOME"), "\n", sep = "")
cat("Availability: ", availability_path, "\n", sep = "")
cat("Ncpus: ", ncpus, "\n\n", sep = "")
cat("Install passes: ", install_passes, "\n\n", sep = "")

if (!file.exists(availability_path)) stop("Availability report not found: ", availability_path)
if (!file.exists(manifest_path)) stop("Manifest not found: ", manifest_path)

availability <- read.delim(availability_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
manifest <- read.delim(manifest_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
availability[is.na(availability)] <- ""
manifest[is.na(manifest)] <- ""

missing_loads <- function(pkgs) {
  pkgs <- unique(pkgs[nzchar(pkgs)])
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}

install_cran <- function(pkgs) {
  pkgs <- missing_loads(pkgs)
  if (!length(pkgs)) return(character())
  cat("Installing available CRAN packages: ", length(pkgs), "\n", sep = "")
  tryCatch(
    install.packages(pkgs, dependencies = c("Depends", "Imports", "LinkingTo"), Ncpus = ncpus),
    error = function(e) warning("CRAN install phase failed: ", conditionMessage(e))
  )
  missing_loads(pkgs)
}

install_cran_bootstrap <- function(pkgs) {
  pkgs <- missing_loads(pkgs)
  if (!length(pkgs)) return(character())
  cat("Installing CRAN bootstrap packages: ", length(pkgs), "\n", sep = "")
  chunks <- split(pkgs, ceiling(seq_along(pkgs) / 10))
  for (chunk in chunks) {
    tryCatch(
      install.packages(chunk, dependencies = dependency_which, Ncpus = ncpus),
      error = function(e) warning("CRAN bootstrap install failed: ", conditionMessage(e))
    )
  }
  missing_loads(pkgs)
}

repo_db <- local({
  db <- NULL
  function(refresh = FALSE) {
    if (is.null(db) || refresh) {
      repos <- BiocManager::repositories()
      db <<- available.packages(repos = repos)
    }
    db
  }
})

package_closure <- function(pkgs, db) {
  pkgs <- unique(pkgs[nzchar(pkgs)])
  pkgs <- intersect(pkgs, rownames(db))
  if (!length(pkgs)) return(character())
  deps <- tools::package_dependencies(pkgs, db = db, which = dependency_which, recursive = TRUE)
  unique(c(pkgs, unlist(deps, use.names = FALSE)))
}

install_chunks <- function(pkgs, label, dependencies = FALSE) {
  pkgs <- unique(pkgs[nzchar(pkgs)])
  if (!length(pkgs)) return(invisible())
  chunks <- split(pkgs, ceiling(seq_along(pkgs) / 25))
  for (chunk in chunks) {
    cat("Installing ", label, " chunk: ", length(chunk), "\n", sep = "")
    tryCatch(
      BiocManager::install(
        chunk,
        update = FALSE,
        ask = FALSE,
        dependencies = dependencies,
        Ncpus = ncpus
      ),
      error = function(e) warning(label, " install chunk failed: ", conditionMessage(e))
    )
  }
}

install_repo_closure <- function(pkgs, label) {
  pkgs <- unique(pkgs[nzchar(pkgs)])
  db <- repo_db()
  available_pkgs <- intersect(pkgs, rownames(db))
  unavailable <- setdiff(pkgs, rownames(db))
  if (length(unavailable)) {
    warning(label, " packages not found in configured repositories: ", paste(unavailable, collapse = ", "))
  }
  closure <- package_closure(available_pkgs, db)
  closure <- setdiff(closure, c("R"))
  closure <- unique(c(closure, available_pkgs))
  closure <- intersect(closure, rownames(db))
  missing <- missing_loads(closure)
  if (!length(missing)) return(missing_loads(available_pkgs))

  cat("Installing ", label, " recursive dependency closure: ", length(missing), "\n", sep = "")
  for (round in seq_len(80)) {
    missing <- missing_loads(closure)
    if (!length(missing)) break
    deps <- tools::package_dependencies(missing, db = db, which = dependency_which, recursive = FALSE)
    ready <- missing[vapply(missing, function(pkg) {
      required <- intersect(deps[[pkg]], closure)
      !any(required %in% missing)
    }, logical(1))]
    if (!length(ready)) ready <- missing
    before <- missing
    install_chunks(ready, paste0(label, " dependency"), dependencies = FALSE)
    after <- missing_loads(closure)
    cat("Remaining ", label, " dependency closure after round ", round, ": ", length(after), "\n", sep = "")
    if (!length(after) || length(after) < length(before)) next
    install_chunks(after, paste0(label, " fallback"), dependencies = dependency_which)
    break
  }

  install_chunks(missing_loads(available_pkgs), label, dependencies = FALSE)
  missing_loads(available_pkgs)
}

install_bioc <- function(pkgs) {
  pkgs <- missing_loads(pkgs)
  if (!length(pkgs)) return(character())
  cat("Installing available Bioconductor packages: ", length(pkgs), "\n", sep = "")
  tryCatch(
    BiocManager::install(pkgs, update = FALSE, ask = FALSE, dependencies = c("Depends", "Imports", "LinkingTo"), Ncpus = ncpus),
    error = function(e) warning("Bioconductor install phase failed: ", conditionMessage(e))
  )
  missing_loads(pkgs)
}

install_archives <- function(pkgs) {
  archive_versions <- c(gghalves = "0.1.4", plogr = "0.2.0")
  pkgs <- intersect(missing_loads(pkgs), names(archive_versions))
  if (!length(pkgs)) return(character())
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

install_github_rows <- function(rows) {
  if (!nrow(rows)) return(character())
  failed <- character()
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
  refs <- unique(rows$install_ref[nzchar(rows$install_ref)])
  for (package in unique(rows$package)) {
    install_vendor_package(package)
  }
  for (ref in refs) {
    loads <- unique(rows$load_name[rows$install_ref == ref])
    loads <- loads[nzchar(loads)]
    if (!length(missing_loads(loads))) next
    cat("Installing GitHub ref: ", ref, "\n", sep = "")
    install_one <- function() {
      if (grepl("^https?://", ref)) {
        remotes::install_url(ref, dependencies = c("Depends", "Imports", "LinkingTo"), upgrade = "never")
      } else if (identical(ref, "xia-lab/MetaboAnalystR")) {
        remotes::install_url(
          "https://github.com/xia-lab/MetaboAnalystR/archive/HEAD.tar.gz",
          dependencies = c("Depends", "Imports", "LinkingTo"),
          upgrade = "never"
        )
      } else {
        remotes::install_github(ref, dependencies = c("Depends", "Imports", "LinkingTo"), upgrade = "never")
      }
    }
    tryCatch(
      install_one(),
      error = function(e) {
        warning("GitHub install failed for ", ref, ": ", conditionMessage(e))
        if (!grepl("^https?://", ref) && !grepl("@", ref, fixed = TRUE)) {
          url <- paste0("https://github.com/", ref, "/archive/HEAD.tar.gz")
          tryCatch(
            remotes::install_url(url, dependencies = c("Depends", "Imports", "LinkingTo"), upgrade = "never"),
            error = function(e2) warning("GitHub tarball install failed for ", ref, ": ", conditionMessage(e2))
          )
        }
      }
    )
    for (package in unique(rows$package[rows$install_ref == ref])) {
      if (length(missing_loads(loads))) install_vendor_package(package)
    }
    failed <- c(failed, missing_loads(loads))
  }
  unique(failed)
}

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

ensure_modern_rust <- function() {
  min_version <- Sys.getenv("MOBIOR_MIN_RUST", "1.85.0")
  current <- suppressWarnings(system2("rustc", "--version", stdout = TRUE, stderr = TRUE))
  current_version <- sub("^rustc ([0-9.]+).*$", "\\1", current[1])
  has_modern <- length(current_version) && !is.na(utils::compareVersion(current_version, min_version)) &&
    utils::compareVersion(current_version, min_version) >= 0
  if (isTRUE(has_modern)) {
    cat("Rust toolchain OK: ", current[1], "\n", sep = "")
    return(invisible(TRUE))
  }
  if (!nzchar(Sys.which("curl"))) {
    warning("curl is unavailable; cannot bootstrap user-level rustup")
    return(invisible(FALSE))
  }
  cat("Installing user-level Rust toolchain with rustup; required >= ", min_version, "\n", sep = "")
  dir.create(Sys.getenv("CARGO_HOME"), recursive = TRUE, showWarnings = FALSE)
  dir.create(Sys.getenv("RUSTUP_HOME"), recursive = TRUE, showWarnings = FALSE)
  installer <- file.path(tempdir(), "rustup-init.sh")
  status <- system2("curl", c("-fsSL", "https://sh.rustup.rs", "-o", installer))
  if (status != 0) {
    warning("rustup download failed with status ", status)
    return(invisible(FALSE))
  }
  status <- system2("sh", c(installer, "-y", "--profile", "minimal", "--default-toolchain", "stable"))
  if (status != 0) {
    warning("rustup bootstrap failed with status ", status)
    return(invisible(FALSE))
  }
  if (nzchar(Sys.which("rustup"))) system2("rustup", c("default", "stable"))
  cat("Rust toolchain after rustup: ", paste(system2("rustc", "--version", stdout = TRUE), collapse = " "), "\n", sep = "")
  invisible(TRUE)
}

ensure_modern_rust()

cran_pkgs <- availability$load_name[availability$availability == "cran"]
bioc_pkgs <- availability$load_name[availability$availability == "bioconductor"]
github_rows <- availability[availability$availability == "github_manifest", ]
archive_pkgs <- availability$load_name[availability$load_name %in% c("gghalves", "plogr")]
observed_dependency_bootstrap <- c(
  "TSP", "splitfngr", "warp", "DiceDesign", "hardhat", "plotly", "benchmarkmeData",
  "GlobalOptions", "tensorA", "svglite", "RcppParallel", "rex", "data.tree",
  "rematch2", "mnormt", "calibrate", "later", "R.methodsS3", "insight",
  "deeptimedata", "estimability", "fontBitstreamVera", "fstcore", "qs2",
  "pbapply", "mathjaxr", "metadat", "metafor"
)

failed <- character()
target_loads <- unique(c(cran_pkgs, bioc_pkgs, github_rows$load_name, archive_pkgs))
for (pass in seq_len(install_passes)) {
  cat("\nFull available install pass ", pass, " of ", install_passes, "\n", sep = "")
  failed <- c(failed, install_cran_bootstrap(observed_dependency_bootstrap))
  failed <- c(failed, install_repo_closure(unique(c(cran_pkgs, bioc_pkgs)), "available CRAN/Bioconductor"))
  failed <- c(failed, install_archives(archive_pkgs))
  failed <- c(failed, install_github_rows(github_rows))
  pass_remaining <- missing_loads(target_loads)
  cat("Remaining after pass ", pass, ": ", length(pass_remaining), "\n", sep = "")
  if (!length(pass_remaining)) break
}
failed <- unique(failed[nzchar(failed)])

remaining <- missing_loads(target_loads)
unresolved <- availability[availability$availability %in% c("unknown", "local_or_generated"), ]

cat("\nFull available install summary\n")
cat("Target load names: ", length(unique(target_loads)), "\n", sep = "")
cat("Remaining target missing: ", length(remaining), "\n", sep = "")
cat("Unresolved non-standard/local: ", nrow(unresolved), "\n", sep = "")

if (length(remaining)) {
  cat("Remaining target packages:\n")
  cat(paste0("  - ", remaining, collapse = "\n"), "\n", sep = "")
}

if (nrow(unresolved)) {
  cat("Unresolved packages are recorded in the availability report.\n")
}

if (length(remaining)) quit(status = 1)
quit(status = 0)
