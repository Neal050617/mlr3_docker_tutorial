# Repository and runtime sync policy

Date: 2026-06-17

This repository and the server directory below are both modeling-analysis workspaces:

```text
local: /Users/colinliu/Desktop/mlr3_docker_tutorial
server: dell:/work/users/chaoliu/Test/20250320-mlr3-test
```

Git should hold only reproducibility assets:

- input files needed for reruns or small tests,
- source scripts,
- Docker and compose definitions,
- README and method notes,
- package manifests,
- result manifests and audit summaries.

Git should not hold large generated binary state:

- PDFs, Excel outputs, rendered HTML reports, figures, model objects, `.RData`,
- Docker images and image tarballs,
- R package libraries such as `R_packages_mobio/site-library`,
- database binaries and caches.

Customer analysis result directories may live under `clients/<client>/results/`.
Git tracks only lightweight text/code/manifest files from those trees; large generated files are ignored and synchronized explicitly with rsync.

## Client layout

The canonical lightweight client layout is:

```text
clients/
  lilei_20260403/
    docs/
    scripts/
    inputs/
    results/
  lina_20260515/
    docs/
    scripts/
    inputs/
    results/
```

Legacy paths such as `lina_20251208/20260403_lilei/` remain as compatibility/runtime locations, especially because historical result roots still live there.
New repository-facing docs, scripts, and curated inputs should be added under `clients/`.

Other historical customer/project directories should follow the same rule.
Move or copy customer-facing result directories into `clients/<client>/results/` when organizing the workspace, but do not rely on Git for large generated files.
Large inputs should also stay outside Git unless intentionally approved as test data; represent them with `LARGE_INPUT_MANIFEST.tsv` and use external sync.
The migration audit is:

```text
docs/audit/20260618_client_candidate_migration.md
```

## Source sync

Use the dry-run source sync script first:

```bash
bash scripts/sync_repository_sources_to_server.sh
```

Apply only after reviewing the dry-run output:

```bash
APPLY=1 bash scripts/sync_repository_sources_to_server.sh
```

The script is intentionally conservative: inside a Git work tree, it builds its file list with:

```bash
git ls-files --cached --others --exclude-standard
```

This means source sync follows `.gitignore` directly. `clients/<client>/results/` may be traversed, but only Git-visible lightweight files such as Markdown, TSV, TXT, and scripts are copied by source sync. PDF, Excel result workbooks, `.RData`, archives, figures, model objects, and database binaries remain excluded from source sync and should be moved with the large-result rsync workflow below.

## Large result sync

Large analysis results are synchronized separately from repository sources.
If legacy results have not yet been copied into `clients/`, first materialize them locally:

```bash
bash scripts/materialize_client_results.sh
APPLY=1 bash scripts/materialize_client_results.sh
```

Use dry-run first:

```bash
bash scripts/sync_large_results.sh
```

Apply after review:

```bash
APPLY=1 bash scripts/sync_large_results.sh
```

By default, the script discovers all current client result roots:

```text
clients/*/results
```

Set `RESULT_PATHS` to restrict the sync to one or more specific result roots.

Important safety rules:

- It never uses `--delete`.
- It defaults to dry-run.
- It is separate from Git and from source sync.
- The default `MODE=ignore-existing` only backfills missing files.
- Use `MODE=update` only when existing result files should be overwritten by newer/different local copies.
- Use `DIRECTION=pull` to copy results from server back to local.

Examples:

```bash
APPLY=1 bash scripts/sync_large_results.sh
```

```bash
MODE=update APPLY=1 bash scripts/sync_large_results.sh
```

```bash
DIRECTION=pull RESULT_PATHS="lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531" \
  bash scripts/sync_large_results.sh
```

## R package policy

R package installation for `mobior:v0.0.6` should be performed on the server.

Reason:

- local macOS is arm64,
- server Docker runtime is Linux amd64,
- compiled R packages can contain architecture-specific shared objects,
- Bioconductor/R version compatibility is tied to the runtime image.

The local machine should keep package manifests and install scripts, not a reusable copy of the server package library.

For server installation:

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test
mkdir -p R_packages_mobio/site-library

docker run --rm --network=host \
  --user "$(id -u):$(id -g)" \
  -e R_SITE_LIBRARY=/usr/local/lib/R/site-library \
  -e MOBIOR_PROFILE=core \
  -v "$PWD/R_packages_mobio/site-library:/usr/local/lib/R/site-library" \
  -v "$PWD:/workspace" \
  -w /workspace/mobior_v0.0.6 \
  mobior:v0.0.6 \
  Rscript install_mobior_v006_packages.R
```

For full-profile legacy overlay:

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test
mkdir -p R_packages_mobio/site-library

docker run --rm --network=host \
  --user "$(id -u):$(id -g)" \
  -e R_SITE_LIBRARY=/opt/mobior-full/site-library \
  -e NCPUS=1 \
  -e FULL_INSTALL_PASSES=3 \
  -v "$PWD/R_packages_mobio/site-library:/opt/mobior-full/site-library" \
  -v "$PWD:/workspace" \
  -w /workspace/mobior_v0.0.6 \
  mobior:v0.0.6 \
  Rscript install_full_available_v006_packages.R
```

After installation, regenerate package check TSVs and sync only those manifests back to local `resources/package_manifests/`.
