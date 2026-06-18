# 2026-06-12 repository and server sync plan

## Scope

Local workspace:

```text
/Users/colinliu/Desktop/mlr3_docker_tutorial
```

Server workspace:

```text
dell:/work/users/chaoliu/Test/20250320-mlr3-test
```

The goal is to make the local directory suitable as a GitHub repository while keeping the server as the runtime and large-result storage location.

## Current facts

- Local Docker image list contains `mobior:v0.0.5`; `mobior:v0.0.6` is not present locally.
- Server Docker image list contains `mobior:v0.0.6`, built on 2026-06-08, using R 4.6.0.
- Server `mobior_v0.0.6/` code directory is small enough for repository sync: about 12 MB.
- Server `R_packages_mobio/` is large runtime state: about 3.1 GB.
- Server `lina_20251208/20260403_lilei/` is about 1.3 GB.
- Server `lina_20260515/` is about 165 MB.
- Server v006 SHAP run directory is about 722 MB and is a completed SHAP/check run, not a polished delivery root.

## Directory policy

Different customers should not be nested under each other. The target logical layout should be:

```text
clients/
  lilei_20260403/
    docs/
    scripts/
    inputs/
    results/
    manifests/
  lina_20260515/
    docs/
    scripts/
    inputs/
    results/
    manifests/
mobior/
  v0.0.6/
    Dockerfile
    README.md
    RELEASE_NOTES.md
    R_scripts/
    install scripts
    package manifests
resources/
  db_manifest.tsv
  download_db.sh
  verify_db_checksums.sh
scripts/
tests/
```

The existing `lina_20251208/20260403_lilei` path can remain as a compatibility source during migration, but the curated repository layout should not place `lina_20260515` under `lina_20251208`.

## Git policy

Keep in Git:

- Source scripts: `.R`, `.sh`, Dockerfiles, compose files.
- Documentation: `README.md`, methods, handoff files, result interpretation guides.
- Small test data needed for reproducibility checks.
- Package manifests and package check summaries.
- Result manifests, expected counts, and checksums.

Keep out of Git:

- Docker images and image tarballs.
- R package libraries: `R_package_mobio/`, `R_packages_mobio/`, `site-library/`.
- Large generated files inside analysis result trees: PDFs, Excel outputs, `.RData`, raw database files, images, archives, model objects, and generated logs.

Customer analysis result directories may live under `clients/<client>/results/`.
Git should track only lightweight files from those result trees, such as Markdown/TSV/TXT summaries and scripts.
Large/generated result files are synchronized separately by rsync between Mac, `dell`, and `wuhe`.

## Immediate sync decisions

1. Pull server `mobior_v0.0.6/` code and manifests to local. Do not pull the image.
2. Pull selected server package manifests from `R_packages_mobio/`, not the package library itself.
3. Keep server v006 SHAP result as a runtime result. Pull or track only:
   - `formal_verification_*.tsv`
   - `formal_verification_report.md`
   - `03_shap_output_summary/SHAP_OUTPUT_SUMMARY.md`
   - `03_shap_output_summary/shap_status_summary.tsv`
   - `03_shap_output_summary/shap_model_files.tsv`
4. Upload local updated docs/scripts to server only after a dry-run diff is reviewed.

Status after the 2026-06-12 sync:

- `mobior_v0.0.6/` source and documentation were synced locally, excluding `vendor_packages/`.
- `resources/package_manifests/` was synced from server `R_packages_mobio/` TSV manifests.
- `resources/result_manifests/lilei_20260403_v006/` was synced from selected v006 verification and SHAP summary files.
- Missing local formal result files were uploaded to the matching server formal roots with `rsync --files-from` and `--ignore-existing`.
- After upload, local-to-server missing file count is 0 for:
  - `lina_20251208/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156`
  - `lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531`
- On 2026-06-17, repository/runtime sync boundaries were formalized in `docs/repository_runtime_sync_policy.md`.
- The local `.gitignore` was expanded so Git excludes modeling result roots, package libraries, binary runtime state, figures, reports, model objects, and database/cache files.
- `scripts/sync_repository_sources_to_server.sh` was added as the default lightweight local-to-server sync path. It syncs only docs, scripts, manifests, `mobior_v0.0.6` source, and curated input directories; it excludes `reproduce_*`, `run_model*_step2*`, `step3_shap*`, `archive/`, package libraries, and generated binary/report artifacts.
- The updated `.gitignore`, policy document, sync script, docs, scripts, manifests, and curated inputs were synced to `dell:/work/users/chaoliu/Test/20250320-mlr3-test`.
- Historical tracked large/generated files were reviewed on 2026-06-17. Clear generated outputs were removed from the Git index with `git rm --cached` while preserving the working-tree files. The cleanup log is `docs/audit/20260612_sync_audit/git_index_cleanup_20260617.md`.
- Remaining tracked `.xls` / `.xlsx` files are listed in `docs/audit/20260612_sync_audit/tracked_large_generated_files_in_git.txt`. They were deferred because several look like possible raw input, parameter, or handoff sheets; decide their final location during client-directory migration.
- A canonical `clients/` layout was created on 2026-06-17:
  - `clients/lilei_20260403/`
  - `clients/lina_20260515/`
- On 2026-06-18, formal result roots were materialized under `clients/<client>/results/` while preserving the old runtime paths for compatibility.
- `scripts/sync_repository_sources_to_server.sh` now syncs `clients/` as the repository-facing client layout and follows `.gitignore` via `git ls-files --cached --others --exclude-standard`. It can copy lightweight result summaries from `clients/<client>/results/`, but it does not copy large/generated result artifacts.
- Legacy paths remain on disk for compatibility and historical result roots, but their lightweight source copies are ignored by Git.
- `mobior_v0.0.6/PACKAGE_INSTALL_RUNBOOK.md` was added to document the server-side package installation workflow and the local/server architecture boundary.
- `scripts/sync_large_results.sh` was added for explicit large-result synchronization. It defaults to discovering all `clients/<client>/results/` roots, dry-run plus `MODE=ignore-existing`, never uses `--delete`, and is separate from Git/source sync.
- On 2026-06-18, additional historical client/project directories were audited in `docs/audit/20260618_client_candidate_migration.md`.
- The organizing rule was updated: customer analysis results can live under `clients/<client>/results/`; Git ignores large generated files such as `.RData`, zip, PDF, Excel, image, and binary database/model artifacts, while rsync handles those files between Mac, `dell`, and `wuhe`.
- `scripts/materialize_client_results.sh` was added to copy legacy result directories into `clients/<client>/results/`. It defaults to dry-run, `MODE=ignore-existing`, and never uses `--delete`.

Important caveat:

- Files named like `* 2.pdf` or `* 3.pdf` are numbered SHAP PDF copies. They should not be treated as the canonical formal output set.
- Canonical SHAP figure counts should be computed from unnumbered PDF names:
  - `20260403_lilei`: importance 142, beeswarm 142, waterfall 284, force 284.
  - `lina_20260515`: importance 108, beeswarm 108, waterfall 216, force 216.
- On 2026-06-17, after explicit confirmation, numbered SHAP PDF copies were moved out of the two formal roots on both local and server. They were archived instead of deleted.
- Formal-root post-clean verification:
  - `lina_20251208/20260403_lilei/reproduce_formal_case_positive_fullrerun_20260527_0156`: numbered PDF 0; unnumbered importance 142, beeswarm 142, waterfall 284, force 284.
  - `lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531`: numbered PDF 0; unnumbered importance 108, beeswarm 108, waterfall 216, force 216.
- Local archive paths:
  - `lina_20251208/20260403_lilei/archive/20260617_numbered_shap_pdf_duplicates/reproduce_formal_case_positive_fullrerun_20260527_0156/`
  - `lina_20260515/archive/20260617_numbered_shap_pdf_duplicates/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531/`
- Server archive paths:
  - `dell:/work/users/chaoliu/Test/20250320-mlr3-test/lina_20251208/20260403_lilei/archive/20260617_numbered_shap_pdf_duplicates/reproduce_formal_case_positive_fullrerun_20260527_0156/`
  - `dell:/work/users/chaoliu/Test/20250320-mlr3-test/lina_20260515/archive/20260617_numbered_shap_pdf_duplicates/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531/`
- Archive manifests:
  - `20260403_lilei`: 263 numbered duplicate PDF files.
  - `lina_20260515`: 117 numbered duplicate PDF files.

## v0.0.6 open items

- `R_packages_mobio/full_unresolved_v0.0.6.tsv` lists unresolved full-profile packages. These should be triaged into:
  - required for formal mlr3/SHAP runs,
  - optional historical full-profile packages,
  - unavailable/upstream-broken packages to document rather than force-install.
- Database resources should be represented by manifests and download/verify scripts rather than committed binary data.
- The v006 SHAP/check result does not currently include complete delivery summaries such as model performance and Step2 technical summary bundles.
- R package installation should run on the server Linux amd64 Docker runtime, not by copying local macOS arm64 package libraries. Compiled R packages are architecture- and R/Bioconductor-version sensitive; local should keep manifests and installer scripts only.
- For package installation details, use `mobior_v0.0.6/PACKAGE_INSTALL_RUNBOOK.md`.

## Next commands

Run the non-destructive audit:

```bash
bash scripts/audit_repository_sync.sh
```

Review:

```text
docs/audit/20260612_sync_audit/
```

Then perform whitelisted syncs only after checking the dry-run output.

For lightweight repository source sync:

```bash
bash scripts/sync_repository_sources_to_server.sh
APPLY=1 bash scripts/sync_repository_sources_to_server.sh
```

For large result files under `clients/<client>/results/`, use separate explicit `rsync` commands or the helper below and keep those large files out of Git.

Recommended helper:

```bash
bash scripts/sync_large_results.sh
APPLY=1 bash scripts/sync_large_results.sh
```

Limit scope with `RESULT_PATHS` when only selected result roots should be moved.
