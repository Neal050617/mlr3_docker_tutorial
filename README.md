# mlr3 Docker Tutorial

This repository keeps the reproducible source side of the mlr3 modeling
workflows: curated inputs, scripts, manifests, runbooks, and client-facing
documentation. Generated analysis results are preserved on disk and synchronized
between machines with explicit rsync scripts instead of Git.

## Repository Layout

- `clients/`: canonical client project roots. Keep each client as a separate
  top-level directory under `clients/`; do not nest one client inside another.
- `scripts/`: shared modeling, SHAP redraw, audit, materialization, and sync
  scripts.
- `mobior_v0.0.6/`: current Docker/runtime source, package manifests, package
  install checks, and runbook. Package installation is performed on the Linux
  amd64 server runtime; compiled libraries are not copied into Git.
- `resources/`: lightweight package and result manifests used to prove
  reproducibility without committing full generated outputs.
- `docs/`: sync policy, cleanup audits, and repository handoff notes.
- `skills/`: repo-local delivery checklist and audit helper.

## Sync Policy

Git is for source assets only: scripts, docs, curated inputs, small manifests,
and reproducibility notes. Large/generated outputs such as PDFs, images,
archives, model binaries, `.RData`, `.rds`, `.npy`, `.pkl`, databases, and full
legacy mirrors are ignored by Git.

Use these scripts for machine-to-machine sync:

```bash
# Source assets only; follows .gitignore and defaults to dry-run.
bash scripts/sync_repository_sources_to_server.sh
APPLY=1 bash scripts/sync_repository_sources_to_server.sh

# Full client result directories; defaults to dry-run and no delete.
bash scripts/sync_large_results.sh
APPLY=1 bash scripts/sync_large_results.sh

# Full legacy mirrors kept under clients/*/legacy_mirror; defaults to dry-run.
bash scripts/sync_client_legacy_mirrors.sh
APPLY=1 bash scripts/sync_client_legacy_mirrors.sh
```

The default server target is `dell:/work/users/chaoliu/Test/20250320-mlr3-test`.
Override `REMOTE_HOST` and `REMOTE_ROOT` when syncing to another machine.

## Current Runtime

Use `mobior_v0.0.6/` as the current runtime source. See
`mobior_v0.0.6/PACKAGE_INSTALL_RUNBOOK.md` for package installation and package
check workflow.

Older local runtime snapshots are ignored and kept only for manual reference.
