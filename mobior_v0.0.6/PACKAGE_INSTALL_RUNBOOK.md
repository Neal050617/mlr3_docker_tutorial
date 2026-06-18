# MoBior v0.0.6 package install runbook

Use the server runtime for package installation:

```text
dell:/work/users/chaoliu/Test/20250320-mlr3-test
```

Do not copy a local macOS arm64 R package library into the server Linux amd64 runtime.
Compiled R packages can contain architecture-specific shared objects, and package compatibility is tied to the Docker image R/Bioconductor version.

Local should keep only:

- `mobior_v0.0.6/package_manifest.tsv`
- installer and checker scripts,
- package check TSVs under `resources/package_manifests/`

The server should keep:

- Docker image `mobior:v0.0.6`
- `R_packages_mobio/site-library`
- package check outputs under `R_packages_mobio/`

## Add a package

Edit:

```text
mobior_v0.0.6/package_manifest.tsv
```

Columns:

```text
package	load_name	source_type	source	version	install_ref	profile
```

Examples:

CRAN core package:

```tsv
newpkg	newpkg	cran	manual_manifest			core
```

Bioconductor core package:

```tsv
newbioc	newbioc	bioconductor	manual_manifest			core
```

GitHub package:

```tsv
newgithub	newgithub	github	manual_manifest		owner/repo	core
```

Full-profile legacy package:

```tsv
legacy_pkg	legacy_pkg	cran	manual_manifest			full
```

Local/generated package:

```tsv
KEGG.db	KEGG.db	local_or_generated	manual_manifest		path/to/KEGG.db_*.tar.gz	full
```

Use `core` only when the package is required for the formal mlr3/SHAP runtime.
Use `full` for optional or legacy analysis workflows.

## Install core packages on server

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

## Check core packages

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test

docker run --rm --network=host \
  --user "$(id -u):$(id -g)" \
  -v "$PWD/R_packages_mobio/site-library:/usr/local/lib/R/site-library" \
  -v "$PWD:/workspace" \
  -w /workspace/mobior_v0.0.6 \
  mobior:v0.0.6 \
  Rscript check_mobior_v006_packages.R \
    --manifest=package_manifest.tsv \
    --site-lib=/usr/local/lib/R/site-library \
    --out=/workspace/R_packages_mobio/package_check_core_v0.0.6.tsv \
    --profile=core \
    --strict=true
```

## Install full-profile available packages

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

## Check full profile

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test

docker run --rm --network=host \
  -v "$PWD/R_packages_mobio/site-library:/opt/mobior-full/site-library" \
  -v "$PWD:/workspace" \
  -w /workspace \
  -e R_SITE_LIBRARY=/opt/mobior-full/site-library \
  mobior:v0.0.6 \
  Rscript /opt/mobior/check_mobior_v006_packages.R \
    --manifest=/workspace/mobior_v0.0.6/package_manifest.tsv \
    --site-lib=/opt/mobior-full/site-library \
    --out=/workspace/R_packages_mobio/package_check_full_after_available_install_v0.0.6.tsv \
    --profile=full \
    --strict=false
```

Use `--strict=true` only when unresolved full-profile packages should fail the run.

## Sync package status back to local

After server install/check:

```bash
cd /Users/colinliu/Desktop/mlr3_docker_tutorial

rsync -av \
  dell:/work/users/chaoliu/Test/20250320-mlr3-test/R_packages_mobio/package_*_v0.0.6.tsv \
  resources/package_manifests/

rsync -av \
  dell:/work/users/chaoliu/Test/20250320-mlr3-test/R_packages_mobio/full_*_v0.0.6.tsv \
  resources/package_manifests/
```

Do not sync `R_packages_mobio/site-library/` into Git.

## Current unresolved full-profile items

The current unresolved list is:

```text
resources/package_manifests/full_unresolved_v0.0.6.tsv
```

Operational handling:

- `GseaVis`: pin a compatible commit or update its dependency stack before adding to the reproducible full profile.
- `KEGG.db`: generate with `createKEGGdb` or provide a local `KEGG.db_*.tar.gz`.
- `qs`: avoid on R 4.6; use `qs2` where possible or keep legacy `qs` workflows in an older R runtime.
- Test/example/tooling packages should stay out of formal runtime dependencies unless a workflow explicitly requires them.
