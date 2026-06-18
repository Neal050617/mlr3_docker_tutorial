# resources

This directory keeps lightweight manifests for server-side runtime state.

Do keep:

- package manifests and package check tables,
- result manifests and expected output counts,
- database download or checksum manifests.

Do not keep:

- R package libraries,
- Docker image archives,
- downloaded database binaries,
- full analysis result trees.

Server runtime root:

```text
dell:/work/users/chaoliu/Test/20250320-mlr3-test
```

Current synchronized lightweight resources:

- `package_manifests/`: `mobior:v0.0.6` package manifests and package check results.
- `result_manifests/lilei_20260403_v006/`: selected v006 verification and SHAP summary files.
