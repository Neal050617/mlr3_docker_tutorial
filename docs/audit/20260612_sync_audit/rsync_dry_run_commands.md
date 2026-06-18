# Dry-run rsync commands

These commands intentionally do not change files.

```bash
rsync -av --dry-run dell:/work/users/chaoliu/Test/20250320-mlr3-test/mobior_v0.0.6/ mobior_v0.0.6/
rsync -av --dry-run dell:/work/users/chaoliu/Test/20250320-mlr3-test/R_packages_mobio/package_*_v0.0.6.tsv resources/package_manifests/
rsync -av --dry-run dell:/work/users/chaoliu/Test/20250320-mlr3-test/R_packages_mobio/full_*_v0.0.6.tsv resources/package_manifests/
```
