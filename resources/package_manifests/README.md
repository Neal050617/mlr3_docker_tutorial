# package_manifests

This directory mirrors lightweight package status tables from:

```text
dell:/work/users/chaoliu/Test/20250320-mlr3-test/R_packages_mobio
```

It intentionally does not mirror the R package library itself.

Key files:

- `package_check_core_v0.0.6.tsv`: core runtime package load checks.
- `package_check_full_after_available_install_v0.0.6.tsv`: full-profile package checks after available installs.
- `full_unresolved_v0.0.6.tsv`: remaining unresolved full-profile packages and recommended handling.
- `full_missing_availability_v0.0.6.tsv`: packages missing from standard availability sources.

Current policy:

- Core mlr3/SHAP runtime packages must load successfully.
- Full-profile legacy packages can remain unresolved if they are test artifacts, unavailable upstream packages, or non-analysis tooling.
- Database-like packages such as `KEGG.db` should be handled through explicit source or database manifests, not copied from old libraries.
