# lilei_20260403_v006

Source result root:

```text
dell:/work/users/chaoliu/Test/20250320-mlr3-test/lina_20251208/20260403_lilei/reproduce_formal_v006_msi12_name_20260609_000808
```

This is a `mobior:v0.0.6` SHAP and verification run for the Li Lei project. It is not yet a polished delivery root because the model performance and Step2 delivery summary bundles are not present in the v006 result root.

Verified server-side status on 2026-06-12:

| Item | Count |
|---|---:|
| Step2 branches | 10 |
| all-model SHAP branches | 10 |
| SHAP Excel files | 84 |
| importance PDF files | 168 |
| beeswarm PDF files | 168 |
| waterfall PDF files | 336 |
| force PDF files | 336 |
| completed SHAP status rows | 168 |
| skipped poor-model rows | 12 |

The `skipped_poor_model` rows are expected low-AUC skips, not container failures.

Full runtime outputs remain on the server. This directory keeps only verification and summary files needed to track reproducibility.
