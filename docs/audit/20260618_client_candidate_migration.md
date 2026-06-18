# Client candidate migration audit

Date: 2026-06-18

Purpose: identify older customer/project directories that should follow the same repository policy as `clients/lilei_20260403` and `clients/lina_20260515`.

Policy:

- Put customer project assets under `clients/`.
- Analysis results may live under `clients/<client>/results/`.
- Do not move or delete analysis results by default.
- Git should ignore large generated files such as `.RData`, zip, PDF, Excel, image, and binary database/model artifacts.
- Use separate rsync tooling, such as `scripts/sync_large_results.sh`, for large generated file synchronization across Mac, `dell`, and `wuhe`.
- Keep source sync limited to Git-eligible files.

## Current canonical client layout

```text
clients/lilei_20260403/
clients/lina_20260515/
```

These contain README, docs, scripts, inputs, and may contain `results/`.
Large generated files under `results/` are ignored by Git and synchronized with rsync.

## Candidate directories

| Source directory | Size | Files | Scripts | Docs | Input-like | Result-like | Recommended action |
|---|---:|---:|---:|---:|---:|---:|---|
| `data_for_analysis/20250211-mlr-JINYU` | 21M | 381 | 34 | 48 | 50 | 240 | Create `clients/jinyu_20250211/`; copy README/docs/scripts/curated inputs only. Keep result/report directories in place. |
| `data_for_analysis/20250908_金域数据单肺炎支原体感染数据建模_repeats20_seed123` | 1.7M | 54 | 0 | 4 | 9 | 24 | Fold into a Jinyu client family or create `clients/jinyu_20250908/`; avoid copying rendered report support files. |
| `data_for_analysis/金域_脚本` | 532K | 12 | 3 | 6 | 2 | 0 | Good lightweight source candidate; merge into the Jinyu client after checking overlap with `20250729_jinyu`. |
| `20250729_jinyu` | 532K | 13 | 3 | 6 | 2 | 1 | Good lightweight source candidate; likely merge or compare with `data_for_analysis/金域_脚本`. |
| `kangwenzhong` | 5.1M | 185 | 4 | 18 | 21 | 152 | Create `clients/kangwenzhong/`; copy README, method docs, scripts, and input maps only. Keep `efs*` results outside Git. |
| `kangwenzhong_delete_RE_15` | 3.0M | 256 | 1 | 5 | 29 | 222 | Treat as a variant/archive project; do not promote until compared with `kangwenzhong`. |
| `lina_valid` | 9.6M | 198 | 4 | 17 | 11 | 152 | Create `clients/lina_valid/`; copy validation docs/scripts/inputs only. Keep validation result trees outside Git. |
| `wangxueping` | 12K | 65 | 1 | 5 | 5 | 57 | Small but mostly result-like; copy only `efs.R`, maps, and method notes if still relevant. |
| `wangxueping_alltax` | 8K | 47 | 2 | 4 | 10 | 33 | Same family as Wang Xueping; merge with `wangxueping` after comparing scripts and maps. |
| `wangxueping_alltax2` | 724K | 143 | 2 | 9 | 13 | 127 | Result-heavy variant; keep results in place and extract only source assets if needed. |
| `python_xgboost_shap` | 224M | 141 | 1 | 6 | 1 | 133 | Create a tool/project client only if still used; do not copy `output/`. |

Counts were generated from local filesystem state on 2026-06-18.
They are for triage, not a completeness guarantee.

## Suggested order

1. Jinyu family:
   - `data_for_analysis/20250211-mlr-JINYU`
   - `data_for_analysis/20250908_金域数据单肺炎支原体感染数据建模_repeats20_seed123`
   - `data_for_analysis/金域_脚本`
   - `20250729_jinyu`
2. Kang Wenzhong:
   - `kangwenzhong`
   - `kangwenzhong_delete_RE_15`
3. Lina validation:
   - `lina_valid`
4. Wang Xueping family:
   - `wangxueping`
   - `wangxueping_alltax`
   - `wangxueping_alltax2`
5. Python XGBoost SHAP:
   - `python_xgboost_shap`

## Important rule

Do not copy full result directories into Git-tracked content.
They may be placed under `clients/<client>/results/`, but large/generated files are ignored by Git and synchronized separately.
Examples that should remain outside Git tracking even if they live under `clients/`:

```text
**/efs*/
**/efs_test*/
**/output/
**/outputs/
**/analysis_report/
**/*_files/
**/fig/
**/中间文件/
```

These are covered by `.gitignore` for future additions, but already tracked files still require reviewed `git rm --cached` cleanup if they should leave the Git index.

## Migration performed

After the audit, lightweight client directories were created under `clients/` for:

```text
clients/jinyu_20250211/
clients/jinyu_20250729/
clients/jinyu_20250908/
clients/jinyu_scripts/
clients/kangwenzhong/
clients/kangwenzhong_delete_RE_15/
clients/lina_valid/
clients/python_xgboost_shap/
clients/wangxueping/
clients/wangxueping_alltax/
clients/wangxueping_alltax2/
```

The copy excluded result-like paths and generated outputs.
Post-copy checks:

```text
client_files=246
bad_result_matches=0
files_larger_than_20MB=0
```

Large input handling:

```text
clients/python_xgboost_shap/inputs/LARGE_INPUT_MANIFEST.tsv
```

The original large input remains at:

```text
python_xgboost_shap/input/clr_variance_filtered.xls
```

## Historical results materialization

After the policy update, historical result directories are allowed to live under
`clients/<client>/results/`. They remain outside Git when they are large or
generated artifacts; rsync handles those files across Mac, `dell`, and `wuhe`.

The historical result candidate inventory is:

```text
docs/audit/20260618_historical_result_materialize_candidates.tsv
```

Inventory summary before materialization:

```text
historical_targets=32
historical_files=765
largest_target=python_xgboost_shap/output, 139M, 132 files
```

Materialization command:

```bash
RESULT_SET=historical MODE=ignore-existing APPLY=1 bash scripts/materialize_client_results.sh
```

Safety settings:

- `MODE=ignore-existing` only backfills missing files.
- The script never passes `--delete`.
- Existing legacy result paths are preserved for compatibility.

Post-materialization verification:

```text
docs/audit/20260618_historical_result_post_materialize_counts.tsv
```

Result:

```text
historical_targets=32
missing_sources=0
mismatched_targets=4
```

The four mismatches are only `.DS_Store` files intentionally excluded by
`scripts/materialize_client_results.sh`; no analysis result files were missing
from the materialized `clients/<client>/results/` copies.

## Large result rsync to dell

Large result files were synchronized from local Mac to:

```text
dell:/work/users/chaoliu/Test/20250320-mlr3-test/clients/
```

Command:

```bash
APPLY=1 MODE=ignore-existing bash scripts/sync_large_results.sh
```

Summary:

```text
docs/audit/20260618_large_results_rsync_to_dell_summary.tsv
```

Post-sync local/server counts:

```text
result_total_files=3311
result_large_files=2799
result_text_files=469
```

The same counts were observed on local Mac and `dell` after sync.

## Legacy root mirrors

Some historical top-level paths had curated source/result copies under
`clients/`, but not a complete source-root mirror. To make the repository root
cleanable while preserving old contents, the following legacy roots were copied
under `clients/*/legacy_mirror/`.

Command:

```bash
APPLY=1 MODE=ignore-existing bash scripts/materialize_legacy_roots_to_clients.sh
```

Mappings:

```text
20250729_jinyu
  -> clients/jinyu_20250729/legacy_mirror/20250729_jinyu

data_for_analysis/金域_脚本
  -> clients/jinyu_scripts/legacy_mirror/data_for_analysis__金域_脚本

lina_20251208/20260403_lilei
  -> clients/lilei_20260403/legacy_mirror/lina_20251208__20260403_lilei

lina_20251208, excluding /20260403_lilei/
  -> clients/lina_20251208_legacy/legacy_mirror/lina_20251208_root

lina_20260515
  -> clients/lina_20260515/legacy_mirror/lina_20260515
```

Post-copy verification:

```text
docs/audit/20260618_legacy_roots_materialize_post_counts.tsv
docs/audit/20260618_legacy_roots_materialize_apply_summary.tsv
```

All expected source file counts matched destination file counts after excluding
editor/runtime folders handled by the script: `.DS_Store`, `.r_libs/`,
`.specstory/`, `.claude/`, and `.vscode/`.

`clients/**/legacy_mirror/**` is ignored by Git. It should be preserved by
rsync, not by Git source sync.

After this step, the old top-level paths below are no longer required as
official entrypoints, but they have not been deleted:

```text
20250729_jinyu/
data_for_analysis/金域_脚本/
lina_20251208/
lina_20260515/
```

Legacy mirrors were also synchronized from local Mac to `dell`:

```bash
APPLY=1 MODE=ignore-existing bash scripts/sync_client_legacy_mirrors.sh
```

Summary:

```text
docs/audit/20260618_legacy_mirrors_rsync_to_dell_summary.tsv
```

Post-sync verification before exact-file completion:

```text
local_legacy_files=5242
server_legacy_files=5242
git_visible_legacy_files=0
```

After deciding that external roots can be cleaned only after exact file
confirmation, runtime/editor files that were previously skipped were also added
to `legacy_mirror/`:

```text
docs/audit/20260618_legacy_roots_exact_compare.tsv
docs/audit/20260618_legacy_roots_materialize_exact_apply_summary.tsv
docs/audit/20260618_legacy_mirrors_exact_sync_to_dell_summary.tsv
```

Exact comparison result:

```text
20250729_jinyu: source_only=0, destination_only=0
data_for_analysis/金域_脚本: source_only=0, destination_only=0
lina_20251208/20260403_lilei: source_only=0, destination_only=0
lina_20251208 excluding /20260403_lilei/: source_only=0, destination_only=0
lina_20260515: source_only=0, destination_only=0
```

Final local/server legacy mirror counts:

```text
local_legacy_files=5361
server_legacy_files=5361
git_visible_legacy_files=0
```

The same exact-mirror and cleanup procedure was then extended to the remaining
external client-analysis directories:

```text
data_for_analysis/20250211-mlr-JINYU/
data_for_analysis/20250908_金域数据单肺炎支原体感染数据建模_repeats20_seed123/
kangwenzhong/
kangwenzhong_delete_RE_15/
lina/
lina_valid/
python_xgboost_shap/
wangxueping/
wangxueping_alltax/
wangxueping_alltax2/
```

Verification and sync records:

```text
docs/audit/20260618_remaining_legacy_exact_compare.tsv
docs/audit/20260618_all_legacy_roots_materialize_apply_summary.tsv
docs/audit/20260618_all_legacy_mirrors_sync_to_dell_summary.tsv
docs/audit/20260618_external_roots_cleanup_paths.txt
```

Final cleanup verification:

```text
local_legacy_files=6839
server_legacy_files=6839
local_result_files=3317
server_result_files=3317
git_visible_legacy_files=0
git_visible_result_large_files=0
```

The old external roots were moved, not hard-deleted:

```text
local_trash=/Users/colinliu/.Trash/mlr3_external_roots_20260618_141106
server_trash=/work/users/chaoliu/Test/mlr3_external_roots_trash_20260618_141107
```
