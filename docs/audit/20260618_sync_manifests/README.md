# 2026-06-18 sync manifest summary

## Context
- `generated_at`: 2026-06-18 16:08:18 +0800
- `cwd`: /Users/colinliu/Desktop/mlr3_docker_tutorial
- `branch`: main
- `head`: f529e3b28e99a9892535d1b69a984af0b721625a
- `upstream`: origin/main
- `ahead_behind`: 0

## Git candidates
- `status_total`: 132
- `status_??`: 33
- `status_D`: 97
- `status_M`: 2
- `candidate_raw`: 1015
- `candidate_existing`: 936
- `git_visible_legacy_mirror`: 0
- `git_visible_result_large_or_csv`: 0

## Source rsync dry-run
- `source_rsync_exit`: 0
- `source_path_list_files`: 829
- `source_legacy_mirror_files`: 0
- `source_result_large_or_csv`: 0

## Result rsync dry-run
- `results_rsync_exit`: 0
- `result_roots`: 9
- `errors`: 0

Result transfer summary:

```text
clients/kangwenzhong/results	0	0 B
clients/kangwenzhong_delete_RE_15/results	0	0 B
clients/lilei_20260403/results	0	0 B
clients/lina_20260515/results	0	0 B
clients/lina_valid/results	0	0 B
clients/python_xgboost_shap/results	0	0 B
clients/wangxueping/results	0	0 B
clients/wangxueping_alltax/results	0	0 B
clients/wangxueping_alltax2/results	0	0 B
```

## Legacy mirror rsync dry-run
- `legacy_rsync_exit`: 0
- `legacy_roots`: 15
- `errors`: 0

Legacy transfer summary:

```text
clients/jinyu_20250211/legacy_mirror	0	0 B
clients/jinyu_20250729/legacy_mirror	0	0 B
clients/jinyu_20250908/legacy_mirror	0	0 B
clients/jinyu_scripts/legacy_mirror	0	0 B
clients/kangwenzhong/legacy_mirror	0	0 B
clients/kangwenzhong_delete_RE_15/legacy_mirror	0	0 B
clients/lilei_20260403/legacy_mirror	0	0 B
clients/lina_20251208_legacy/legacy_mirror	0	0 B
clients/lina_20260515/legacy_mirror	0	0 B
clients/lina_legacy/legacy_mirror	0	0 B
clients/lina_valid/legacy_mirror	0	0 B
clients/python_xgboost_shap/legacy_mirror	0	0 B
clients/wangxueping/legacy_mirror	0	0 B
clients/wangxueping_alltax/legacy_mirror	0	0 B
clients/wangxueping_alltax2/legacy_mirror	0	0 B
```

## Interpretation

- Git/source rsync follow `.gitignore`; they exclude `clients/**/legacy_mirror/**` and result large files.
- Result rsync and legacy mirror rsync intentionally sync Git-ignored runtime artifacts by explicit path.
- Current result and legacy rsync dry-runs show 0 bytes pending for all roots.
