# Clients

This directory is the canonical lightweight repository layout for client modeling projects.

It contains only reproducibility assets:

- README and method notes,
- source scripts,
- curated input files,
- small manifests and handoff notes.

Generated modeling results are intentionally kept out of this directory and out of Git.
Formal result trees remain in the runtime workspace and are synchronized separately when needed.

Current clients:

```text
clients/lilei_20260403/
clients/lina_20260515/
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

Legacy compatibility paths still exist locally and on the server:

```text
lina_20251208/20260403_lilei/
lina_20260515/
```

For new repository work, use the `clients/` paths.

Large inputs and full generated results stay outside Git. If a client needs a large input, keep a manifest in `clients/*/inputs/` and synchronize the real file with rsync or another external transfer tool.
