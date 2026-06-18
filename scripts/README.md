# scripts

仓库级通用脚本目录。新项目复用时应优先从本目录调用通用 Step 脚本、批量 SHAP 和汇总脚本；项目目录只保留输入整理、分组定义、标题/branch map 和最终 wrapper。

| 脚本 | 用途 |
|---|---|
| `efs_valid_plus_step1.v0.2.2.R` | v0.2.2 Step1 仓库级入口；执行数据预处理、类型识别和 `preprocessed_data.RData` 输出 |
| `efs_valid_plus_step2.v0.2.2.R` | v0.2.2 Step2 仓库级入口；执行特征选择、模型训练、调参、性能评估和 workbook 输出 |
| `efs_valid_plus_step3.v0.2.2.R` | v0.2.2 Step3 仓库级入口；基于 Step2 `efs.RData` 生成 SHAP 解释输出 |
| `summarize_mlr3_results.R` | 通用 mlr3 建模结果汇总脚本；输入任意结果根目录，自动扫描其中的 `run_model_*_step2` 文件夹，并通过 `--tasks` 控制生成模型性能、Step2 技术设置、SHAP 输出和统一说明书 |
| `repair_mlr3_threshold_columns.R` | 修复旧 Step2 结果中的 `model_performance_comparison.xlsx` 展示：将 `_thr` 指标合并为正式无后缀分类指标，删除 `_thr` 冗余列，并保留实际阈值列；新 fullrerun 结果不应依赖该脚本作为必要步骤 |
| `run_mlr3_shap_all.sh` | 通用 Step3 SHAP 批量运行脚本；自动扫描结果根目录下的 `run_model_*_step2/efs.RData`，支持 case/control waterfall、指定样本、valid 和 force plot |
| `sync_repository_sources_to_server.sh` | 轻量仓库源文件同步脚本；按 `.gitignore` 生成清单，默认 dry-run，只同步 Git 可见的脚本、说明、输入/测试数据和结果文本清单 |
| `sync_large_results.sh` | 大型客户结果同步脚本；默认发现并同步所有 `clients/<client>/results/`，默认 dry-run、`MODE=ignore-existing`，永不使用 `--delete` |
| `sync_client_legacy_mirrors.sh` | 同步 `clients/<client>/legacy_mirror/` 完整旧目录镜像；默认 dry-run、`MODE=ignore-existing`，永不使用 `--delete` |
| `materialize_client_results.sh` | 将旧路径中的客户结果复制到 `clients/<client>/results/`；默认只处理 formal 结果，默认 dry-run，永不使用 `--delete` |
| `materialize_legacy_roots_to_clients.sh` | 将旧顶层入口完整镜像到 `clients/<client>/legacy_mirror/`；用于清理根目录前保留旧内容，默认 dry-run，永不使用 `--delete` |

基本用法：

运行 Step1/Step2/Step3：

```bash
Rscript scripts/efs_valid_plus_step1.v0.2.2.R --input=/path/to/input.tsv --map=/path/to/map-group.txt --gp=Case-Control --outdir=/path/to/step1
Rscript scripts/efs_valid_plus_step2.v0.2.2.R --load_preprocessed=/path/to/step1/preprocessed_data.RData --map=/path/to/map-group.txt --gp=Case-Control --outdir=/path/to/step2
Rscript scripts/efs_valid_plus_step3.v0.2.2.R --load_rdata=/path/to/step2/efs.RData --outdir=/path/to/step2/step3_shap
```

```bash
Rscript scripts/summarize_mlr3_results.R \
  --root=/path/to/result_root \
  --tasks=all
```

批量运行 Step3 SHAP：

```bash
SHAP_OUTDIR_NAME=step3_shap_formal_case_control_force_20260526 \
SHAP_WATERFALL_GROUPS=case,control \
SHAP_LOCAL_DISPLAY_FEATURES=10 \
SHAP_FORCE_PLOTS=TRUE \
bash scripts/run_mlr3_shap_all.sh /path/to/result_root
```

`SHAP_LOCAL_DISPLAY_FEATURES` 只限制 waterfall/force 单样本局部解释图的展示特征数；importance/beeswarm 主图仍使用 Step3 的 `--display_features` / `--auto_display_cap` 规则。

指定一个或多个样本出 waterfall/force 图：

```bash
SHAP_WATERFALL_SAMPLES=train:AF33,test:HC7 \
SHAP_FORCE_PLOTS=TRUE \
bash scripts/run_mlr3_shap_all.sh /path/to/result_root
```

清理 Step2 原始工作簿中的 `_thr` 冗余列：

```bash
Rscript scripts/repair_mlr3_threshold_columns.R \
  --root=/path/to/result_root
```

可选参数：

| 参数 | 作用 |
|---|---|
| `--branch-map=/path/to/input_summary.tsv` | 提供分支显示名称和输入类型；不提供时从目录名推断 |
| `--title=项目标题` | 控制生成 Markdown 标题 |
| `--project-name=项目名` | 控制默认项目名 |
| `--tasks=performance,step2,shap,manual` | 只生成指定模块；`all` 表示全部生成 |
| `--out-performance=...` | 自定义模型性能汇总目录 |
| `--out-step2=...` | 自定义 Step2 技术设置汇总目录 |
| `--out-shap=...` | 自定义 SHAP 汇总目录 |
| `--shap-dir-name=...` | 只汇总指定 SHAP 输出目录，避免 fastcheck、formal 和历史 SHAP 目录混在一起 |
| `--manual=...` | 自定义统一说明书路径 |

项目目录中只应保留必要的项目级包装入口；正式通用统计、批量 SHAP、Step1/2/3 主线实现和阈值列修复逻辑在本目录维护。`lina_20251208/efs_valid_plus_step*.v0.2.2.R` 保留为历史来源和测试目录上下文，不再作为跨项目复用入口。

仓库/服务器同步：

```bash
# 轻量源文件同步：按 .gitignore 规则同步，先 dry-run
bash scripts/sync_repository_sources_to_server.sh
APPLY=1 bash scripts/sync_repository_sources_to_server.sh

# 大型结果同步：用于 clients/<client>/results/，先 dry-run
bash scripts/sync_large_results.sh
APPLY=1 bash scripts/sync_large_results.sh
```

需要限制范围时使用 `RESULT_PATHS`：

```bash
RESULT_PATHS="clients/lilei_20260403/results" bash scripts/sync_large_results.sh
```

`clients/<client>/results/` 可以保留完整客户分析结果目录；Git 只跟踪其中的 Markdown/TSV/TXT/脚本等轻量文件，PDF、Excel、`.RData`、图片、压缩包和模型/数据库二进制文件通过 `sync_large_results.sh` 或显式 rsync 在 Mac、`dell`、`wuhe` 之间同步。

整理旧顶层入口前，先做 legacy mirror：

```bash
bash scripts/materialize_legacy_roots_to_clients.sh
APPLY=1 bash scripts/materialize_legacy_roots_to_clients.sh
```

`clients/**/legacy_mirror/**` 被 `.gitignore` 忽略，只作为 rsync 保留的完整旧目录镜像；正式可交付入口仍然是 `clients/<client>/docs`、`scripts`、`inputs`、`results`。

同步 legacy mirror：

```bash
bash scripts/sync_client_legacy_mirrors.sh
APPLY=1 bash scripts/sync_client_legacy_mirrors.sh
```

`lina_20260515` 本轮 v0.2.2 管线修正和跨项目复用说明见：

```text
lina_20260515/docs/05_v0.2.2管线修正与跨项目复用说明.md
```
