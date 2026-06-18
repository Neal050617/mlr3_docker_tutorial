# scripts

本目录存放 `lina_20260515` GP01.HC-AF 正式分析脚本。仓库级通用脚本位于 `../../scripts/`；本目录只保留 GP01 输入整理、建模调度、SHAP 调度兼容入口和最终交付包装入口。

## 脚本说明

| 脚本 | 用途 |
|---|---|
| `prepare_gp01_hc_af_inputs.R` | 读取原始 5 组学数据和 `map-group.txt`，生成 145 个共同样本的单组学输入矩阵 |
| `build_joint_biomarker_input.R` | 从 5 个单组学 Step2 的 `selected_features` 抽取 biomarkers，生成 `Model 0.6` 联合输入 |
| `run_gp01_hc_af_analysis.sh` | 统一运行 `prepare`、`single`、`joint` 或 `all` |
| `run_gp01_shap_all.sh` | GP01 兼容入口；实际调用仓库级 `scripts/run_mlr3_shap_all.sh` 自动扫描 Step2 分支并运行 SHAP |
| `summarize_gp01_final_delivery.sh` | GP01 包装入口；建模和 SHAP 完成后运行一次，调用通用脚本生成全部汇总和总说明书 |

## 常用命令

只生成输入：

```bash
bash lina_20260515/scripts/run_gp01_hc_af_analysis.sh prepare
```

运行完整正式分析：

```bash
export CASE_GROUP=AF
export CONTROL_GROUP=HC
bash lina_20260515/scripts/run_gp01_hc_af_analysis.sh all \
  /Users/colinliu/Desktop/mlr3_docker_tutorial/lina_20260515/reproduce_gp01_hc_af_af_positive_manual
```

本项目诊断任务以 `AF` 为 case/positive class，`HC` 为 control class。当前 Step2 使用 `positive = gp[1]`，因此脚本必须打印并传入 `gp=AF-HC` 后才能启动建模。

建模和 SHAP 都完成后，统一生成最终汇总和总说明书：

```bash
bash lina_20260515/scripts/summarize_gp01_final_delivery.sh \
  lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531
```

该命令会刷新：

```text
01_model_performance_summary/
02_step2_technical_summary/
03_shap_output_summary/
RESULT_INTERPRETATION_MANUAL.md
```

也可以直接调用合并版 R 脚本，并通过 `--tasks` 指定只统计某几类结果：

```bash
Rscript scripts/summarize_mlr3_results.R \
  --root=lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531 \
  --branch-map=lina_20260515/v0.2.2_inputs_gp01_hc_af/input_summary.tsv \
  --title=GP01.HC-AF \
  --shap-dir-name=step3_shap_formal_case_control_force_20260526 \
  --tasks=performance,step2,shap,manual
```

常用 `--tasks`：

| 参数 | 作用 |
|---|---|
| `performance` | 生成 `01_model_performance_summary/` |
| `step2` | 生成 `02_step2_technical_summary/` |
| `shap` | 生成 `03_shap_output_summary/` |
| `manual` | 生成 `RESULT_INTERPRETATION_MANUAL.md` 和结果入口 README |
| `all` | 运行以上全部 |

通用脚本不写死 6 个模型目录，而是自动扫描指定 `--root` 下的所有 `run_model_*_step2` 文件夹。`--branch-map` 是可选参数；不提供时，分支名称从目录名推断。GP01 项目提供 `input_summary.tsv` 作为 branch map，以保留中文组学名称。

如需清理 Step2 原始工作簿中旧版 `_thr` 冗余列：

```bash
Rscript scripts/repair_mlr3_threshold_columns.R \
  --root=/path/to/old_result_root
```

该脚本同时会把 `Sensitivity` 和 `Specificity` 补写到 `混淆矩阵_Train/Test` 以及每个单模型 sheet 中。当前 fullrerun 的正式 workbook 已由 Step2 主流程直接规范写出，不依赖该修复脚本。

如只需要单独重建某一类汇总，直接改 `--tasks`。

```bash
Rscript scripts/summarize_mlr3_results.R \
  --root=lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531 \
  --branch-map=lina_20260515/v0.2.2_inputs_gp01_hc_af/input_summary.tsv \
  --title=GP01.HC-AF \
  --tasks=performance
```

对当前正式结果补跑 SHAP。其他项目建议直接调用 `scripts/run_mlr3_shap_all.sh`；GP01 也可继续使用下面这个兼容入口：

```bash
SHAP_LOCAL_DISPLAY_FEATURES=10 \
bash lina_20260515/scripts/run_gp01_shap_all.sh \
  lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531
```

如只需基于已有 `*_SHAP.xlsx` 重画 waterfall/force 单样本图，不重新计算 SHAP：

```bash
Rscript scripts/redraw_mlr3_shap_local_plots.R \
  --root=lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531 \
  --shap-dir-name=step3_shap_formal_case_control_force_20260526 \
  --local-display-features=10
```

查看 SHAP 进度：

```bash
find lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531 \
  -path '*step3_shap_formal_case_control_force_20260526/*/*_SHAP.xlsx' | wc -l
```

## 依赖脚本

默认复用仓库级入口：

```text
scripts/efs_valid_plus_step1.v0.2.2.R
scripts/efs_valid_plus_step2.v0.2.2.R
scripts/efs_valid_plus_step3.v0.2.2.R
```

如需指定其他 v0.2.2 脚本根目录：

```bash
MLR3_ROOT=/path/to/compatible_script_dir \
  bash lina_20260515/scripts/run_gp01_hc_af_analysis.sh all \
  /Users/colinliu/Desktop/mlr3_docker_tutorial/lina_20260515/reproduce_gp01_hc_af_af_positive_manual
```
