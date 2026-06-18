# Step2 技术设置汇总

分析目录：`/Users/colinliu/Desktop/mlr3_docker_tutorial/lina_20260515/reproduce_gp01_hc_af_af_positive_fullrerun_20260526_1531`

本文件由 `scripts/summarize_mlr3_results.R --tasks=step2` 自动生成。

## 这个汇总回答什么问题

- 每个 `run_model_*_step2` 的最终特征数是多少。
- 内层和外层交叉验证如何设置。
- 超参数优化使用多少次评估。
- 每个模型最终采用的超参数是什么。

## 输出文件

- `step2_technical_summary.tsv`：每个分支一行，汇总特征选择、CV 和调参设置。
- `step2_best_hyperparameters.tsv`：每个分支、每个模型、每个超参数一行。
- `step2_technical_summary.xlsx`：以上两个表的 Excel 版本。

## 注意

`Best_Model_By_CE` 来自 Step2 内部按交叉验证 classification error 记录的模型，不等同于本项目最终推荐模型。当前正式交付不声明唯一最优模型。
