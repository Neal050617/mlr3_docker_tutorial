# 20260403_lilei 结果解读总入口

本目录作为当前正式结果的集中解读入口。性能汇总文件物理存放在本目录；Step2 技术设置和 SHAP 汇总仍保留在同级目录中，以保持脚本输出路径和审计规则稳定。

## 建议阅读顺序

1. `MODEL_PERFORMANCE_SUMMARY.md`
2. `all_model_metrics.tsv`
3. `../02_step2_technical_summary/README.md`
4. `../02_step2_technical_summary/step2_technical_summary.tsv`
5. `../02_step2_technical_summary/step2_best_hyperparameters.tsv`
6. `../03_shap_output_summary/SHAP_OUTPUT_SUMMARY.md`
7. `../03_shap_output_summary/shap_model_files.tsv`

## 解读边界

- 本目录不声明唯一最优模型。
- `branch_sorted_by_balanced_score.tsv` 和 `overall_sorted_by_balanced_score.tsv` 仅用于展示排序，不代表正式最优模型结论。
- `Balanced_Score` 是工程化辅助展示列，不作为独立统计学或临床结论依据。
- 分类阈值以各 Step2 输出记录为准，不默认使用固定 `0.5` 作为最优临界点。
- SHAP 用于解释模型，不用于模型选择。
