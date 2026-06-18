# GP01.HC-AF v0.2.2 inputs

本目录由 `scripts/prepare_gp01_hc_af_inputs.R` 生成。

## 样本口径

- 原始 `map-group.txt`: HC=48, AF=99, 合计 147。
- 5 套组学共同样本: HC=46, AF=99, 合计 145。
- `HC20` 缺少口腔真菌，`HC37` 缺少肠道真菌，因此正式 5 组学分析统一排除这 2 个样本。

## 文件摘要

 model_id      modality   label_cn                                 input_file
      0.1    metabolome   非靶代谢    model_0.1_gp01_hc_af_metabolome_145.tsv
      0.2 oral_bacteria 口腔细菌属 model_0.2_gp01_hc_af_oral_bacteria_145.tsv
      0.3    oral_fungi 口腔真菌属    model_0.3_gp01_hc_af_oral_fungi_145.tsv
      0.4  gut_bacteria 肠道细菌属  model_0.4_gp01_hc_af_gut_bacteria_145.tsv
      0.5     gut_fungi 肠道真菌属     model_0.5_gp01_hc_af_gut_fungi_145.tsv
              preset samples features                          group_file
 metabolome_standard     145       59 map-group_gp01_hc_af_common_145.txt
 microbiome_standard     145      144 map-group_gp01_hc_af_common_145.txt
 microbiome_standard     145      159 map-group_gp01_hc_af_common_145.txt
 microbiome_standard     145      210 map-group_gp01_hc_af_common_145.txt
 microbiome_standard     145      119 map-group_gp01_hc_af_common_145.txt
      groups
 AF=99;HC=46
 AF=99;HC=46
 AF=99;HC=46
 AF=99;HC=46
 AF=99;HC=46
