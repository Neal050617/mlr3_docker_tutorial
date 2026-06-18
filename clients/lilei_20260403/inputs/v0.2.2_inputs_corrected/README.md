# v0.2.2 corrected inputs

本目录按 2026-05-15 修正版建模计划生成。

- 所有样本均来自映射表中粪便菌群和血清代谢均存在的交集。
- 所有矩阵列名均使用粪便菌群样本名。
- 0.1-0.3 使用 Con vs LAEC，n=246。
- 1.0-1.6 使用 NP_NRT vs P_NRT，n=77。
- 1.4-1.6 的组学特征为 RT - NRT 配对差值。

## 文件摘要

                                       file samples features
      model_0.1_con_laec_microbiome_246.tsv     246      327
      model_0.2_con_laec_metabolome_246.tsv     246     1802
      model_0.3_con_laec_multiomics_246.tsv     246     2129
                  model_1.0_clinical_77.tsv      77       15
       model_1.1_clinical_microbiome_77.tsv      77      301
       model_1.2_clinical_metabolome_77.tsv      77     1817
       model_1.3_clinical_multiomics_77.tsv      77     2103
 model_1.4_clinical_microbiome_delta_77.tsv      77      274
 model_1.5_clinical_metabolome_delta_77.tsv      77     1817
 model_1.6_clinical_multiomics_delta_77.tsv      77     2076
                    group_file             groups
    map-group_con_laec_246.txt   Con=118;LAEC=128
    map-group_con_laec_246.txt   Con=118;LAEC=128
    map-group_con_laec_246.txt   Con=118;LAEC=128
 map-group_nrt_response_77.txt NP_NRT=63;P_NRT=14
 map-group_nrt_response_77.txt NP_NRT=63;P_NRT=14
 map-group_nrt_response_77.txt NP_NRT=63;P_NRT=14
 map-group_nrt_response_77.txt NP_NRT=63;P_NRT=14
 map-group_nrt_response_77.txt NP_NRT=63;P_NRT=14
 map-group_nrt_response_77.txt NP_NRT=63;P_NRT=14
 map-group_nrt_response_77.txt NP_NRT=63;P_NRT=14
