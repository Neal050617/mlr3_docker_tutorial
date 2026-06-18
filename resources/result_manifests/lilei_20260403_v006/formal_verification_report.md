# mobior v0.0.6 Li Lei formal verification

- Result root: `lina_20251208/20260403_lilei/reproduce_formal_v006_msi12_name_20260609_000808`
- Branches checked: 10
- Completed step2 branches: 10/10
- Branches with biomarker TSV: 10/10
- Branches with AUC direction columns: 10/10
- Branches with prefixed metabolite descriptions: None
- Branches with selected biomarker text still containing Metabolite prefixes: None

## MSI filter
- Metabolite feature map rows: 1413
- MSI counts: MSI_level1=563, MSI_level2=850
- MSI_level3 rows retained: 0

## Low Test AUC models
- Branch=run_model_1.0; Model=adaboost; Test_AUC=0.484375; Test_AUC_Reversed=0.515625; Test_AUC_Direction_Flag=AUC_BELOW_0.5_REVERSED_RANKING_ON_THIS_SPLIT
- Branch=run_model_1.3; Model=nnet; Test_AUC=0.484375; Test_AUC_Reversed=0.515625; Test_AUC_Direction_Flag=AUC_BELOW_0.5_REVERSED_RANKING_ON_THIS_SPLIT
- Branch=run_model_1.4; Model=rpart; Test_AUC=0.453125; Test_AUC_Reversed=0.546875; Test_AUC_Direction_Flag=AUC_BELOW_0.5_REVERSED_RANKING_ON_THIS_SPLIT
- Branch=run_model_1.6; Model=rpart; Test_AUC=0.406250; Test_AUC_Reversed=0.593750; Test_AUC_Direction_Flag=AUC_BELOW_0.5_REVERSED_RANKING_ON_THIS_SPLIT

## Non-OK AUC direction flags
- Branch=run_model_1.0; Model=xgboost; Test_AUC=0.500000; Test_AUC_Reversed=NA; Test_AUC_Direction_Flag=SINGLE_CLASS_OR_CONSTANT_PROB
- Branch=run_model_1.0; Model=adaboost; Test_AUC=0.484375; Test_AUC_Reversed=0.515625; Test_AUC_Direction_Flag=AUC_BELOW_0.5_REVERSED_RANKING_ON_THIS_SPLIT
- Branch=run_model_1.3; Model=nnet; Test_AUC=0.484375; Test_AUC_Reversed=0.515625; Test_AUC_Direction_Flag=AUC_BELOW_0.5_REVERSED_RANKING_ON_THIS_SPLIT
- Branch=run_model_1.4; Model=xgboost; Test_AUC=0.500000; Test_AUC_Reversed=NA; Test_AUC_Direction_Flag=SINGLE_CLASS_OR_CONSTANT_PROB
- Branch=run_model_1.4; Model=rpart; Test_AUC=0.453125; Test_AUC_Reversed=0.546875; Test_AUC_Direction_Flag=AUC_BELOW_0.5_REVERSED_RANKING_ON_THIS_SPLIT
- Branch=run_model_1.6; Model=rpart; Test_AUC=0.406250; Test_AUC_Reversed=0.593750; Test_AUC_Direction_Flag=AUC_BELOW_0.5_REVERSED_RANKING_ON_THIS_SPLIT

## Output files
- `formal_verification_branch_checks.tsv`
- `formal_verification_model_summary.tsv`
- `formal_verification_biomarkers_long.tsv`
- `formal_verification_low_auc.tsv`
- `formal_verification_auc_flags.tsv`
