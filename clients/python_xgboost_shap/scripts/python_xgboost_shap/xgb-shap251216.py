# 最终完整版: XGBoost回归分析 (训练、保存、加载、增强SHAP解释)
# 包含所有交叉验证、RFE过程和最终结果的保存和可视化
import pandas as pd
import numpy as np
import warnings
import time
import pickle
import matplotlib.pyplot as plt
import seaborn as sns
import xgboost as xgb
import shap
import os
import logging
import json
from collections import Counter
import sys
import re
from sklearn.model_selection import KFold, RandomizedSearchCV
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.feature_selection import RFE
from sklearn.pipeline import Pipeline

# 导入自定义waterfall配置
try:
    from waterfall_config import WATERFALL_CONFIG, generate_waterfall_plots_with_config
except ImportError:
    print("警告：无法导入waterfall_config，将使用默认设置")
    WATERFALL_CONFIG = None
    generate_waterfall_plots_with_config = None

# --- 全局配置 ---
RANDOM_STATE = 42
np.random.seed(RANDOM_STATE)
#TRAIN_MODEL = True
TRAIN_MODEL = False
warnings.filterwarnings('ignore')

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('xgboost_analysis.log', mode='w' if TRAIN_MODEL else 'a'),
        logging.StreamHandler()
    ]
)

plt.style.use('ggplot')

def setup_data():
    """
    数据加载和预处理
    """
    logging.info("开始数据加载和预处理...")
    # 你的数据已经过对数比转换（如CLR），因此不需要额外的标准化。
    otu_df = pd.read_csv("./input/clr_variance_filtered.xls", sep='\t')
    metadata_df = pd.read_csv("./input/age.txt", sep='\t')

    logging.info(f"原始OTU表形状: {otu_df.shape}")

    logging.info(f"筛选后OTU表形状: {otu_df.shape}")

    otu_df = otu_df.set_index('OTU_tax').T.reset_index()
    otu_df = otu_df.rename(columns={'index': 'sample'})

    metadata_df = metadata_df.rename(columns={'#SampleID': 'sample'})

    merged_df = pd.merge(otu_df, metadata_df[['sample', 'age']], on='sample', how='inner')
    logging.info(f"合并后数据形状: {merged_df.shape}")

    X = merged_df.drop(['sample', 'age'], axis=1)
    y = merged_df['age']
    # 保留样本名称信息用于后续分析，确保与X、y的行索引完全对应
    sample_names = merged_df['sample'].tolist()
    
    print(f"样本信息预览（前5个）: {sample_names[:5]}")
    print(f"总样本数: {len(sample_names)}")
    print(f"X形状: {X.shape}, y形状: {y.shape}, 样本名称数量: {len(sample_names)}")
    
    # 数据一致性验证
    assert len(sample_names) == len(X) == len(y), f"数据长度不一致: sample_names={len(sample_names)}, X={len(X)}, y={len(y)}"
    logging.info(f"数据一致性验证通过: 样本名称、特征矩阵、目标变量长度均为 {len(sample_names)}")

    output_path = "./output"
    os.makedirs(output_path, exist_ok=True)
    os.makedirs(os.path.join(output_path, 'models'), exist_ok=True)
    os.makedirs(os.path.join(output_path, 'shap_values'), exist_ok=True)
    os.makedirs(os.path.join(output_path, 'results'), exist_ok=True)
    os.makedirs(os.path.join(output_path, 'plots'), exist_ok=True)
    os.makedirs(os.path.join(output_path, 'csv_paths'), exist_ok=True)
    logging.info(f"输出目录已创建: {output_path}")

    return X, y, sample_names, output_path


def train_and_save_model(X, y, output_path):
    print("\n--- 步骤1: Pipeline联合交叉验证与超参数优化 ---")
    logging.info("开始嵌套交叉验证...")
    
    outer_cv = KFold(n_splits=5, shuffle=True, random_state=RANDOM_STATE)
    inner_cv = KFold(n_splits=5, shuffle=True, random_state=RANDOM_STATE)
    
    rfe_estimator = xgb.XGBRegressor(
        objective='reg:squarederror', 
        n_estimators=100,
        max_depth=3,
        learning_rate=0.1,
        n_jobs=-1,
        random_state=RANDOM_STATE
    )
    
    # 封装整个流程：RFE特征选择 -> XGBoost模型
    pipeline = Pipeline(steps=[
        ('feature_select', RFE(estimator=rfe_estimator)),
        ('xgb', xgb.XGBRegressor(objective='reg:squarederror', n_jobs=-1, random_state=RANDOM_STATE))
    ])

    # 配置随机搜索的参数网格
    param_grid = {
        'feature_select__n_features_to_select': list(range(5, X.shape[1] + 1, 5)),
        'xgb__n_estimators': [100, 200, 300],
        'xgb__max_depth': [3, 5, 7],
        'xgb__learning_rate': [0.01, 0.05, 0.1],
        'xgb__subsample': [0.8, 0.9, 1.0],
        'xgb__colsample_bytree': [0.8, 0.9, 1.0],
        'xgb__reg_alpha': [0, 0.1, 0.5],
        'xgb__reg_lambda': [0.1, 1, 2]
    }

    cv_results_list = []
    outer_scores_list = []
    best_params_list = []
    selected_features_list = []
    
    start_total_time = time.time()
    for fold, (train_index, test_index) in enumerate(outer_cv.split(X, y)):
        print(f"\n--- 外部折叠: {fold + 1}/{outer_cv.n_splits} ---")
        X_train, X_test = X.iloc[train_index], X.iloc[test_index]
        y_train, y_test = y.iloc[train_index], y.iloc[test_index]
        
        start_fold_time = time.time()
        
        # 使用 RandomizedSearchCV 进行内层超参数优化
        random_search = RandomizedSearchCV(
            estimator=pipeline, 
            param_distributions=param_grid, 
            n_iter=50, # 你可以根据计算资源调整这个值
            cv=inner_cv, 
            scoring='neg_mean_squared_error', 
            random_state=RANDOM_STATE,
            n_jobs=-1,
            verbose=1
        )
        random_search.fit(X_train, y_train)
        
        best_pipeline = random_search.best_estimator_
        best_params = random_search.best_params_
        best_params_list.append(best_params)
        
        final_features = X_train.columns[best_pipeline.named_steps['feature_select'].support_]
        selected_features_list.append(final_features)
        
        y_pred = best_pipeline.predict(X_test)
        mse = mean_squared_error(y_test, y_pred)
        r2 = r2_score(y_test, y_pred)
        
        outer_scores_list.append({'R2_score': r2, 'MSE_score': mse})
        
        end_fold_time = time.time()
        print(f"外部折叠 {fold + 1} 耗时: {end_fold_time - start_fold_time:.2f} 秒")
        print(f"外部折叠 {fold + 1} - MSE: {mse:.4f}, R^2: {r2:.4f}")
        print(f"最佳参数: {best_params}")
    
    end_total_time = time.time()
    logging.info(f"嵌套交叉验证总耗时: {end_total_time - start_total_time:.2f} 秒")

    cv_results_df = pd.DataFrame(outer_scores_list)
    cv_results_df.to_csv(os.path.join(output_path, 'results', 'cross_validation_summary.csv'), index=False)
    
    with open(os.path.join(output_path, 'results', 'best_params_list.pkl'), 'wb') as f:
        pickle.dump(best_params_list, f)
    with open(os.path.join(output_path, 'results', 'selected_features_list.pkl'), 'wb') as f:
        pickle.dump(selected_features_list, f)

    print("\n--- 交叉验证最终结果 ---")
    avg_r2 = cv_results_df['R2_score'].mean()
    std_r2 = cv_results_df['R2_score'].std()
    avg_mse = cv_results_df['MSE_score'].mean()
    std_mse = cv_results_df['MSE_score'].std()
    print(f"平均R^2: {avg_r2:.4f} ± {std_r2:.4f}")
    print(f"平均MSE: {avg_mse:.4f} ± {std_mse:.4f}")

    print("\n--- 步骤2: 最终模型训练与保存 ---")
    logging.info("开始最终模型训练...")

    agg_params = {}
    param_keys = best_params_list[0].keys()
    for key in param_keys:
        values = [p[key] for p in best_params_list]
        if 'n_features' in key or 'n_estimators' in key or 'max_depth' in key:
            agg_params[key] = int(round(np.mean(values)))
        else:
            agg_params[key] = np.mean(values)

    final_pipeline = Pipeline(steps=[
        ('feature_select', RFE(estimator=rfe_estimator, n_features_to_select=agg_params.pop('feature_select__n_features_to_select'))),
        ('xgb', xgb.XGBRegressor(objective='reg:squarederror', n_jobs=-1, random_state=RANDOM_STATE, **{k.replace('xgb__', ''): v for k, v in agg_params.items()}))
    ])

    final_pipeline.fit(X, y)
    
    final_model = final_pipeline.named_steps['xgb']
    final_features = X.columns[final_pipeline.named_steps['feature_select'].support_]
    X_final = X[final_features]
    logging.info(f"最终模型使用的特征数量: {len(final_features)}")

    try:
        with open(os.path.join(output_path, 'models', 'final_pipeline.pkl'), "wb") as f:
            pickle.dump(final_pipeline, f)
        with open(os.path.join(output_path, 'results', 'final_features.txt'), "w") as f:
            for feature in final_features:
                f.write(f"{feature}\n")
        with open(os.path.join(output_path, 'results', 'final_model_params.json'), 'w') as f:
            json.dump({k: v for k, v in agg_params.items()}, f, indent=4)
        logging.info("所有模型和结果文件已成功保存。")
    except Exception as e:
        logging.error(f"文件保存过程中发生错误: {e}")
        return None, None, None
        
    return final_model, X_final, output_path


def load_model_and_data(output_path):
    """
    加载已保存的模型和数据
    """
    print("\n--- 步骤1: 加载数据和训练好的模型 ---")
    logging.info("开始加载必要的文件...")
    
    X, y, sample_names, _ = setup_data()
    if X is None:
        return None, None, None, None

    try:
        model_file_path = os.path.join(output_path, 'models', 'final_pipeline.pkl')
        with open(model_file_path, "rb") as f:
            final_pipeline = pickle.load(f)
        logging.info(f"最终Pipeline模型已加载: {model_file_path}")

        features_file_path = os.path.join(output_path, 'results', 'final_features.txt')
        with open(features_file_path, "r") as f:
            final_features = [line.strip() for line in f.readlines()]
        logging.info(f"最终特征列表已加载，共 {len(final_features)} 个特征。")

        # 从原始X中选择最终特征，以保证后续SHAP解释使用的数据是正确的
        X_final = X[final_features]
        final_model = final_pipeline.named_steps['xgb']
        # 样本名称信息直接从setup_data()获取，无需依赖训练时保存的文件
        logging.info(f"用于SHAP解释的数据形状: {X_final.shape}")
        logging.info(f"样本名称列表已从数据源获取，共 {len(sample_names)} 个样本。")
        print(f"样本信息预览（前5个）: {sample_names[:5]}")
        
        # 数据一致性验证：确保样本名称、X_final、y的行索引完全对应
        assert len(sample_names) == len(X_final) == len(y), f"数据长度不一致: sample_names={len(sample_names)}, X_final={len(X_final)}, y={len(y)}"
        
        # 保存样本名称和索引映射关系供验证使用
        sample_mapping_path = os.path.join(output_path, 'results', 'sample_index_mapping.csv')
        sample_mapping_df = pd.DataFrame({
            'index': range(len(sample_names)),
            'sample_name': sample_names
        })
        sample_mapping_df.to_csv(sample_mapping_path, index=False)
        logging.info(f"样本索引映射关系已保存至: {sample_mapping_path}")
        
        print(f"\n数据一致性验证通过：")
        print(f"- 样本名称数量: {len(sample_names)}")
        print(f"- 特征矩阵行数: {len(X_final)}")
        print(f"- 目标变量行数: {len(y)}")
        print(f"- 样本索引映射已保存供验证")

        
    except FileNotFoundError as e:
        logging.error(f"错误: 文件加载失败。请确保之前已成功运行并保存了文件。错误信息: {e}")
        return None, None, None, None
    
    return final_model, X_final, y, sample_names, output_path

##############################################

from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.model_selection import train_test_split

def create_custom_waterfall_plot(shap_values, base_value, feature_values, feature_names, 
                                 sample_index=0, sample_name=None, max_display=20, specific_features=None,
                                 save_path=None, title_suffix=""):
    """
    创建自定义的SHAP waterfall plot
    
    参数:
    - shap_values: SHAP值数组
    - base_value: 基础值
    - feature_values: 特征值
    - feature_names: 特征名称列表
    - sample_index: 样本索引
    - sample_name: 样本名称(可选)
    - max_display: 最大显示特征数量
    - specific_features: 指定要显示的特征名称列表(可选)
    - save_path: 保存路径
    - title_suffix: 标题后缀
    """
    
    if specific_features is not None:
        # 如果指定了特定特征，只显示这些特征
        feature_indices = [i for i, name in enumerate(feature_names) if name in specific_features]
        selected_shap_values = shap_values[feature_indices]
        selected_feature_values = feature_values.iloc[feature_indices] if hasattr(feature_values, 'iloc') else feature_values[feature_indices]
        selected_feature_names = [feature_names[i] for i in feature_indices]
        
        plt.figure(figsize=(12, max(5, len(selected_feature_names) * 0.25)))  # 进一步调整高度使图表更紧凑
        # 使用更兼容的参数来提高PDF渲染兼容性
        shap.waterfall_plot(shap.Explanation(values=selected_shap_values, 
                                           base_values=base_value, 
                                           data=selected_feature_values, 
                                           feature_names=selected_feature_names),
                          max_display=len(selected_feature_names), show=False)
        # 显示样本名称（如果提供）
        sample_display = f"{sample_name} (Index: {sample_index})" if sample_name else f"Sample {sample_index}"
        plt.title(f"Waterfall Plot for {sample_display} - Custom Selected Features {title_suffix}")
    else:
        # 使用max_display参数
        plt.figure(figsize=(12, max(5, max_display * 0.25)))  # 进一步调整高度使图表更紧凑
        # 使用更兼容的参数来提高PDF渲染兼容性
        shap.waterfall_plot(shap.Explanation(values=shap_values, 
                                           base_values=base_value, 
                                           data=feature_values, 
                                           feature_names=feature_names),
                          max_display=max_display, show=False)
        # 显示样本名称（如果提供）
        sample_display = f"{sample_name} (Index: {sample_index})" if sample_name else f"Sample {sample_index}"
        plt.title(f"Waterfall Plot for {sample_display} - Top {max_display} Features {title_suffix}")
    
    plt.grid(False)
    plt.tight_layout()
    
    if save_path:
        # 使用更兼容的PDF渲染参数
        plt.savefig(save_path, dpi=300, bbox_inches='tight', 
                   facecolor='white', edgecolor='none', 
                   transparent=False)
        print(f"Waterfall plot已保存到: {save_path}")
    
    plt.show()


def _create_shap_waterfall_plot(shap_values, base_value, feature_values, feature_names, 
                                sample_name=None, sample_index=0, max_display=20, 
                                save_path=None, title_suffix="", shap_threshold=0.0, shap_threshold_mode='absolute'):
    """
    使用原始SHAP库创建waterfall图(增强版)

    变更说明:
    - 新增参数 shap_threshold_mode, 可选值 'absolute'(按 |SHAP|), 'positive'(只保留 SHAP > threshold), 'negative'(只保留 SHAP < -threshold)
    - 当设置阈值且存在不满足阈值的特征时, 会过滤掉这些特征
    - 支持 pandas.Series 或 ndarray 的 feature_values 输入
    """
    shap_vals = np.array(shap_values)

    # 处理 feature_values 为 numpy array
    if hasattr(feature_values, 'iloc'):
        feat_vals_arr = np.array(feature_values)
    else:
        feat_vals_arr = np.array(feature_values)

    # 如果设置了阈值，则根据模式过滤
    if shap_threshold > 0:
        if shap_threshold_mode == 'absolute':
            significant_indices = np.where(np.abs(shap_vals) > shap_threshold)[0]
        elif shap_threshold_mode == 'positive':
            significant_indices = np.where(shap_vals > shap_threshold)[0]
        elif shap_threshold_mode == 'negative':
            significant_indices = np.where(shap_vals < -shap_threshold)[0]
        else:
            raise ValueError(f"未知的shap_threshold_mode: {shap_threshold_mode}")

        if len(significant_indices) == 0:
            print(f"警告: 没有特征满足阈值条件 (mode={shap_threshold_mode}, threshold={shap_threshold})，将显示所有特征")
            filtered_shap_values = shap_vals
            filtered_feature_values = feat_vals_arr
            filtered_feature_names = feature_names
        else:
            significant_shap_values = shap_vals[significant_indices]
            significant_feature_values = feat_vals_arr[significant_indices]
            significant_feature_names = [feature_names[i] for i in significant_indices]

            # 排序：根据模式选择排序方式
            if shap_threshold_mode == 'positive':
                sorted_order = np.argsort(significant_shap_values)[::-1]  # 从大到小
            elif shap_threshold_mode == 'negative':
                sorted_order = np.argsort(significant_shap_values)  # 从小到大（更负的在前）
            else:
                sorted_order = np.argsort(np.abs(significant_shap_values))[::-1]

            sorted_significant_shap_values = significant_shap_values[sorted_order]
            sorted_significant_feature_values = significant_feature_values[sorted_order]
            sorted_significant_feature_names = [significant_feature_names[i] for i in sorted_order]

            filtered_shap_values = sorted_significant_shap_values
            filtered_feature_values = sorted_significant_feature_values
            filtered_feature_names = sorted_significant_feature_names
    else:
        # 不过滤，按绝对值排序显示
        sorted_indices = np.argsort(np.abs(shap_vals))[::-1]
        filtered_shap_values = shap_vals[sorted_indices]
        filtered_feature_values = feat_vals_arr[sorted_indices]
        filtered_feature_names = [feature_names[i] for i in sorted_indices]

    # 限制显示数量
    display_count = min(max_display, len(filtered_shap_values))

    plt.figure(figsize=(14, max(6, display_count * 0.3)))

    explanation = shap.Explanation(
        values=filtered_shap_values[:display_count],
        base_values=base_value,
        data=filtered_feature_values[:display_count],
        feature_names=filtered_feature_names[:display_count]
    )

    shap.waterfall_plot(explanation, max_display=display_count, show=False)

    if sample_name:
        plt.title(f'Waterfall Plot for {sample_name} - Top Features {title_suffix}')
    else:
        plt.title(f'Waterfall Plot - Top Features')

    plt.grid(False)
    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight', facecolor='white', edgecolor='none', transparent=False)
        print(f"Waterfall图已保存到: {save_path}")

def run_post_analysis(final_model, X_final, y, sample_names, output_path, n_select_Feature=50):
    """
    绘制RFE图和实际值vs预测值图，并进行更全面的SHAP解释
    """
    print("\n--- 步骤3: 结果可视化与SHAP解释 ---")
    
    # 创建SHAP绘图的子文件夹
    shap_plots_dir = os.path.join(output_path, 'plots')
    os.makedirs(os.path.join(shap_plots_dir, 'shap_dependence'), exist_ok=True)
    os.makedirs(os.path.join(shap_plots_dir, 'shap_scatter'), exist_ok=True)
    os.makedirs(os.path.join(shap_plots_dir, 'shap_local'), exist_ok=True)
    csvs_path = os.path.join(output_path, 'csv_paths')

    logging.info("SHAP绘图子目录已创建。")
    print("X_final",X_final.shape)
    print("y",y.shape)

    # --- 导出 XGBoost 特征重要性表格 ---
    print("\n导出 XGBoost 特征重要性表格...")
    try:
        feature_names = X_final.columns.tolist()
        importance_scores = final_model.feature_importances_

        importance_df = pd.DataFrame({
            'Feature': feature_names,
            'Importance_Score': importance_scores
        }).sort_values('Importance_Score', ascending=False) # 按重要性降序排序

        output_csv_path = os.path.join(csvs_path, 'xgboost_feature_importance.csv')
        #importance_df.to_csv(output_csv_path, index=False)
        #logging.info(f"XGBoost特征重要性表格已保存至: {output_csv_path}")

    except Exception as e:
        logging.error(f"XGBoost特征重要性表格导出失败: {e}")
    
    # 统一设置绘图样式
    plt.style.use('seaborn-v0_8-white')
    
    try:
        y_final_pred = final_model.predict(X_final)
        predictions_df = pd.DataFrame({
            'sample_name': sample_names,
            'actual_age': y.values,
            'predicted_age': y_final_pred
        })
        predictions_df.to_csv(os.path.join(csvs_path, 'sample_predictions.csv'), index=False)        
        final_r2 = r2_score(y, y_final_pred)
        print("y_final_pred",y_final_pred.shape)

        # 1. 划分训练集和测试集
        X_train, X_test, y_train, y_test = train_test_split(X_final, y, test_size=0.2, random_state=42)

        # 2. 预测
        y_train_pred = final_model.predict(X_train)
        y_test_pred = final_model.predict(X_test)

        # 3. 计算指标
        r2 = r2_score(y_test, y_test_pred)
        print("r2",r2)
        mae = mean_absolute_error(y_test, y_test_pred)
        print("mae",mae)
        mse = mean_squared_error(y_test, y_test_pred)
        rmse = mse ** 0.5  # 手动开平方得到 RMSE
        print("rmse",rmse)
        # 重置索引以确保所有数据的索引对齐
        y_train_reset = y_train.reset_index(drop=True)
        y_test_reset = y_test.reset_index(drop=True)
        y_train_pred_reset = pd.Series(y_train_pred).reset_index(drop=True)
        y_test_pred_reset = pd.Series(y_test_pred).reset_index(drop=True)
        # 同时重置y的索引
        y_reset = y.reset_index(drop=True)

        # 4. 绘图
        plt.figure(figsize=(10, 8))
        plt.scatter(y_train_reset, y_train_pred_reset, s=3, alpha=1, color='blue', label='Train')
        plt.scatter(y_test_reset, y_test_pred_reset, s=3, alpha=1, color='red', label='Test')
        # 修复：使用y_test_reset的最小值和最大值来绘制理想线
        y_test_min, y_test_max = y_test_reset.min(), y_test_reset.max()
        plt.plot([y_test_min, y_test_max], [y_test_min, y_test_max], color='red', linestyle='--', linewidth=2, label='Ideal Line (y = x)')

        # 5. 添加文本注释
        plt.text(0.05, 0.95, f"R²: {r2:.2f}\nRMSE: {rmse:.2f}\nMAE: {mae:.2f}",
                transform=plt.gca().transAxes,
                fontsize=12,
                verticalalignment='top',
                bbox=dict(boxstyle="round,pad=0.3", edgecolor='gray', facecolor='white'))

        # 6. 图像美化
        plt.xlabel('Actual Values')
        plt.ylabel('Predicted Values')
        plt.title('Actual vs Predicted Values (Train & Test)')
        plt.legend()
        plt.tight_layout()
        plt.savefig(os.path.join(shap_plots_dir, 'y_pred_vs_y_test.pdf'), dpi=300)
        plt.show()
        logging.info("实际值vs预测值散点图已生成。")
        
        # 新增：在同一坐标系中绘制训练集和测试集的预测值vs实际值，并分别标注R²和RMSE
        plt.figure(figsize=(10, 8))
        
        # 计算训练集的指标
        train_r2 = r2_score(y_train_reset, y_train_pred_reset)
        train_mse = mean_squared_error(y_train_reset, y_train_pred_reset)
        train_rmse = train_mse ** 0.5
        train_mae = mean_absolute_error(y_train_reset, y_train_pred_reset)
        
        # 绘制训练集和测试集
        plt.scatter(y_train_reset, y_train_pred_reset, s=3, alpha=1, color='blue', label=f'Train (R²={train_r2:.2f}, RMSE={train_rmse:.2f}, MAE={train_mae:.2f})')
        plt.scatter(y_test_reset, y_test_pred_reset, s=3, alpha=1, color='red', label=f'Test (R²={r2:.2f}, RMSE={rmse:.2f}, MAE={mae:.2f})')
        # 修复：使用y_reset的最小值和最大值来绘制理想线
        y_min, y_max = y_reset.min(), y_reset.max()
        plt.plot([y_min, y_max], [y_min, y_max], 'k--', lw=2, label='Ideal Line (y=x)')
        
        # 图表美化
        plt.xlabel('Actual Values')
        plt.ylabel('Predicted Values')
        plt.title('Actual vs Predicted Values for Train and Test Sets')
        plt.legend()
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(os.path.join(shap_plots_dir, 'y_pred_vs_y_actual_combined.pdf'), dpi=300)
        plt.show()
        logging.info("训练集和测试集实际值vs预测值散点图已生成。")
        
        # 新增：残差分析图
        # 计算残差
        train_residuals = y_train_reset - y_train_pred_reset
        test_residuals = y_test_reset - y_test_pred_reset
        
        plt.figure(figsize=(12, 5))
        
        # 左图：训练集残差分布
        plt.subplot(1, 2, 1)
        plt.scatter(y_train_pred_reset, train_residuals, s=3, alpha=1, color='blue')
        plt.axhline(y=0, color='red', linestyle='--')
        plt.xlabel('Predicted Values')
        plt.ylabel('Residuals')
        plt.title('Residual Plot - Train Set')
        plt.grid(True, alpha=0.3)
        
        # 右图：测试集残差分布
        plt.subplot(1, 2, 2)
        plt.scatter(y_test_pred_reset, test_residuals, s=3, alpha=1, color='red')
        plt.axhline(y=0, color='red', linestyle='--')
        plt.xlabel('Predicted Values')
        plt.ylabel('Residuals')
        plt.title('Residual Plot - Test Set')
        plt.grid(True, alpha=0.3)
        
        plt.tight_layout()
        plt.savefig(os.path.join(shap_plots_dir, 'residual_analysis.pdf'), dpi=300)
        plt.show()
        logging.info("残差分析图已生成。")
        
        # 新增：残差直方图（重叠显示训练集和测试集）
        plt.figure(figsize=(10, 6))
        
        # 计算联合范围以确保使用相同的bins边界
        combined_residuals = np.concatenate([train_residuals, test_residuals])
        bins = np.histogram_bin_edges(combined_residuals, bins=30)
        
        # 绘制训练集和测试集的残差直方图（重叠显示，使用相同的bins边界）
        plt.hist(train_residuals, bins=bins, alpha=0.7, color='blue', edgecolor='black', label='Train Set')
        plt.hist(test_residuals, bins=bins, alpha=0.7, color='red', edgecolor='black', label='Test Set')
        
        plt.xlabel('Residuals')
        plt.ylabel('Frequency')
        plt.title('Distribution of Residuals (Train and Test Sets)')
        plt.legend()
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(os.path.join(shap_plots_dir, 'residual_histogram.pdf'), dpi=300)
        plt.show()
        logging.info("残差直方图已生成。")

        # 绘制最终模型在全部数据上的实际值 vs 预测值散点图
        plt.figure(figsize=(10, 8))
        # 重置索引以确保y和y_final_pred的索引匹配
        y_final_pred_reset = pd.Series(y_final_pred).reset_index(drop=True)
        plt.scatter(y_reset, y_final_pred_reset, alpha=0.4, label='Predicted vs Actual', color='blue')
        plt.plot([y_reset.min(), y_reset.max()], [y_reset.min(), y_reset.max()], color='red', linestyle='--', linewidth=2, label='Ideal Line (y = x)')
        plt.xlabel('Actual Values')
        plt.ylabel('Predicted Values')
        plt.title(f'Actual vs Predicted Values\nR-squared: {final_r2:.2f}')
        plt.grid(False) # 确保没有网格线
        plt.legend()
        plt.tight_layout()
        plt.savefig(os.path.join(shap_plots_dir, 'old_y_pred_vs_y_test.pdf'), dpi=300)
        plt.show()
    except Exception as e:
        logging.warning(f"实际值vs预测值散点图绘制失败: {e}")
    
    print("\n生成XGBoost模型自带的特征重要性图...") 
    try: 
        feature_names = X_final.columns.tolist() 
        importance_scores = final_model.feature_importances_ 
        
        importance_df = pd.DataFrame({ 
            'Feature': feature_names, # 将 'feature' 改为 'Feature'
            'Importance_Score': importance_scores # 将 'importance' 改为 'Importance_Score'
        }).sort_values('Importance_Score', ascending=False)

        plt.figure(figsize=(10, 12)) 
        sns.barplot(x='Importance_Score', y='Feature', data=importance_df, palette='viridis') 
        plt.title("XGBoost Built-in Feature Importance", fontsize=16) 
        plt.xlabel("Importance Score", fontsize=12) 
        plt.ylabel("Features", fontsize=12) 
        plt.grid(False) # 确保没有网格线
        plt.tight_layout() 
        plt.savefig(os.path.join(shap_plots_dir, 'xgboost_feature_importance.pdf'), dpi=300) 
        plt.show() 

        logging.info("XGBoost自带特征重要性图已生成。")
        
        # 同时修改前n_select_Feature个特征的排序
        top_n_select_Feature_importance_df = importance_df.head(n_select_Feature)
        plt.figure(figsize=(10, 10))
        sns.barplot(x='Importance_Score', y='Feature', data=top_n_select_Feature_importance_df, palette='viridis')
        plt.title(f"XGBoost Built-in Feature Importance (Top {n_select_Feature})", fontsize=16)
        plt.xlabel("Importance Score", fontsize=12)
        plt.ylabel("Features", fontsize=12)
        plt.grid(False) # 确保没有网格线
        plt.tight_layout()
        plt.savefig(os.path.join(shap_plots_dir, f'xgboost_feature_importance_top{n_select_Feature}.pdf'), dpi=300)
        plt.show()
        
    except Exception as e:
        logging.error(f"XGBoost特征重要性图绘制失败: {e}")

    logging.info("开始生成SHAP解释...")
    try:
        sample_data = X_final
        
        sample_data = sample_data.loc[:, ~sample_data.columns.duplicated()]

        explainer = shap.TreeExplainer(final_model)
        shap_values = explainer.shap_values(sample_data)
        
        shap_explanation = shap.Explanation(
            values=shap_values,
            base_values=explainer.expected_value,
            data=sample_data.values,
            feature_names=sample_data.columns.tolist()
        )
        
        np.save(os.path.join(output_path, 'shap_values', 'shap_values.npy'), shap_values)
        logging.info("SHAP值已计算并保存。")

        # --- 导出 SHAP 全局重要性表格 ---
        print("\n导出 SHAP 全局重要性表格...")
        feature_importance_shap = np.mean(np.abs(shap_values), axis=0)
        shap_importance_df = pd.DataFrame({
            'Feature': sample_data.columns.tolist(),
            'Mean_Absolute_SHAP_Value': feature_importance_shap
        }).sort_values('Mean_Absolute_SHAP_Value', ascending=False) # 按SHAP值降序排序

        output_csv_path_shap = os.path.join(csvs_path, 'shap_global_feature_importance.csv')
        shap_importance_df.to_csv(output_csv_path_shap, index=False)
        logging.info(f"SHAP全局重要性表格已保存至: {output_csv_path_shap}")

        # --- 导出 SHAP 局部重要性表格 (针对所有样本) ---
        print("\n导出 SHAP 局部重要性表格 (针对所有样本)...")
        shap_df = pd.DataFrame(data=shap_values, columns=sample_data.columns)
        output_csv_path_shap_local = os.path.join(csvs_path, 'shap_local_values_all_samples.csv')
        shap_df.to_csv(output_csv_path_shap_local, index=False)
        logging.info(f"SHAP局部重要性表格已保存至: {output_csv_path_shap_local}")
        print(f"\n注意：CSV文件包含所有{shap_df.shape[0]}个样本和{shap_df.shape[1]}个特征的SHAP值")
        print(f"行索引对应样本编号，列名对应特征名称")

        # --- 绘制 Mean SHAP value vs. Mean XGBoost Gain 散点图 ---
        print("\n生成 Mean SHAP value vs. Mean XGBoost Gain 散点图...")
        try:
            combined_df = pd.merge(
                importance_df, 
                shap_importance_df, 
                left_on='Feature', 
                right_on='Feature', 
                how='inner'
            )
            
            plt.figure(figsize=(12, 10))
            ax = plt.gca()
            
            # 使用for循环绘制每个点，并添加文本标签
            for i, row in combined_df.iterrows():
                ax.scatter(row['Importance_Score'], row['Mean_Absolute_SHAP_Value'], s=50, c='black', alpha=0.7)
                if row['Importance_Score'] > 0.02 or row['Mean_Absolute_SHAP_Value'] > 1.5:
                    plt.text(row['Importance_Score'] * 1.05, row['Mean_Absolute_SHAP_Value'],
                             row['Feature'], fontsize=9)

            top_feature = combined_df.sort_values('Mean_Absolute_SHAP_Value', ascending=False).iloc[0]
            ax.scatter(top_feature['Importance_Score'], top_feature['Mean_Absolute_SHAP_Value'],
                       s=150, c='red', alpha=0.9, marker='o')
            plt.axhline(y=top_feature['Mean_Absolute_SHAP_Value'], color='red', linestyle='--', linewidth=1)
            plt.axvline(x=top_feature['Importance_Score'], color='red', linestyle='--', linewidth=1)
            plt.text(top_feature['Importance_Score'] * 1.05, top_feature['Mean_Absolute_SHAP_Value'] * 1.05,
                     f"{top_feature['Feature']}", fontsize=10, color='red', weight='bold')

            plt.xlabel("Mean XGBoost Gain", fontsize=12)
            plt.ylabel("Mean |SHAP| value", fontsize=12)
            plt.title("Mean SHAP value vs. Mean XGBoost Gain", fontsize=16)
            plt.grid(False)
            plt.tight_layout()
            
            scatter_plot_path = os.path.join(shap_plots_dir, 'shap_vs_xgboost_gain_scatter.pdf')
            plt.savefig(scatter_plot_path, dpi=300)
            plt.show()
            logging.info(f"Mean SHAP value vs. Mean XGBoost Gain 散点图已保存至: {scatter_plot_path}")

        except Exception as e:
            logging.error(f"生成Mean SHAP vs XGBoost Gain 散点图失败: {e}")

        # 计算并排序全局特征重要性
        feature_importance = np.sum(np.abs(shap_values), axis=0)
        sorted_features_indices = np.argsort(feature_importance)[::-1]

        #sys.exit("程序暂停")  # 暂停程序
        
        # === 新增：方案1 年龄范围样本聚合分析 ===
        print("\n" + "=" * 80)
        print("开始执行年龄特异性SHAP分析...")
        print("=" * 80)
        
        # 调用年龄范围样本聚合分析
        try:
            age_analysis_dir, aggregation_results_dir = analyze_age_group_shap_aggregation(
                explainer=explainer,
                shap_values=shap_values, 
                sample_data=sample_data,
                sample_names=sample_names,
                ages=y,  # 使用年龄作为分组依据
                output_path=output_path,
                n_select_Feature=n_select_Feature
            )
            logging.info(f"年龄范围样本聚合分析完成，结果保存在: {age_analysis_dir}")
        except Exception as e:
            logging.error(f"年龄范围样本聚合分析失败: {e}")
            print(f"警告：年龄聚合分析遇到错误: {e}")
        
        # === 原有的样本筛选逻辑继续执行 ===
        
        # 检查是否存在样本筛选文件
        sample_select_file = "select_sample.txt"
        has_sample_select = os.path.exists(sample_select_file)
        
        if has_sample_select:
            print(f"\n发现 {sample_select_file} 文件，将根据指定样本批量生成SHAP waterfall图...")
            
            # 读取指定的样本名称
            with open(sample_select_file, "r") as f:
                selected_samples = [line.strip() for line in f.readlines() if line.strip()]
            print(f"从 {sample_select_file} 中读取到 {len(selected_samples)} 个指定样本")
            print(f"指定样本: {selected_samples}")
            
            # 找到对应的样本索引
            selected_sample_indices = []
            found_samples = []
            for sample_name in selected_samples:
                if sample_name in sample_names:
                    sample_idx = sample_names.index(sample_name)
                    selected_sample_indices.append(sample_idx)
                    found_samples.append(sample_name)
                else:
                    print(f"警告：样本 '{sample_name}' 在数据中未找到")
            
            print(f"在数据中找到 {len(found_samples)} 个指定样本用于生成waterfall图")
            
            if not selected_sample_indices:
                print("未找到任何匹配的样本，无法生成图表。")
            else:
                # 批量生成指定样本的waterfall图
                generate_selected_samples_waterfall(explainer, shap_values, sample_data, 
                                                   selected_sample_indices, found_samples, 
                                                   shap_plots_dir, n_select_Feature)
        
        
        # 检查select.txt文件
        # 检查是否存在select1.txt和select2.txt文件
        if os.path.exists("select1.txt") and os.path.exists("select2.txt"):
            print("\n发现 select1.txt 和 select2.txt 文件，将根据其内容生成SHAP图表...")
            
            # 读取select1.txt中的特征
            with open("select1.txt", "r") as f:
                lines1 = f.readlines()
            selected_asv_names1 = [line.strip() for line in lines1 if line.strip()]
            print(f"从 select1.txt 中解析出的ASV名称: {selected_asv_names1}")
            
            # 读取select2.txt中的特征
            with open("select2.txt", "r") as f:
                lines2 = f.readlines()
            selected_asv_names2 = [line.strip() for line in lines2 if line.strip()]
            print(f"从 select2.txt 中解析出的ASV名称: {selected_asv_names2}")
            
            # 在数据集中查找匹配的特征
            found_features1 = []
            for asv_name in selected_asv_names1:
                if asv_name in sample_data.columns:
                    found_features1.append(asv_name)
                    
            found_features2 = []
            for asv_name in selected_asv_names2:
                if asv_name in sample_data.columns:
                    found_features2.append(asv_name)
            
            print(f"在数据集中找到 select1.txt 中的 {len(found_features1)} 个特征")
            print(f"在数据集中找到 select2.txt 中的 {len(found_features2)} 个特征")
            
            if not found_features1 or not found_features2:
                print("未找到匹配的特征，无法生成图表。请检查名称格式。")
            else:
                # 创建一个新的 Explanation 对象用于绘图
                all_found_features = list(set(found_features1 + found_features2))  # 合并去重
                feature_indices_to_plot = [sample_data.columns.get_loc(f) for f in all_found_features]
                
                filtered_shap_values = shap_values[:, feature_indices_to_plot]
                filtered_sample_data = sample_data.iloc[:, feature_indices_to_plot]
                
                filtered_explanation = shap.Explanation(
                    values=filtered_shap_values,
                    base_values=explainer.expected_value,
                    data=filtered_sample_data.values,
                    feature_names=filtered_sample_data.columns.tolist()        
                )
                
                print("最终用于绘图的特征顺序：")
                print(filtered_explanation.feature_names)

                # --- SHAP值热力图 ---
                print("\n生成筛选后特征的SHAP值热力图...")
                plt.figure(figsize=(10, 15))
                shap.plots.heatmap(filtered_explanation, show=False)
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_heatmap_selected.pdf'), dpi=300)
                plt.show()
                
                # --- 生成筛选后的特征图表 ---
                print("\n生成全局特征重要性图表 (筛选后的特征)...")
                plt.figure(figsize=(10, 8))
                shap.summary_plot(filtered_explanation, plot_type="bar", max_display=398, show=False)
                plt.title("SHAP Bar Plot (Selected Features)")
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_bar_plot_selected.pdf'), dpi=300)
                plt.show()

                plt.figure(figsize=(12, 8))
                shap.summary_plot(filtered_explanation, plot_type="dot", max_display=398, show=False)
                plt.title("SHAP Beeswarm Plot (Selected Features)")
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_beeswarm_plot_selected.pdf'), dpi=300)
                plt.show()
                
                # --- 生成select1.txt和select2.txt中特征的交叉组合依赖图 ---
                print("\n生成select1.txt和select2.txt中特征的交叉组合依赖图...")
                # 生成交叉组合（select1中的每个特征与select2中的每个特征组合）
                feature_pairs = [(f1, f2) for f1 in found_features1 for f2 in found_features2 if f1 != f2]
                print(f"将生成 {len(feature_pairs)} 个特征对的依赖图...")
                
                for i, (feature1, feature2) in enumerate(feature_pairs):
                    try:
                        plt.figure(figsize=(10, 6))
                        # 使用interaction_index参数指定第二个特征
                        shap.dependence_plot(ind=feature1, shap_values=shap_values, features=sample_data, 
                                        interaction_index=feature2, show=False)
                        plt.title(f"SHAP Dependence Plot for {feature1} vs {feature2}")
                        plt.grid(False)
                        plt.tight_layout()
                        # 创建安全的文件名
                        filename = f"shap_dependence_plot_{feature1}_vs_{feature2}.pdf"
                        # 清理文件名中的特殊字符
                        filename = filename.replace("(", "").replace(")", "").replace(" ", "_")
                        plt.savefig(os.path.join(shap_plots_dir, 'shap_dependence', filename), dpi=300)
                        plt.close()
                        print(f"已生成 {i+1}/{len(feature_pairs)}: {filename}")
                    except Exception as e:
                        print(f"生成 {feature1} vs {feature2} 的依赖图时出错: {e}")
                        plt.close()
                
                # --- 特征贡献散点图 ---
                print("\n生成筛选后特征的贡献散点图...")
                for feature in all_found_features:
                    plt.figure(figsize=(10, 6))
                    shap.plots.scatter(filtered_explanation[:, feature], show=False)
                    plt.title(f"SHAP Scatter Plot for {feature}")
                    plt.grid(False)
                    plt.tight_layout()
                    plt.savefig(os.path.join(shap_plots_dir, 'shap_scatter', f'shap_scatter_plot_{feature}.pdf'), dpi=300)
                    plt.close()
                
                # --- 局部解释图表 (针对单个样本) ---
                print("\n生成筛选后特征的局部解释图表 (针对单个样本)...")
                sample_index = 0
                
                # 修正：先计算完整样本的SHAP值，再进行切片
                sample_to_explain = sample_data.iloc[[sample_index]]
                shap_values_sample = explainer.shap_values(sample_to_explain)
                
                # 从完整SHAP值中筛选出我们想要的特征
                top_shap_values_sample = shap_values_sample[0][feature_indices_to_plot]
                top_sample_to_explain_data = sample_to_explain.iloc[0][all_found_features]

                plt.figure(figsize=(10, 6))
                shap.waterfall_plot(shap.Explanation(values=top_shap_values_sample, 
                                                     base_values=explainer.expected_value, 
                                                     data=top_sample_to_explain_data, 
                                                     feature_names=all_found_features),
                                    show=False)
                plt.title(f"Waterfall Plot for Sample {sample_index} (Selected Features)")
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_local', f'shap_waterfall_plot_sample_{sample_index}_selected.pdf'), dpi=300)
                plt.close()

                shap.initjs()
                html_file_path_single = os.path.join(shap_plots_dir, 'shap_local', f'shap_force_plot_sample_{sample_index}_selected.html')
                shap.save_html(html_file_path_single, shap.force_plot(explainer.expected_value, top_shap_values_sample, top_sample_to_explain_data, show=False))
                logging.info(f"SHAP Force Plot (单个预测, 筛选后) 已保存为 '{html_file_path_single}' (HTML)")
                
                shap.decision_plot(explainer.expected_value, filtered_shap_values, filtered_sample_data, show=False)
                plt.title("SHAP Decision Plot (Global View of Local Explanations)")
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_decision_plot_selected.pdf'), dpi=300)
                plt.show()

                print("\n生成多样本汇总图表...")
                shap.initjs()
                html_file_path_multi = os.path.join(shap_plots_dir, 'shap_force_plot_all_samples_selected.html')
                shap.save_html(html_file_path_multi, shap.force_plot(explainer.expected_value, filtered_shap_values, filtered_sample_data, show=False))
                logging.info(f"SHAP Force Plot (所有样本汇总, 筛选后) 已保存为 '{html_file_path_multi}' (HTML)")

        elif os.path.exists("select.txt"):  # 保持原有的select.txt逻辑
            print("\n发现 select.txt 文件，将根据其内容生成SHAP图表...")
            
            # 读取并筛选特征
            select_file_path = "select.txt"
            import re
            
            with open(select_file_path, "r") as f:
                lines = f.readlines()

            selected_asv_names = [line.strip() for line in lines if line.strip()]
            print(f"从 select.txt 中解析出的ASV名称: {selected_asv_names}")
            
            found_features = []
            for asv_name in selected_asv_names:
                if asv_name in sample_data.columns:
                    found_features.append(asv_name)

            # 获取对应的列索引
            feature_indices_to_plot = [sample_data.columns.get_loc(f) for f in found_features]

            
            print(f"在数据集中找到并筛选出的 {len(feature_indices_to_plot)} 个特征进行绘图。")
            
            if not feature_indices_to_plot:
                print("未找到匹配的特征，无法生成图表。请检查名称格式。")
            else:
                # 创建一个新的 Explanation 对象用于绘图
                filtered_shap_values = shap_values[:, feature_indices_to_plot]
                filtered_sample_data = sample_data.iloc[:, feature_indices_to_plot]
                 
                filtered_explanation = shap.Explanation(
                    values=filtered_shap_values,
                    base_values=explainer.expected_value,
                    data=filtered_sample_data.values,
                    feature_names=filtered_sample_data.columns.tolist()        
                )
                
                print("最终用于绘图的特征顺序：")
                print(filtered_explanation.feature_names)

                # --- SHAP值热力图 ---
                print("\n生成筛选后特征的SHAP值热力图...")
                plt.figure(figsize=(10, 15))
                shap.plots.heatmap(filtered_explanation, show=False)
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_heatmap_selected.pdf'), dpi=300)
                plt.show()
                
                # --- 生成筛选后的特征图表 ---
                print(f"\n生成全局特征重要性图表 (筛选后的特征)...")
                plt.figure(figsize=(10, 8))
                shap.summary_plot(filtered_explanation, plot_type="bar", max_display=n_select_Feature, show=False)
                plt.title("SHAP Bar Plot (Selected Features)")
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_bar_plot_selected.pdf'), dpi=300)
                plt.show()

                plt.figure(figsize=(12, 8))
                shap.summary_plot(filtered_explanation, plot_type="dot", max_display=n_select_Feature, show=False)
                plt.title("SHAP Beeswarm Plot (Selected Features)")
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_beeswarm_plot_selected.pdf'), dpi=300)
                plt.show()
                
                # --- 局部解释图表（针对前{n_select_Feature}个） ---
                print(f"\n生成前{n_select_Feature}个特征的依赖图...")
                for feature in found_features:
                    plt.figure(figsize=(10, 6))
                    shap.dependence_plot(ind=feature, shap_values=shap_values, features=sample_data, show=False)
                    plt.title(f"SHAP Dependence Plot for {feature}")
                    plt.grid(False)
                    plt.tight_layout()
                    plt.savefig(os.path.join(shap_plots_dir, 'shap_dependence', f'shap_dependence_plot_{feature}.pdf'), dpi=300)
                    plt.close()
                
                # 新增：为select.txt中指定的特征生成两两组合的dependence plots
                print("\n生成指定特征的两两组合依赖图...")
                import itertools
                # 生成所有两两组合
                feature_pairs = list(itertools.combinations(found_features, 2))
                print(f"将生成 {len(feature_pairs)} 个特征对的依赖图...")
                
                for i, (feature1, feature2) in enumerate(feature_pairs):
                    try:
                        plt.figure(figsize=(10, 6))
                        # 使用interaction_index参数指定第二个特征
                        shap.dependence_plot(ind=feature1, shap_values=shap_values, features=sample_data, 
                                           interaction_index=feature2, show=False)
                        plt.title(f"SHAP Dependence Plot for {feature1} vs {feature2}")
                        plt.grid(False)
                        plt.tight_layout()
                        # 创建安全的文件名
                        filename = f"shap_dependence_plot_{feature1}_vs_{feature2}.pdf"
                        # 清理文件名中的特殊字符
                        filename = filename.replace("(", "").replace(")", "").replace(" ", "_")
                        plt.savefig(os.path.join(shap_plots_dir, 'shap_dependence', filename), dpi=300)
                        plt.close()
                        print(f"已生成 {i+1}/{len(feature_pairs)}: {filename}")
                    except Exception as e:
                        print(f"生成 {feature1} vs {feature2} 的依赖图时出错: {e}")
                        plt.close()
                
                # --- 特征贡献散点图 ---
                print("\n生成筛选后特征的贡献散点图...")
                for feature in found_features:
                    plt.figure(figsize=(10, 6))
                    shap.plots.scatter(filtered_explanation[:, feature], show=False)
                    plt.title(f"SHAP Scatter Plot for {feature}")
                    plt.grid(False)
                    plt.tight_layout()
                    plt.savefig(os.path.join(shap_plots_dir, 'shap_scatter', f'shap_scatter_plot_{feature}.pdf'), dpi=300)
                    plt.close()
                
                # --- 局部解释图表 (针对单个样本) - 增强版 ---
                print("\n生成筛选后特征的局部解释图表 (针对单个样本)...")
                sample_index = 0
                
                # 使用已计算的完整SHAP值，确保与CSV文件一致
                sample_index = 0
                sample_to_explain = sample_data.iloc[[sample_index]]
                
                # 直接从已计算的shap_values中获取对应样本和特征的值
                top_shap_values_sample = shap_values[sample_index][feature_indices_to_plot]
                top_sample_to_explain_data = sample_to_explain.iloc[0][found_features]

                # 简化版：只显示前n_select_Feature个特征的waterfall图
                print(f"生成前{n_select_Feature}个特征的waterfall图，样本: {sample_names[sample_index]}")
                create_custom_waterfall_plot(
                    shap_values=top_shap_values_sample,
                    base_value=explainer.expected_value,
                    feature_values=top_sample_to_explain_data,
                    feature_names=top_50_feature_names,
                    sample_index=sample_index,
                    sample_name=sample_names[sample_index],
                    max_display=n_select_Feature,
                    save_path=os.path.join(shap_plots_dir, 'shap_local', f'shap_waterfall_plot_sample_{sample_index}_top{n_select_Feature}_features.pdf'),
                    title_suffix=f"(Top {n_select_Feature} Features)"
                )

                # 简化版：只生成一个包含所有选中特征的waterfall图
                print("生成包含所有选中特征的 waterfall plot...")
                # 显示所有可用样本信息
                print(f"\n可用样本信息 (共{len(sample_names)}个):")
                for i in range(min(10, len(sample_names))):
                    print(f"  索引 {i}: {sample_names[i]}")
                if len(sample_names) > 10:
                    print(f"  ... 还有 {len(sample_names)-10} 个样本")
                print(f"\n当前分析的样本: 索引 {sample_index} -> {sample_names[sample_index]}")
                print("提示: 要分析其他样本，请修改代码中的 sample_index 值")
                print(f"waterfall图中的SHAP值来源: 与CSV文件第{sample_index+1}行对应")
                create_custom_waterfall_plot(
                    shap_values=top_shap_values_sample,
                    base_value=explainer.expected_value,
                    feature_values=top_sample_to_explain_data,
                    feature_names=found_features,
                    sample_index=sample_index,
                    sample_name=sample_names[sample_index],
                    max_display=len(found_features),
                    save_path=os.path.join(shap_plots_dir, 'shap_local', f'shap_waterfall_plot_sample_{sample_index}_selected_all.pdf'),
                    title_suffix="(All Selected Features)"
                )

                shap.initjs()
                html_file_path_single = os.path.join(shap_plots_dir, 'shap_local', f'shap_force_plot_sample_{sample_index}_selected.html')
                shap.save_html(html_file_path_single, shap.force_plot(explainer.expected_value, top_shap_values_sample, top_sample_to_explain_data, show=False))
                logging.info(f"SHAP Force Plot (单个预测, 筛选后) 已保存为 '{html_file_path_single}' (HTML)")
                
                shap.decision_plot(explainer.expected_value, filtered_shap_values, filtered_sample_data, show=False)
                plt.title("SHAP Decision Plot (Global View of Local Explanations)")
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_decision_plot_selected.pdf'), dpi=300)
                plt.show()

                print("\n生成多样本汇总图表...")
                shap.initjs()
                html_file_path_multi = os.path.join(shap_plots_dir, 'shap_force_plot_all_samples_selected.html')
                shap.save_html(html_file_path_multi, shap.force_plot(explainer.expected_value, filtered_shap_values, filtered_sample_data, show=False))
                logging.info(f"SHAP Force Plot (所有样本汇总, 筛选后) 已保存为 '{html_file_path_multi}' (HTML)")
        
        else: # 如果没有找到 select.txt，则执行原有的逻辑
            print(f"\n未发现 select.txt 文件，将根据前{n_select_Feature}个最重要的特征生成SHAP图表...")
            
            # 计算并排序全局特征重要性
            feature_importance = np.sum(np.abs(shap_values), axis=0)
            sorted_features_indices = np.argsort(feature_importance)[::-1]
            top_50_features_indices = sorted_features_indices[:n_select_Feature]
            top_50_feature_names = [sample_data.columns[i] for i in top_50_features_indices]

            # --- 全局特征重要性图表 (前n_select_Feature个特征) ---
            print(f"\n生成全局特征重要性图表...")
            plt.figure(figsize=(10, 8))
            shap.summary_plot(shap_explanation, plot_type="bar", max_display=n_select_Feature, show=False)
            plt.title("SHAP Bar Plot (Global Feature Importance)")
            plt.grid(False)
            plt.tight_layout()
            plt.savefig(os.path.join(shap_plots_dir, 'shap_bar_plot.pdf'), dpi=300)
            plt.show()
            
            plt.figure(figsize=(12, 8))
            shap.summary_plot(shap_explanation, plot_type="dot", max_display=n_select_Feature, show=False)
            plt.title("SHAP Beeswarm Plot (Global Feature Importance and Impact)")
            plt.grid(False)
            plt.tight_layout()
            plt.savefig(os.path.join(shap_plots_dir, 'shap_beeswarm_plot.pdf'), dpi=300)
            plt.show()

            # --- SHAP 依赖图（前{n_select_Feature}个特征） ---
            print(f"\n生成前{n_select_Feature}个特征的依赖图...")
            for feature in top_50_feature_names:
                plt.figure(figsize=(10, 6))
                shap.dependence_plot(ind=feature, shap_values=shap_values, features=sample_data, show=False)
                plt.title(f"SHAP Dependence Plot for {feature}")
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_dependence', f'shap_dependence_plot_{feature}.pdf'), dpi=300)
                plt.close()

            # --- SHAP值热力图（前{n_select_Feature}个特征） ---
            print(f"\n生成前{n_select_Feature}个特征的SHAP值热力图...")
            top_shap_explanation = shap.Explanation(
                values=shap_values[:, top_50_features_indices],
                base_values=explainer.expected_value,
                data=sample_data[top_50_feature_names].values,
                feature_names=top_50_feature_names
            )
            plt.figure(figsize=(10, 15))
            shap.plots.heatmap(top_shap_explanation, show=False)
            plt.grid(False)
            plt.tight_layout()
            plt.savefig(os.path.join(shap_plots_dir, f'shap_heatmap_top{n_select_Feature}.pdf'), dpi=300)
            plt.show()

            # --- 特征贡献散点图（前{n_select_Feature}个特征） ---
            print(f"\n生成前{n_select_Feature}个特征的贡献散点图...")
            for feature in top_50_feature_names:
                plt.figure(figsize=(10, 6))
                shap.plots.scatter(shap_explanation[:, feature], show=False)
                plt.title(f"SHAP Scatter Plot for {feature}")
                plt.grid(False)
                plt.tight_layout()
                plt.savefig(os.path.join(shap_plots_dir, 'shap_scatter', f'shap_scatter_plot_{feature}.pdf'), dpi=300)
                plt.close()

            # --- 局部解释图表 (针对单个样本) ---
            print(f"\n生成前{n_select_Feature}个特征的局部解释图表 (针对单个样本)...")
            sample_index = 0  # 默认使用第一个样本，您可以修改这个值来分析不同样本
            
            # 显示当前分析的样本信息
            print(f"当前分析的样本: 索引 {sample_index} -> {sample_names[sample_index]}")
            sample_to_explain = sample_data.iloc[[sample_index]]
            
            # 直接从已计算的shap_values中获取对应样本和特征的值，确保与CSV文件一致
            top_shap_values_sample = shap_values[sample_index][top_50_features_indices]
            top_sample_to_explain_data = sample_to_explain.iloc[0][top_50_feature_names]
            
            # 简化版：只显示前{n_select_Feature}个特征的waterfall图
            print(f"生成前{n_select_Feature}个特征的waterfall图，样本: {sample_names[sample_index]}")
            create_custom_waterfall_plot(
                shap_values=top_shap_values_sample,
                base_value=explainer.expected_value,
                feature_values=top_sample_to_explain_data,
                feature_names=top_50_feature_names,
                sample_index=sample_index,
                sample_name=sample_names[sample_index],
                max_display=n_select_Feature,
                save_path=os.path.join(shap_plots_dir, 'shap_local', f'shap_waterfall_plot_sample_{sample_index}_top{n_select_Feature}_features.pdf'),
                title_suffix=f"(Top {n_select_Feature} Features)"
            )
            
            shap.initjs()
            html_file_path_single = os.path.join(shap_plots_dir, 'shap_local', f'shap_force_plot_sample_{sample_index}_top{n_select_Feature}.html')
            shap.save_html(html_file_path_single, shap.force_plot(explainer.expected_value, top_shap_values_sample, top_sample_to_explain_data, show=False))
            logging.info(f"SHAP Force Plot (单个预测, 前{n_select_Feature}) 已保存为 '{html_file_path_single}' (HTML)")
            
            shap.decision_plot(explainer.expected_value, shap_values, sample_data, show=False)
            plt.title("SHAP Decision Plot (Global View of Local Explanations)")
            plt.grid(False)
            plt.tight_layout()
            plt.savefig(os.path.join(shap_plots_dir, 'shap_decision_plot.pdf'), dpi=300)
            plt.show()

            print("\n生成多样本汇总图表...")
            shap.initjs()
            html_file_path_multi = os.path.join(shap_plots_dir, 'shap_force_plot_all_samples.html')
            shap.save_html(html_file_path_multi, shap.force_plot(explainer.expected_value, shap_values, sample_data, show=False))
            logging.info(f"SHAP Force Plot (所有样本汇总) 已保存为 '{html_file_path_multi}' (HTML)")

    except Exception as e:
        logging.error(f"SHAP解释过程中发生错误: {e}")

def analyze_age_group_shap_aggregation(explainer, shap_values, sample_data, sample_names, ages, output_path, n_select_Feature=50):
    """
    方案1: 年龄范围样本聚合分析
    
    根据年龄将样本分组, 计算每个年龄组的SHAP值中位数, 生成年龄特异性waterfall图
    
    参数:
    - explainer: SHAP解释器
    - shap_values: 所有样本的SHAP值矩阵
    - sample_data: 特征数据
    - sample_names: 样本名称列表
    - ages: 样本年龄列表
    - output_path: 输出路径
    - n_select_Feature: 选择的特征数量
    """
    print("\n" + "=" * 80)
    print("方案1：年龄范围样本聚合分析")
    print("=" * 80)
    
    # 创建年龄特异性分析输出目录
    age_analysis_dir = os.path.join(output_path, 'plots', 'age_specific_analysis')
    os.makedirs(age_analysis_dir, exist_ok=True)
    
    # 数据验证：确保年龄、样本名称、SHAP值的索引对应
    assert len(ages) == len(sample_names) == shap_values.shape[0], f"数据长度不一致: ages={len(ages)}, sample_names={len(sample_names)}, shap_values={shap_values.shape[0]}"
    
    # 创建年龄-样本数据框用于分组分析
    age_sample_df = pd.DataFrame({
        'sample_name': sample_names,
        'age': ages,
        'sample_index': range(len(sample_names))
    })
    
    print(f"年龄范围: {ages.min():.1f} - {ages.max():.1f} 岁")
    print(f"年龄中位数: {np.median(ages):.1f} 岁")
    print(f"总样本数: {len(ages)}")
    
    # 定义年龄分组策略（只保留您需要的5岁年龄组）
    age_grouping_strategies = {
        'custom_5year_groups': {
            'name': '5岁年龄组',
            'bins': [2, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53, 58, 63, 68, 73, 78, 83, 88, 92, 100],
            'labels': ['A2-7', 'A8-12', 'A13-17', 'A18-22', 'A23-27', 'A28-32', 'A33-37', 
                      'A38-42', 'A43-47', 'A48-52', 'A53-57', 'A58-62', 'A63-67', 
                      'A68-72', 'A73-77', 'A78-82', 'A83-87', 'A88-92', 'A92+']
        }
    }
    
    # 创建年龄组聚合结果保存目录
    aggregation_results_dir = os.path.join(output_path, 'csv_paths', 'age_group_aggregation')
    os.makedirs(aggregation_results_dir, exist_ok=True)
    
    # 对每种分组策略进行分析
    for strategy_name, strategy_config in age_grouping_strategies.items():
        print(f"\n--- {strategy_config['name']} 分析 ---")
        
        # 根据年龄分组（只有一种策略）
        age_sample_df['age_group'] = pd.cut(
            age_sample_df['age'], 
            bins=strategy_config['bins'],
            labels=strategy_config['labels'],
            include_lowest=True,
            right=False  # 左闭右开区间，例如[2,8)表示2-7岁
        )
        
        # 统计各年龄组样本数量
        group_counts = age_sample_df['age_group'].value_counts().sort_index()
        print(f"各年龄组样本分布:")
        for group, count in group_counts.items():
            if pd.notna(group):  # 排除NaN组
                print(f"  {group}: {count}个样本")
        
        # 过滤掉样本数量过少的组（<3个样本）
        valid_groups = group_counts[group_counts >= 3].index.tolist()
        if len(valid_groups) == 0:
            print(f"  警告：{strategy_config['name']} 没有足够样本的年龄组，跳过此策略")
            continue
            
        print(f"  有效年龄组数量: {len(valid_groups)}")
        
        # 对每个有效年龄组进行SHAP值聚合分析
        group_aggregation_results = []
        
        for group_label in valid_groups:
            if pd.isna(group_label):
                continue
                
            # 获取该年龄组的样本索引
            group_samples = age_sample_df[age_sample_df['age_group'] == group_label]
            group_indices = group_samples['sample_index'].tolist()
            group_ages = group_samples['age'].tolist()
            
            print(f"\n  处理年龄组: {group_label} ({len(group_indices)}个样本)")
            print(f"    年龄范围: {min(group_ages):.1f} - {max(group_ages):.1f} 岁")
            
            # 显示详细信息
            print(f"    对应年龄段: {group_label}")
            # 验证年龄范围是否符合预期
            expected_ranges = {
                'A2-7': (2, 7), 'A8-12': (8, 12), 'A13-17': (13, 17), 'A18-22': (18, 22),
                'A23-27': (23, 27), 'A28-32': (28, 32), 'A33-37': (33, 37), 'A38-42': (38, 42),
                'A43-47': (43, 47), 'A48-52': (48, 52), 'A53-57': (53, 57), 'A58-62': (58, 62),
                'A63-67': (63, 67), 'A68-72': (68, 72), 'A73-77': (73, 77), 'A78-82': (78, 82),
                'A83-87': (83, 87), 'A88-92': (88, 92), 'A92+': (92, 120)
            }
            if str(group_label) in expected_ranges:
                exp_min, exp_max = expected_ranges[str(group_label)]
                actual_min, actual_max = min(group_ages), max(group_ages)
                if str(group_label) != 'A92+':
                    print(f"    预期范围: {exp_min}-{exp_max}岁, 实际范围: {actual_min:.1f}-{actual_max:.1f}岁")
                else:
                    print(f"    预期范围: {exp_min}岁以上, 实际范围: {actual_min:.1f}-{actual_max:.1f}岁")
            
            print(f"    样本: {group_samples['sample_name'].tolist()[:5]}{'...' if len(group_indices) > 5 else ''}")
            
            # 提取该年龄组的SHAP值
            group_shap_values = shap_values[group_indices, :]
            
            # 使用中位数进行聚合（根据经验规范，对异常值稳健）
            aggregated_shap_values = np.median(group_shap_values, axis=0)
            
            # 计算其他统计量用于验证和分析
            mean_shap_values = np.mean(group_shap_values, axis=0)
            std_shap_values = np.std(group_shap_values, axis=0)
            
            # 保存聚合结果
            group_result = {
                'strategy': strategy_name,
                'group_label': str(group_label),
                'sample_count': len(group_indices),
                'age_range': f"{min(group_ages):.1f}-{max(group_ages):.1f}",
                'median_age': np.median(group_ages),
                'aggregated_shap_median': aggregated_shap_values,
                'aggregated_shap_mean': mean_shap_values,
                'aggregated_shap_std': std_shap_values,
                'sample_names': group_samples['sample_name'].tolist(),
                'sample_indices': group_indices
            }
            group_aggregation_results.append(group_result)
            
            # 为该年龄组生成waterfall图（使用修复版实现）
            # 创建虚拟的特征值（使用该年龄组样本的中位数特征值）
            group_feature_values = sample_data.iloc[group_indices].median()
            
            # 生成年龄组聚合waterfall图（使用修复版）
            safe_group_label = str(group_label).replace('/', '_').replace('<', 'lt').replace('>', 'gt')
            waterfall_save_path = os.path.join(age_analysis_dir, 
                                             f'age_group_waterfall_{strategy_name}_{safe_group_label}.pdf')
            _create_shap_waterfall_plot(
                shap_values=aggregated_shap_values,
                base_value=explainer.expected_value,
                feature_values=group_feature_values,
                feature_names=sample_data.columns.tolist(),
                sample_name=group_label,
                sample_index=len(group_indices),
                max_display=min(30, n_select_Feature),  # 保持最大显示30个特征，但不超过n_select_Feature
                save_path=waterfall_save_path,
                title_suffix=f"(n={len(group_indices)})",
                shap_threshold=0.45,
                shap_threshold_mode='absolute'
            )
            plt.close()
            
            print(f"    ✓ Waterfall图已保存: {waterfall_save_path}")
            
            # 保存该年龄组的详细SHAP数据
            group_shap_df = pd.DataFrame({
                'feature': sample_data.columns,
                'shap_median': aggregated_shap_values,
                'shap_mean': mean_shap_values,
                'shap_std': std_shap_values
            }).sort_values('shap_median', key=abs, ascending=False)
            
            # 创建安全的组标签用于文件名
            safe_group_label = str(group_label).replace('/', '_').replace('<', 'lt').replace('>', 'gt')
            group_csv_path = os.path.join(aggregation_results_dir, 
                                        f'age_group_shap_{strategy_name}_{safe_group_label}.csv')
            group_shap_df.to_csv(group_csv_path, index=False)
            print(f"    ✓ SHAP聚合数据已保存: {group_csv_path}")
        
        # 生成该分组策略的汇总报告
        if group_aggregation_results:
            # 创建年龄组对比热力图
            create_age_group_comparison_heatmap(group_aggregation_results, sample_data.columns, 
                                               age_analysis_dir, strategy_name, strategy_config['name'], n_select_Feature)
            
            # 保存分组策略汇总数据
            strategy_summary = {
                'strategy_name': strategy_name,
                'strategy_display_name': strategy_config['name'],
                'total_groups': len(group_aggregation_results),
                'total_samples_analyzed': sum(r['sample_count'] for r in group_aggregation_results),
                'age_range_analyzed': f"{min(r['median_age'] for r in group_aggregation_results):.1f}-{max(r['median_age'] for r in group_aggregation_results):.1f}",
                'groups_info': [(r['group_label'], r['sample_count'], r['age_range']) for r in group_aggregation_results]
            }
            
            summary_path = os.path.join(aggregation_results_dir, f'strategy_summary_{strategy_name}.json')
            with open(summary_path, 'w', encoding='utf-8') as f:
                json.dump(strategy_summary, f, ensure_ascii=False, indent=2)
            
            print(f"\n  ✓ {strategy_config['name']} 分析完成")
            print(f"    - 有效年龄组: {len(group_aggregation_results)}个")
            print(f"    - 分析样本: {sum(r['sample_count'] for r in group_aggregation_results)}个")
            print(f"    - 汇总报告: {summary_path}")
    
    # 生成最终的年龄组聚合分析报告
    print(f"\n" + "=" * 60)
    print("年龄范围样本聚合分析完成")
    print("=" * 60)
    print(f"✓ 分析结果保存在: {age_analysis_dir}")
    print(f"✓ 数据文件保存在: {aggregation_results_dir}")
    print(f"✓ 分析策略: {len(age_grouping_strategies)}种")
    print("\n主要输出:")
    print("  1. 各年龄组的waterfall图")
    print("  2. 年龄组SHAP值聚合数据(CSV)")
    print("  3. 年龄组对比热力图")
    print("  4. 分组策略汇总报告(JSON)")
    
    return age_analysis_dir, aggregation_results_dir



def create_age_group_comparison_heatmap(group_results, feature_names, output_dir, strategy_name, strategy_display_name, n_select_Feature=50):
    """
    创建年龄组间SHAP值对比热力图
    
    参数:
    - group_results: 年龄组聚合结果列表
    - feature_names: 特征名称列表
    - output_dir: 输出目录
    - strategy_name: 策略名称
    - strategy_display_name: 策略显示名称
    - n_select_Feature: 选择的特征数量
    """
    if len(group_results) < 2:
        print(f"    警告: {strategy_display_name} 年龄组数量不足，跳过对比热力图生成")
        return
    
    print(f"\n  生成 {strategy_display_name} 年龄组对比热力图...")
    
    # 构建年龄组×特征的SHAP值矩阵
    group_labels = [result['group_label'] for result in group_results]
    shap_matrix = np.array([result['aggregated_shap_median'] for result in group_results])
    
    # 检查是否存在select.txt文件，如果存在则使用其中指定的特征
    select_file_path = "select.txt"
    if os.path.exists(select_file_path):
        print(f"    发现 {select_file_path} 文件，将使用指定的特征生成热力图")
        
        # 读取select.txt中的特征名称
        with open(select_file_path, "r", encoding="utf-8") as f:
            selected_features = [line.strip() for line in f.readlines() if line.strip()]
        
        # 查找在数据中实际存在的特征
        found_features = []
        found_indices = []
        for feature_name in selected_features:
            # 将feature_names转换为列表以便使用index方法
            feature_names_list = list(feature_names) if not isinstance(feature_names, list) else feature_names
            if feature_name in feature_names_list:
                found_features.append(feature_name)
                found_indices.append(feature_names_list.index(feature_name))
        
        if found_features:
            print(f"    在数据中找到 {len(found_features)} 个指定特征")
            top_features = found_features
            top_shap_matrix = shap_matrix[:, found_indices]
        else:
            print(f"    未找到匹配的特征，将使用默认的前{n_select_Feature}个重要特征")
            # 选择影响最大的前n_select_Feature个特征进行可视化
            feature_importance = np.mean(np.abs(shap_matrix), axis=0)
            top_feature_indices = np.argsort(feature_importance)[::-1][:n_select_Feature]
            top_features = [feature_names[i] for i in top_feature_indices]
            top_shap_matrix = shap_matrix[:, top_feature_indices]
    else:
        print(f"    未发现 select.txt 文件，将使用前{n_select_Feature}个最重要的特征")
        # 选择影响最大的前n_select_Feature个特征进行可视化
        feature_importance = np.mean(np.abs(shap_matrix), axis=0)
        top_feature_indices = np.argsort(feature_importance)[::-1][:n_select_Feature]
        top_features = [feature_names[i] for i in top_feature_indices]
        top_shap_matrix = shap_matrix[:, top_feature_indices]
    
    # 创建热力图
    plt.figure(figsize=(16, max(5, len(group_labels) * 0.4)))  # 进一步减小高度使图表更紧凑
    
    # 使用seaborn绘制热力图
    heatmap_df = pd.DataFrame(
        top_shap_matrix,
        index=group_labels,
        columns=top_features
    )
    
    sns.heatmap(heatmap_df, 
                annot=False,  # 不显示数值，避免过于拥挤
                cmap='RdBu_r',  # 红蓝色图，红色表示正向影响，蓝色表示负向影响
                center=0,  # 以0为中心
                cbar_kws={'label': 'SHAP Value'},  # 使用英文标签避免特殊字符问题
                xticklabels=True,
                yticklabels=True)
    
    # 进一步简化标题以确保完整显示
    plt.title(f'Age Group SHAP Comparison', fontsize=16, pad=20)
    
    plt.xlabel('ASV Features', fontsize=12)
    plt.ylabel('Age Groups', fontsize=12)
    plt.xticks(rotation=45, ha='right')
    plt.yticks(rotation=0)
    
    # 增加边距以确保标题和标签完整显示
    plt.tight_layout(rect=[0.05, 0.05, 0.95, 0.95])
    
    # 保存热力图
    heatmap_save_path = os.path.join(output_dir, f'age_group_comparison_heatmap_{strategy_name}.pdf')
    plt.savefig(heatmap_save_path, dpi=300, bbox_inches='tight')
    plt.close()  # 关闭图形以释放内存
    
    # 保存热力图数据
    heatmap_data_path = os.path.join(output_dir, f'age_group_comparison_data_{strategy_name}.csv')
    heatmap_df.to_csv(heatmap_data_path)
    
    print(f"    ✓ 年龄组对比热力图已保存: {heatmap_save_path}")
    print(f"    ✓ 热力图数据已保存: {heatmap_data_path}")
    
    # 生成年龄组差异分析报告
    if len(group_results) >= 2:
        generate_age_group_difference_analysis(group_results, feature_names, output_dir, strategy_name)


def generate_age_group_difference_analysis(group_results, feature_names, output_dir, strategy_name):
    """
    生成年龄组间差异分析
    
    识别在不同年龄组间表现差异最大的ASV特征
    """
    print(f"\n  进行年龄组差异分析...")
    
    # 计算各特征在不同年龄组间的变异系数
    group_shap_matrix = np.array([result['aggregated_shap_median'] for result in group_results])
    
    # 计算每个特征在各年龄组间的标准差和变异系数
    feature_std = np.std(group_shap_matrix, axis=0)
    feature_mean_abs = np.mean(np.abs(group_shap_matrix), axis=0)
    
    # 避免除零错误
    feature_cv = np.divide(feature_std, feature_mean_abs + 1e-8)  # 变异系数
    
    # 识别年龄相关的差异特征
    age_differential_features = []
    
    for i, feature_name in enumerate(feature_names):
        feature_values = group_shap_matrix[:, i]
        median_ages = [result['median_age'] for result in group_results]
        
        # 计算特征SHAP值与年龄的相关性
        correlation = np.corrcoef(feature_values, median_ages)[0, 1]
        
        age_differential_features.append({
            'feature': feature_name,
            'variation_coefficient': feature_cv[i],
            'std_across_groups': feature_std[i],
            'mean_abs_shap': feature_mean_abs[i],
            'age_correlation': correlation if not np.isnan(correlation) else 0,
            'max_group_shap': np.max(feature_values),
            'min_group_shap': np.min(feature_values),
            'shap_range': np.max(feature_values) - np.min(feature_values)
        })
    
    # 按变异系数排序，识别年龄差异最大的特征
    age_differential_df = pd.DataFrame(age_differential_features)
    age_differential_df = age_differential_df.sort_values('variation_coefficient', ascending=False)
    
    # 保存年龄差异分析结果
    diff_analysis_path = os.path.join(output_dir, f'age_differential_features_{strategy_name}.csv')
    age_differential_df.to_csv(diff_analysis_path, index=False)
    
    # 生成年龄差异特征报告
    top_differential_features = age_differential_df.head(20)
    
    print(f"    ✓ 年龄差异分析完成")
    print(f"    ✓ 最高变异系数: {age_differential_df['variation_coefficient'].max():.3f}")
    print(f"    ✓ 前5个年龄差异特征:")
    for _, row in top_differential_features.head(5).iterrows():
        print(f"      - {row['feature']}: CV={row['variation_coefficient']:.3f}, 年龄相关性={row['age_correlation']:.3f}")
    
    print(f"    ✓ 详细结果保存至: {diff_analysis_path}")
    
    return diff_analysis_path


def generate_selected_samples_waterfall(explainer, shap_values, sample_data, 
                                       selected_indices, sample_names_list, 
                                       shap_plots_dir, n_select_Feature=50):
    """
    为指定的样本批量生成waterfall图
    """
    print(f"\n开始为 {len(selected_indices)} 个指定样本生成waterfall图...")
    
    # 检查是否存在特征筛选文件
    feature_select_file = "select.txt"
    has_feature_select = os.path.exists(feature_select_file)
    
    if has_feature_select:
        print(f"同时发现 {feature_select_file} 文件，将使用指定特征生成waterfall图")
        
        # 读取指定的特征
        with open(feature_select_file, "r") as f:
            selected_features = [line.strip() for line in f.readlines() if line.strip()]
        
        # 找到匹配的特征
        found_features = []
        for feature_name in selected_features:
            if feature_name in sample_data.columns:
                found_features.append(feature_name)
        
        feature_indices_to_plot = [sample_data.columns.get_loc(f) for f in found_features]
        print(f"在数据集中找到并筛选出 {len(feature_indices_to_plot)} 个特征进行绘图")
        
        # 为每个指定样本生成waterfall图
        for i, (sample_idx, sample_name) in enumerate(zip(selected_indices, sample_names_list)):
            print(f"\n正在生成第{i+1}/{len(selected_indices)}个样本: {sample_name} (索引: {sample_idx})")
            
            # 获取该样本的SHAP值和特征值
            sample_shap_values = shap_values[sample_idx][feature_indices_to_plot]
            sample_feature_values = sample_data.iloc[sample_idx][found_features]
            
            # 生成waterfall图
            create_custom_waterfall_plot(
                shap_values=sample_shap_values,
                base_value=explainer.expected_value,
                feature_values=sample_feature_values,
                feature_names=found_features,
                sample_index=sample_idx,
                sample_name=sample_name,
                max_display=len(found_features),
                save_path=os.path.join(shap_plots_dir, 'shap_local', f'shap_waterfall_{sample_name}_selected_features.pdf'),
                title_suffix="(Selected Features)"
            )
            
    else:
        print(f"未发现特征筛选文件，将使用前{n_select_Feature}个最重要的特征")
        
        # 计算全局特征重要性
        feature_importance = np.sum(np.abs(shap_values), axis=0)
        sorted_features_indices = np.argsort(feature_importance)[::-1]
        top_50_features_indices = sorted_features_indices[:n_select_Feature]
        top_50_feature_names = [sample_data.columns[i] for i in top_50_features_indices]
        
        # 为每个指定样本生成waterfall图
        for i, (sample_idx, sample_name) in enumerate(zip(selected_indices, sample_names_list)):
            print(f"\n正在生成第{i+1}/{len(selected_indices)}个样本: {sample_name} (索引: {sample_idx})")
            
            # 获取该样本的SHAP值和特征值
            sample_shap_values = shap_values[sample_idx][top_50_features_indices]
            sample_feature_values = sample_data.iloc[sample_idx][top_50_feature_names]
            
            # 生成waterfall图（使用原始SHAP实现）
            waterfall_save_path = os.path.join(shap_plots_dir, 'shap_local', f'shap_waterfall_{sample_name}_top{n_select_Feature}_features.pdf')
            _create_shap_waterfall_plot(
                shap_values=sample_shap_values,
                base_value=explainer.expected_value,
                feature_values=sample_feature_values,
                feature_names=top_50_feature_names,
                sample_index=sample_idx,
                sample_name=sample_name,
                max_display=n_select_Feature,
                save_path=waterfall_save_path,
                title_suffix=f"(Top {n_select_Feature} Features)"
            )
    
    print(f"\n✓ 已完成 {len(selected_indices)} 个指定样本的waterfall图生成")
    print(f"图表保存在: {os.path.join(shap_plots_dir, 'shap_local')}")

def main():
    """主程序入口"""
    X_data, y_data, sample_names, output_dir = setup_data()
    if X_data is None:
        return

    if TRAIN_MODEL:
        final_model, X_final, output_dir = train_and_save_model(X_data, y_data, output_dir)
        if final_model is not None:
            run_post_analysis(final_model, X_final, y_data, sample_names, output_dir, n_select_Feature=50)
    else:
        final_model, X_final, y, sample_names, output_dir = load_model_and_data(output_dir)
        if final_model is not None:
            run_post_analysis(final_model, X_final, y, sample_names, output_dir, n_select_Feature=50)

            
    logging.info("程序执行完毕。")


if __name__ == "__main__":
    main()

sys.exit("程序终止。")  # 终止程序
