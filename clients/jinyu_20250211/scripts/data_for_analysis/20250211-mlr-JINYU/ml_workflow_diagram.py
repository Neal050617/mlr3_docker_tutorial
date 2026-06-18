import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch, ConnectionPatch
import numpy as np

# 设置中文字体
plt.rcParams['font.sans-serif'] = ['Arial Unicode MS', 'SimHei', 'DejaVu Sans']
plt.rcParams['axes.unicode_minus'] = False

# 创建图形
fig, ax = plt.subplots(1, 1, figsize=(16, 20))
ax.set_xlim(0, 10)
ax.set_ylim(0, 24)
ax.axis('off')

# 定义颜色
colors = {
    'data': '#E8F4FD',      # 浅蓝色 - 数据处理
    'feature': '#FFF2CC',    # 浅黄色 - 特征选择
    'model': '#E1D5E7',      # 浅紫色 - 模型训练
    'eval': '#D5E8D4',       # 浅绿色 - 评估
    'output': '#FFE6CC'      # 浅橙色 - 输出
}

# 定义框的样式
def create_box(ax, x, y, width, height, text, color, fontsize=10):
    box = FancyBboxPatch((x, y), width, height,
                        boxstyle="round,pad=0.1",
                        facecolor=color,
                        edgecolor='black',
                        linewidth=1.5)
    ax.add_patch(box)
    ax.text(x + width/2, y + height/2, text,
           ha='center', va='center', fontsize=fontsize,
           wrap=True, weight='bold')

# 定义箭头
def create_arrow(ax, start_x, start_y, end_x, end_y):
    arrow = ConnectionPatch((start_x, start_y), (end_x, end_y), 
                          "data", "data",
                          arrowstyle="->", 
                          shrinkA=5, shrinkB=5,
                          mutation_scale=20, 
                          fc="black", 
                          linewidth=2)
    ax.add_patch(arrow)

# 1. 数据准备阶段
create_box(ax, 1, 22, 8, 1.5, 
          "1. 数据准备阶段\n加载step1.RData → 特征筛选 → 数据预处理 → 训练/测试集划分(9:1)", 
          colors['data'], 11)

# 2. 集成特征选择(EFS)阶段
create_box(ax, 0.5, 19.5, 4, 2, 
          "2. 集成特征选择(EFS)\n• RFE特征选择器\n• 4个基础学习器\n• 100次子采样+5折CV", 
          colors['feature'], 10)

create_box(ax, 5.5, 19.5, 4, 2, 
          "EFS输出文件:\n• performance.pdf\n• n_features.pdf\n• pareto.pdf\n• stability.pdf", 
          colors['output'], 9)

# 3. 模型训练与调优
create_box(ax, 0.5, 16.5, 4, 2, 
          "3. 模型训练与调优\n• 5个ML模型\n• 随机搜索调优\n• 5折内外层CV", 
          colors['model'], 10)

create_box(ax, 5.5, 16.5, 4, 2, 
          "调优输出文件:\n• model_comparison.auc.pdf\n• model_comparison.roc.pdf", 
          colors['output'], 9)

# 4. 模型评估
create_box(ax, 1, 13.5, 8, 1.5, 
          "4. 模型评估阶段\n训练集评估 → 测试集评估 → 交叉验证评估", 
          colors['eval'], 11)

# 5. 特征重要性分析
create_box(ax, 0.5, 10.5, 4, 2, 
          "5. 特征重要性分析\n• RF重要性\n• XGBoost重要性\n• LightGBM重要性\n• GLMNet系数", 
          colors['feature'], 10)

create_box(ax, 5.5, 10.5, 4, 2, 
          "重要性输出:\n• rf_feature_importance.pdf\n• xgboost_feature_importance.pdf\n• lightgbm_feature_importance.pdf\n• glmnet_feature_importance.pdf", 
          colors['output'], 8)

# 6. 综合分析
create_box(ax, 1, 7.5, 8, 1.5, 
          "6. 综合分析阶段\nWilcoxon检验 → Patchwork可视化 → 性能汇总表", 
          colors['eval'], 11)

# 最终输出
create_box(ax, 0.5, 4.5, 4, 2, 
          "统计检验结果:\n• step3.*_pod_wilcox_test.tsv\n• step3.*.patchwork.pdf", 
          colors['output'], 9)

create_box(ax, 5.5, 4.5, 4, 2, 
          "最终输出:\n• model_performance_comparison.xlsx\n• train/test/cv_roc_plot.pdf", 
          colors['output'], 9)

# 关键特征发现
create_box(ax, 1, 1.5, 8, 1.5, 
          "关键发现\n重要特征: feature83(铁蛋白), feature45(DD二聚体), feature40(IL-8), feature44(白蛋白)\n最佳模型: Random Forest (AUC≈0.748)", 
          colors['eval'], 11)

# 添加箭头连接
create_arrow(ax, 5, 22, 5, 21.5)  # 1->2
create_arrow(ax, 2.5, 19.5, 2.5, 18.5)  # 2->3
create_arrow(ax, 5, 16.5, 5, 15)  # 3->4
create_arrow(ax, 5, 13.5, 5, 12.5)  # 4->5
create_arrow(ax, 2.5, 10.5, 2.5, 9)  # 5->6
create_arrow(ax, 5, 7.5, 5, 6.5)  # 6->outputs
create_arrow(ax, 5, 4.5, 5, 3)  # outputs->findings

# 添加侧边箭头
create_arrow(ax, 4.5, 20.5, 5.5, 20.5)  # EFS->output
create_arrow(ax, 4.5, 17.5, 5.5, 17.5)  # model->output
create_arrow(ax, 4.5, 11.5, 5.5, 11.5)  # feature->output

# 添加标题
ax.text(5, 24, 'MLR3机器学习流程图', ha='center', va='center', 
        fontsize=16, weight='bold')

# 添加图例
legend_elements = [
    mpatches.Patch(color=colors['data'], label='数据处理'),
    mpatches.Patch(color=colors['feature'], label='特征选择'),
    mpatches.Patch(color=colors['model'], label='模型训练'),
    mpatches.Patch(color=colors['eval'], label='模型评估'),
    mpatches.Patch(color=colors['output'], label='输出文件')
]
ax.legend(handles=legend_elements, loc='upper right', bbox_to_anchor=(0.98, 0.98))

plt.tight_layout()
plt.savefig('ml_workflow_diagram.pdf', dpi=300, bbox_inches='tight')
plt.savefig('ml_workflow_diagram.png', dpi=300, bbox_inches='tight')
plt.show()

print("流程图已保存为 ml_workflow_diagram.pdf 和 ml_workflow_diagram.png")