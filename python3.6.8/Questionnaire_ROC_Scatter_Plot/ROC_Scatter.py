import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn import metrics


def plot_questionnaire_roc(
        roc_csv_path,
        survey_excel_path,
        group_col='type',
        dept_col='department',
        sens_col='Sensitivity',
        spec_col='Specificity',
        output_path='ROC_questionnaire_plot.pdf',
        model_label='Model',
        figsize=(8, 6),
        level_colors=None,
        markers=None,
        ci_bootstrap=True,
        n_bootstrap=1000,
        random_seed=42
):
    """
    绘制问卷ROC散点图

    roc_csv_path: CSV文件，包含FPR/TPR列
    survey_excel_path: Excel文件，包含问卷数据的敏感度、特异度、分组、子群
    group_col: 问卷分组列
    dept_col: 子群列
    sens_col: 敏感度列
    spec_col: 特异度列
    """
    # 默认颜色和marker
    if level_colors is None:
        level_colors = {}
    if markers is None:
        markers = {}

    # 读取ROC曲线数据
    roc_df = pd.read_csv(roc_csv_path)
    fpr = roc_df['FPR'].values
    tpr = roc_df['TPR'].values
    auc_value = metrics.auc(fpr, tpr)

    # bootstrap计算95%CI
    if ci_bootstrap:
        rng = np.random.default_rng(random_seed)
        aucs = []
        n = len(fpr)
        for _ in range(n_bootstrap):
            indices = rng.integers(0, n, n)
            aucs.append(metrics.auc(fpr[indices], tpr[indices]))
        ci_lower, ci_upper = np.percentile(aucs, [2.5, 97.5])
        auc_text = f"AUC {auc_value:.3f} (95% CI {ci_lower:.3f}–{ci_upper:.3f})"
    else:
        auc_text = f"AUC {auc_value:.3f}"

    # 读取问卷Excel
    df = pd.read_excel(survey_excel_path)
    groups = df[group_col].unique()

    plt.figure(figsize=figsize)
    plt.plot(1 - fpr, tpr, color='red', label=model_label, linewidth=2)

    # 绘制每个问卷组别和子群
    for g in groups:
        g_df = df[df[group_col] == g]
        subgroups = g_df[dept_col].unique()
        for sub in subgroups:
            sub_df = g_df[g_df[dept_col] == sub]
            plt.scatter(
                1 - sub_df[spec_col],
                sub_df[sens_col],
                color=level_colors.get(sub, 'gray'),
                marker=markers.get(g, 'o'),
                s=70,
                label=f"{g} {sub}"
            )
        # 平均值
        avg_x = np.mean(1 - g_df[spec_col])
        avg_y = np.mean(g_df[sens_col])
        plt.scatter(
            avg_x, avg_y,
            color='red' if g != groups[0] else 'deepskyblue',
            marker=markers.get(g, 'o'),
            s=110,
            label=f"Average {g}"
        )

    plt.xlabel('1 - Specificity')
    plt.ylabel('Sensitivity')
    plt.legend(fontsize=10, loc='upper right')
    plt.text(0.6, 0.64, auc_text, fontsize=12)
    plt.xlim(0, 1)
    plt.ylim(0, 1)
    plt.grid(False)
    plt.tight_layout()
    plt.savefig(output_path)
    plt.show()
    print(f"[Saved] {output_path} | {auc_text}")



