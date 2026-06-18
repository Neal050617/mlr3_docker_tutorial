setwd("/Users/colinliu/Nutstore\ Files/我的坚果云/R/mlr3_docker_tutorial/data_for_analysis/20250211-mlr-JINYU/")
library(tidyverse)
library(dtplyr)
library(ggplot2)
library(showtext)
library(openxlsx)

load("./step1.RData")
set.seed(123)

# 拆分为哑变量的变量对应信息
clinic_tb <- clinic_colnames |>
    mutate(old_name = str_remove(old_name, "^feature_24_"))
tNGS_tb <- tNGS_colnames |>
    mutate(old_name = str_remove(old_name, "^feature_17_"))
write_csv(clinic_tb, "./clinic_tb.csv")
write_csv(tNGS_tb, "./tNGS_tb.csv")

# 重要信息的变量名
feature_names
# 对应信息表
#glimpse(data0)
# 所有特征表
#glimpse(data14)

list_data = list(
    "普通和重症肺炎病原体检出结果",
    "在不同性别中普通和重症肺炎的病原体检出结果",
    "在不同年龄段轻重症肺炎的病原体检出结果",
    "在不同样本中普通和重症肺炎的病原体检出结果",
    "不同季节普通和重症肺炎的病原体检出结果",
    "不同免疫状态轻重症肺炎的病原体检出结果",
    "单双叶受累中普通和重症肺炎的病原体检出结果",
    "不同肺叶数受累中普通和重症肺炎的病原体检出结果",
    "肺炎支原体耐药和非耐药患者中普通和重症肺炎的病原体检出结果",
    "不同样本在不同肺炎类型中病原体的检出情况",
    "不同季节在不同肺炎类型中病原体的检出情况",
    "不同免疫状态在不同肺炎类型中病原体的检出情况",
    "影像学累计不同肺叶数量在不同肺炎类型中病原体的检出情况"
)

list_data2 <- list(
    NULL, # 普通与重症
    "feature09", # 性别
    "feature10", # 按照3，6，14年龄分三类
    "feature14", # 按照三种样本类型分三类
    "feature03", # 按照3-5，6-8，9-11，12-2分四个季节
    "feature65", # cd4/cd8
    "feature26", # 单双叶
    "feature27", # 不同肺叶数
    c("feature17_1", "feature17_2"), # 肺炎支原体耐药和非耐药
    "feature14", # 不同样本在不同肺炎类型中病原体的检出情况；把第四种情况倒着来
    "feature03", # 不同季节在不同肺炎类型中病原体的检出情况
    "feature65", # 不同免疫状态在不同肺炎类型中病原体的检出情况
    "feature27"  # 影像学累计不同肺叶数量在不同肺炎类型中病原体的检出情况
)

#total_samples <- nrow(data14) # 获取总样本数

# 修改后的统计检验函数
perform_statistical_test <- function(tbl_result) {
    tbl <- tbl_result$table
    feature_name <- tbl_result$feature_name

    # 确保至少有2x2表格
    if (nrow(tbl) >= 2 && ncol(tbl) >= 2) {
        tryCatch(
            {
                # 计算期望频数
                expected <- chisq.test(tbl)$expected

                # 判断使用哪种检验
                if (any(expected < 5)) {
                    # 使用费歇尔精确检验
                    test_result <- fisher.test(tbl, simulate.p.value = TRUE)
                    method <- "Fisher's Exact Test"
                } else {
                    # 使用卡方检验
                    test_result <- chisq.test(tbl)
                    method <- "Chi-square Test"
                }

                # 返回结果
                return(tibble(
                    feature = feature_name, # 使用提取的特征名
                    p_value = test_result$p.value,
                    method = method
                ))
            },
            error = function(e) {
                return(NULL) # 如果出现错误，返回NULL
            }
        )
    } else {
        return(NULL) # 如果表格小于2x2，返回NULL
    }
}

# 先进行统计检验
create_contingency_table <- function(focus_on = "feature13",
                                     feature_name = "feature17_1",
                                     focus_on2 = NULL,
                                     focus_on2_subset = NULL) {
    # 构建分组变量
    group_vars <- c(focus_on, focus_on2)
    if (is.null(focus_on2_subset)) {
        dd <- data14 |>
            select(all_of(group_vars), all_of(feature_name)) |> # 使用具体特征列名
            table()
    } else {
        dd <- data14 |>
            select(all_of(group_vars), all_of(feature_name)) |> # 使用具体特征列名
            filter(!!sym(focus_on2) %in% focus_on2_subset) |>
            select(-all_of(focus_on2)) |>
            table()
    }
    # 返回列联表和特征名
    return(list(table = dd, feature_name = feature_name))
}
# 数据预处理函数
prepare_feature_data <- function(data = data14, 
                                focus_on = "feature13",
                                focus_on2 = NULL,
                                feature_colnames = tNGS_colnames,
                                total_samples = nrow(data)) {
  # 构建分组变量
  group_vars <- c(focus_on, focus_on2)

  data |>
    select(all_of(c(group_vars, feature_colnames$new_name))) |>
    rownames_to_column("id") |>
    as_tibble() |>
    pivot_longer(
        cols = -c(id, all_of(group_vars)),
        names_to = "features",
        values_to = "value"
    ) |>
    # 按feature13和features分组计算
    group_by(across(all_of(focus_on)), features) |>
    summarise(
        count = sum(value), # 计算每组的计数
        group_size = n(), # 获取每组的样本总数
        per = count / group_size * 100, # 计算百分比
        .groups = "drop"
    ) |> ungroup() |> select(-group_size) |>
    group_by(across(all_of(focus_on))) |>
    nest() |>
    mutate(data2 = map2(!!sym(focus_on), data, function(x, y) {
      y |> rename_with(~ paste0("level_", x), .cols = "count") |> 
        rename_with(~ paste0("level_", x, "_per"), .cols = "per")
    })) |>  
    pull(data2) |> 
    reduce(full_join, by = "features") |>  # 全连接合并
    arrange(desc(rowSums(across(starts_with("level_"))))) # 按行总和降序排列
}

# 可视化函数
create_feature_plot <- function(plot_data=Data, sig_features = significant_features, 
                                title = "标题", x_label = "特征",y_label = "百分比", 
                                color_labels = c("轻症", "重症")) {
    # 参数说明:
    # plot_data: 预处理好的数据
    # sig_features: 显著性特征数据
    # title: 图表标题
    # x_label: X轴标签
    # y_label: Y轴标签
    # color_labels: 图例标签

    # 合并显著性标记
    plot_sign <- plot_data |>
        left_join(sig_features, by = c("features" = "feature")) |>
        mutate(features = fct_inorder(features))

    # 转换绘图数据格式  
    plot_data_long <- plot_data |>
        left_join(tNGS_tb, by = c("features" = "new_name")) |>
        pivot_longer(
            cols = c(level_0_per, level_1_per),
            names_to = "level",
            values_to = "percentage"
        ) |>
        mutate(
            level = factor(str_remove(level, "_per") |> str_remove("level_")) |>
                fct_recode(
                               !!color_labels[1] := "0",
                    !!color_labels[2] := "1"
                ),
            count_label = ifelse(level == color_labels[1], level_0, level_1),
            display_label = paste0(count_label, "\n(", percentage, "%)"),
            features = fct_inorder(features),        
            old_name = fct_inorder(old_name)
        )

    # 创建图表
    w <- ggplot(plot_data_long, aes(x = old_name, y = percentage, fill = level)) +
        scale_fill_manual(values = setNames(c("#4F81BD", "#F79646"), color_labels)) +
        geom_col(position = position_dodge(0.8), width = 0.7) +
        geom_text(
            aes(label = display_label, y = percentage + 2),
            position = position_dodge(width = 0.8),
            size = 3,
            lineheight = 0.8
        ) +
        annotate(
            "text",
            x = seq_along(unique(plot_sign$features)),
            y = max(plot_data_long$percentage) * 1.01,
            label = plot_sign$label,
            vjust = -0.2,
            size = 5
        ) +
        labs(
            title = title,
            x = x_label,
            y = y_label,
            fill = ""
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(
                hjust = 0.5,
                size = 14,
                face = "bold",
                margin = margin(b = 10)
            ),
            axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = c(0.95, 0.95),
            legend.justification = c(1, 1)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
}

# 数据预处理函数给多组用的
prepare_feature_data2 <- function(data = data14,
                                  focus_on = "feature13",
                                  focus_on2 = "feature14",
                                  feature_colnames = tNGS_colnames,
                                  total_samples = nrow(data)) {
    # 构建分组变量
    group_vars <- c(focus_on, focus_on2)

    data |>
        select(all_of(c(group_vars, feature_colnames$new_name))) |>
        rownames_to_column("id") |>
        as_tibble() |>
        pivot_longer(
            cols = -c(id, all_of(group_vars)),
            names_to = "features",
            values_to = "value"
        ) |>
        mutate(across(all_of(focus_on), as.factor)) |>
        group_by(across(all_of(group_vars)), features) |>
        summarise(total = sum(value), .groups = "drop") |>
        pivot_wider(
            names_from = all_of(focus_on2),
            values_from = total,
            names_prefix = "level_",
            values_fill = 0
        ) |>
        mutate(
            total_samples = total_samples,
            total_per = rowSums(across(starts_with("level_"), ~.x)),
            across(starts_with("level_"),
                ~ round(.x / total_samples * 100, 1),
                .names = "{.col}_per"
            )
        ) |>
        arrange(desc(total_per))
}
# 可视化函数
create_feature_plot2 <- function(plot_data = Data, sig_features = significant_features,
                                 title = "标题", x_label = "特征", y_label = "百分比",
                                 color_labels = c("痰", "灌洗液", "咽拭子")) {
    # 合并显著性标记
    plot_sign <- plot_data |>
        left_join(sig_features, by = c("features" = "feature")) |>
        mutate(features = fct_inorder(features))

    # 获取实际的level数量和对应关系
    level_cols <- names(plot_data)[str_detect(names(plot_data), "^level_\\d+$")]
    level_nums <- str_extract(level_cols, "\\d+")

    # 动态生成 case_when 条件
    case_conditions <- map2(seq_along(color_labels), level_cols, function(i, col) {
        quo(level == color_labels[!!i] ~ !!sym(col))
    })
    # 添加默认条件
    case_conditions <- c(case_conditions, quo(TRUE ~ NA_real_))

    # 转换绘图数据格式
    plot_data_long <- plot_data |>
        left_join(tNGS_tb, by = c("features" = "new_name")) |>
        pivot_longer(
            cols = starts_with("level_") & ends_with("_per"),
            names_to = "level",
            values_to = "percentage"
        ) |>
        mutate(
            level = str_remove(level, "_per") |> str_remove("level_"),
            level = factor(level,
                levels = names(color_labels),
                labels = unname(color_labels)
            ),
            count_label = case_when(!!!case_conditions),
            display_label = paste0(count_label, "\n(", percentage, "%)"),
            features = fct_inorder(features),
            old_name = fct_inorder(old_name)
        )

    # 创建颜色向量
    color_values <- c("#4F81BD", "#F79646", "#9BBB59", "#8064A2")[1:length(color_labels)]
    names(color_values) <- color_labels

    # 创建图表
    ggplot(plot_data_long, aes(x = old_name, y = percentage, fill = level)) +
        scale_fill_manual(values = color_values) +
        geom_col(position = position_dodge(0.8), width = 0.7) +
        geom_text(
            aes(label = display_label, y = percentage + 2),
            position = position_dodge(width = 0.8),
            size = 3,
            lineheight = 0.8
        ) +
        annotate(
            "text",
            x = seq_along(unique(plot_sign$features)),
            y = max(plot_data_long$percentage) * 1.01,
            label = plot_sign$label,
            vjust = -0.2,
            size = 5
        ) +
        labs(
            title = title,
            x = x_label,
            y = y_label,
            fill = ""
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(
                hjust = 0.5,
                size = 14,
                face = "bold",
                margin = margin(b = 10)
            ),
            axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = c(0.95, 0.95),
            legend.justification = c(1, 1)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
}

# 1. 普通和重症肺炎病原体检出结果 #########################################################
Data <- prepare_feature_data(
    data = data14,          # 明确指定数据
    focus_on = "feature13", # 分析特征
    focus_on2 = list_data2[[1]], # 分析特征
    feature_colnames = tNGS_colnames  # 明确传递列名信息
)
feature_colnames = tNGS_colnames
# 使用修改后的函数
significant_features <- map_df(feature_colnames$new_name, ~ {
    tbl <- create_contingency_table(focus_on, .x, 
                                   focus_on2 = list_data2[[1]], 
                                   focus_on2_subset = NULL)
    # 调用整合后的函数
    perform_statistical_test(tbl) |> 
        mutate(label = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*"
    ))})

PP <- create_feature_plot(plot_data=Data[1:10,],
                          sig_features = significant_features,
                          title = list_data[[1]],
                          x_label = "病原体",y_label = "检出率(%)",
                          color_labels = c("轻症","重症"))
showtext_auto() # 自动启用中文支持
ggsave(paste0("./","01.",list_data[[1]],".pdf"), PP, width = 10, height = 8)

# 保存为Excel工作簿
wb <- createWorkbook()
addWorksheet(wb, "01.sign")
writeData(wb, "01.sign",significant_features)
addWorksheet(wb, "01.data")
writeData(wb, "01.data", Data)

# 2. 在不同性别中普通和重症肺炎的病原体检出结果 #########################################################
feature_colnames = tNGS_colnames
focus_on = "feature13"
Data <- prepare_feature_data(
    data = data14,          # 明确指定数据
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[2]], # 分析特征
    feature_colnames = feature_colnames  # 明确传递列名信息
)
sData <- Data %>% group_by(!!sym(list_data2[[2]])) %>% nest()
c_list <- c("1"="男","0"="女")
lapply(1:nrow(sData), function(i) {
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ {
        tbl <- create_contingency_table(focus_on, .x,
            focus_on2 = list_data2[[2]],
            focus_on2_subset = as.numeric(as.character(sData[[list_data2[[2]]]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot(plot_data = sData$data[[i]] %>% 
                               arrange(desc(total_per)) %>% head(10), 
             sig_features = significant_features, 
             title = paste0(c_list[as.character(sData[[list_data2[[2]]]][i])],"性患者普通和重症肺炎检出统计图"), 
             x_label = "病原体", 
             y_label = "检出率(%)", 
             color_labels = c("轻症", "重症"))
    showtext_auto() # 自动启用中文支持
    ggsave(paste0("./", "02.", c_list[as.character(sData[[list_data2[[2]]]][i])],
        list_data[[1]], ".pdf"), PP, width = 10, height = 8)
})

# 保存为Excel工作簿
addWorksheet(wb, "02.sign")
writeData(wb, "02.sign", significant_features)
addWorksheet(wb, "02.data")
writeData(wb, "02.data", Data)

# 3. 在不同年龄段轻重症肺炎的病原体检出结果 #########################################################
analysis_id <- 3
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
Data <- prepare_feature_data(
    data = data14,          # 明确指定数据
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames  # 明确传递列名信息
)
# 将年龄段重新划分为三类
Data <- Data %>%
    mutate(!!sym(list_data2[[analysis_id]]) := case_when(
        !!sym(list_data2[[analysis_id]]) < 3 ~ 1,
        !!sym(list_data2[[analysis_id]]) >= 3 & !!sym(list_data2[[analysis_id]]) < 6 ~ 2,
        !!sym(list_data2[[analysis_id]]) >= 6 & !!sym(list_data2[[analysis_id]]) < 14 ~ 3
    )) |> group_by(!!sym(list_data2[[3]]),features) |>
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    filter(is.na(!!sym(list_data2[[analysis_id]])) == FALSE)

sData <- Data %>% group_by(!!sym(list_data2[[analysis_id]])) %>% nest()
c_list <- c("1"="幼儿组","2"="学龄前组","3"="学龄组")
lapply(1:nrow(sData), function(i) {
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ {
        tbl <- create_contingency_table(focus_on, .x,
            focus_on2 = list_data2[[analysis_id]],
            focus_on2_subset = as.numeric(as.character(sData[[list_data2[[analysis_id]]]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[list_data2[[analysis_id]]]][i])], "普通和重症肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c("轻症", "重症")
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0",analysis_id), analysis_id), 
            c_list[as.character(sData[[list_data2[[analysis_id]]]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})

addWorksheet(wb, "03.sign")
writeData(wb, "03.sign", significant_features)
addWorksheet(wb, "03.data")
writeData(wb, "03.data", Data)

# 4. 在不同样本中普通和重症肺炎的病原体检出结果 #########################################################
analysis_id <- 4
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
data14 <- data14 %>%
    mutate(feature14 = case_when(
        feature14_0 == 1 ~ 0,
        feature14_1 == 1 ~ 1,
        feature14_2 == 1 ~ 2,
        feature14_3 == 1 ~ 3
    ))
Data <- prepare_feature_data(
    data = data14, 
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames  # 明确传递列名信息
)
sData <- Data %>%
    group_by(!!sym(list_data2[[analysis_id]])) %>%
    nest()
c_list <- c("0" = "痰", "1" = "灌洗液", "2" = "咽拭子", "3" = "气管分泌液")
lapply(1:nrow(sData), function(i) {# i <- 2
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ {
        tbl <- create_contingency_table(focus_on, .x,
            focus_on2 = list_data2[[analysis_id]],
            focus_on2_subset = as.numeric(as.character(sData[[list_data2[[analysis_id]]]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[list_data2[[analysis_id]]]][i])], "普通和重症肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c("轻症", "重症")
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        c_list[as.character(sData[[list_data2[[analysis_id]]]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})
addWorksheet(wb, "04.sign")
writeData(wb, "04.sign", significant_features)
addWorksheet(wb, "04.data")
writeData(wb, "04.data", Data)

# 5. 不同季节普通和重症肺炎的病原体检出结果 #########################################################
analysis_id <- 5
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
data14$feature03 = 
    left_join(rownames(data14) %>% as_tibble() %>% rename_at(1,~"SampleID"),
  data1 |> 
  rename("SampleID" := !!feature_names$id) |>  
  mutate(SampleID = str_replace_all(SampleID, "^`", "S")) |>
  select(SampleID,feature02,feature03)) |>
   mutate(
       month02 = month(feature02),
       month03 = month(feature03)
   ) |>
       # 创建新列season，优先使用feature03，缺失时使用feature02
       mutate(season = case_when(
           month03 %in% c(3, 4, 5) ~ 1, # 3-5月为季节1
           month03 %in% c(6, 7, 8) ~ 2, # 6-8月为季节2
           month03 %in% c(9, 10, 11) ~ 3, # 9-11月为季节3
           month03 %in% c(12, 1, 2) ~ 4, # 12-2月为季节4
           is.na(month03) & month02 %in% c(3, 4, 5) ~ 1,
           is.na(month03) & month02 %in% c(6, 7, 8) ~ 2,
           is.na(month03) & month02 %in% c(9, 10, 11) ~ 3,
           is.na(month03) & month02 %in% c(12, 1, 2) ~ 4,
           TRUE ~ NA_real_ # 如果都无法判断，设为NA
       )) |>
       select(season) %>% pull(season)

Data <- prepare_feature_data(
    data = data14,
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames # 明确传递列名信息
)
sData <- Data %>%
    group_by(!!sym(list_data2[[analysis_id]])) %>%
    nest()
c_list <- c("1" = "春", "2" = "夏", "3" = "秋","4" = "冬")
lapply(1:nrow(sData), function(i) { # i <- 2
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ {
        tbl <- create_contingency_table(focus_on, .x,
            focus_on2 = list_data2[[analysis_id]],
            focus_on2_subset = as.numeric(as.character(sData[[list_data2[[analysis_id]]]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[list_data2[[analysis_id]]]][i])], "普通和重症肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c("轻症", "重症")
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        c_list[as.character(sData[[list_data2[[analysis_id]]]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})
addWorksheet(wb, "05.sign")
writeData(wb, "05.sign", significant_features)
addWorksheet(wb, "05.data")
writeData(wb, "05.data", Data)

# 6. 不同免疫状态和重症肺炎的病原体检出结果 #########################################################
analysis_id <- 6
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
data14 <- data14 %>%
    mutate(feature65 = case_when(
        feature65 <1.5 ~ 0,
        feature65 >=1.5 ~ 1,
        TRUE ~ NA_real_
    ))
Data <- prepare_feature_data(
    data = data14, # 明确指定数据
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames # 明确传递列名信息
) |> filter(is.na(!!sym(list_data2[[analysis_id]])) == FALSE)

sData <- Data %>%
    group_by(!!sym(list_data2[[analysis_id]])) %>%
    nest()
c_list <- c("0" = "免疫低下", "1" = "免疫正常")
lapply(1:nrow(sData), function(i) { # i <- 2
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ {
        tbl <- create_contingency_table(focus_on, .x,
            focus_on2 = list_data2[[analysis_id]],
            focus_on2_subset = as.numeric(as.character(sData[[list_data2[[analysis_id]]]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[list_data2[[analysis_id]]]][i])], "普通和重症肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c("轻症", "重症")
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        c_list[as.character(sData[[list_data2[[analysis_id]]]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})
addWorksheet(wb, "06.sign")
writeData(wb, "06.sign", significant_features)
addWorksheet(wb, "06.data")
writeData(wb, "06.data", Data)

# 7. 单双叶肺叶数和重症肺炎的病原体检出结果 #########################################################
analysis_id <- 7
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
Data <- prepare_feature_data(
    data = data14, # 明确指定数据
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames # 明确传递列名信息
) |> filter(is.na(!!sym(list_data2[[analysis_id]])) == FALSE)

sData <- Data %>%
    group_by(!!sym(list_data2[[analysis_id]])) %>%
    nest()
c_list <- c("0" = "单侧", "1" = "双侧")
lapply(1:nrow(sData), function(i) { # i <- 2
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ {
        tbl <- create_contingency_table(focus_on, .x,
            focus_on2 = list_data2[[analysis_id]],
            focus_on2_subset = as.numeric(as.character(sData[[list_data2[[analysis_id]]]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[list_data2[[analysis_id]]]][i])], "普通和重症肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c("轻症", "重症")
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        c_list[as.character(sData[[list_data2[[analysis_id]]]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})
addWorksheet(wb, "07.sign")
writeData(wb, "07.sign", significant_features)
addWorksheet(wb, "07.data")
writeData(wb, "07.data", Data)

# 8. 不同肺叶数和重症肺炎的病原体检出结果 #########################################################
analysis_id <- 8
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
data14 <- data14 %>% 
    mutate(feature27 = case_when(
        feature27 == 1 ~ 0,
        feature27 >= 2 ~ 1,
        TRUE ~ NA_real_
    ))
Data <- prepare_feature_data(
    data = data14, # 明确指定数据
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames # 明确传递列名信息
) |> filter(is.na(!!sym(list_data2[[analysis_id]])) == FALSE)

sData <- Data %>%
    group_by(!!sym(list_data2[[analysis_id]])) %>%
    nest()
c_list <- c("0" = "单个", "1" = "多个")
lapply(1:nrow(sData), function(i) { # i <- 2
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ {
        tbl <- create_contingency_table(focus_on, .x,
            focus_on2 = list_data2[[analysis_id]],
            focus_on2_subset = as.numeric(as.character(sData[[list_data2[[analysis_id]]]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[list_data2[[analysis_id]]]][i])], "普通和重症肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c("轻症", "重症")
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        c_list[as.character(sData[[list_data2[[analysis_id]]]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})
addWorksheet(wb, "08.sign")
writeData(wb, "08.sign", significant_features)
addWorksheet(wb, "08.data")
writeData(wb, "08.data", Data)

# 9. 肺炎支原体耐药和非耐药患者中普通和重症肺炎的病原体检出结果 #########################################################
analysis_id <- 9
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
data14 <- data14 %>%
  mutate(feature17 = case_when(
      feature17_1 == 1 & feature17_2 == 0 ~ 0, # 耐药
      feature17_1 == 1 & feature17_2 == 1 ~ 1, # 非耐药
      TRUE ~ NA_real_ # 其他情况设为NA
  ))
list_data2[[analysis_id]] <- "feature17"
Data <- prepare_feature_data(
    data = data14, # 明确指定数据
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames # 明确传递列名信息
) |> filter(is.na(!!sym(list_data2[[analysis_id]])) == FALSE)

sData <- Data %>%
    group_by(!!sym(list_data2[[analysis_id]])) %>%
    nest()
c_list <- c("0" = "支原体非耐药", "1" = "支原体耐药")
lapply(1:nrow(sData), function(i) { # i <- 2
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ {
        tbl <- create_contingency_table(focus_on, .x,
            focus_on2 = list_data2[[analysis_id]],
            focus_on2_subset = as.numeric(as.character(sData[[list_data2[[analysis_id]]]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[list_data2[[analysis_id]]]][i])], "普通和重症肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c("轻症", "重症")
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        c_list[as.character(sData[[list_data2[[analysis_id]]]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})
addWorksheet(wb, "09.sign")
writeData(wb, "09.sign", significant_features)
addWorksheet(wb, "09.data")
writeData(wb, "09.data", Data)

# 10. 不同病原体和重症肺炎的病原体检出结果 #########################################################
analysis_id <- 10
feature_colnames <- tNGS_colnames
focus_on <- "feature13"

data14 <- data14 %>%
    mutate(feature14 = case_when(
        feature14_0 == 1 ~ 0,
        feature14_1 == 1 ~ 1,
        feature14_2 == 1 ~ 2,
        TRUE ~ NA_real_ # 其他情况设为NA
    ))

Data <- prepare_feature_data2(
    data = data14,
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames # 明确传递列名信息
)  |> select(-all_of(c("level_NA","level_NA_per")))

sData <- Data %>%
    group_by(!!sym(focus_on)) %>%
    nest()
c_list <- c("0" = "痰", "1" = "灌洗液", "2" = "咽拭子")
F_list <- c("0" = "普通肺炎", "1" = "重症肺炎")
lapply(1:nrow(sData), function(i) { # i <- 1
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ { # x <- feature_colnames$new_name[1]
        tbl <- create_contingency_table(list_data2[[analysis_id]], .x,
            focus_on2 = focus_on,
            focus_on2_subset = as.numeric(as.character(sData[[focus_on]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot2(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[focus_on]][i])], "肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c_list
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        F_list[as.character(sData[[focus_on]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})
addWorksheet(wb, "10.sign")
writeData(wb, "10.sign", significant_features)
addWorksheet(wb, "10.data")
writeData(wb, "10.data", Data)

# 11. 不同季节在不同肺炎类型中的病原体检出结果 #########################################################
analysis_id <- 11
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
data14$feature03 <-
    left_join(
        rownames(data14) %>% as_tibble() %>% rename_at(1, ~"SampleID"),
        data1 |>
            rename("SampleID" := !!feature_names$id) |>
            mutate(SampleID = str_replace_all(SampleID, "^`", "S")) |>
            select(SampleID, feature02, feature03)
    ) |>
    mutate(
        month02 = month(feature02),
        month03 = month(feature03)
    ) |>
    # 创建新列season，优先使用feature03，缺失时使用feature02
    mutate(season = case_when(
        month03 %in% c(3, 4, 5) ~ 1, # 3-5月为季节1
        month03 %in% c(6, 7, 8) ~ 2, # 6-8月为季节2
        month03 %in% c(9, 10, 11) ~ 3, # 9-11月为季节3
        month03 %in% c(12, 1, 2) ~ 4, # 12-2月为季节4
        is.na(month03) & month02 %in% c(3, 4, 5) ~ 1,
        is.na(month03) & month02 %in% c(6, 7, 8) ~ 2,
        is.na(month03) & month02 %in% c(9, 10, 11) ~ 3,
        is.na(month03) & month02 %in% c(12, 1, 2) ~ 4,
        TRUE ~ NA_real_ # 如果都无法判断，设为NA
    )) |>
    select(season) %>%
    pull(season)

Data <- prepare_feature_data2(
    data = data14,
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames # 明确传递列名信息
)

sData <- Data %>%
    group_by(!!sym(focus_on)) %>%
    nest()
c_list <- c("1" = "春", "2" = "夏", "3" = "秋", "4" = "冬")
F_list <- c("0" = "普通肺炎", "1" = "重症肺炎")
lapply(1:nrow(sData), function(i) { # i <- 1
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ { # x <- feature_colnames$new_name[1]
        tbl <- create_contingency_table(list_data2[[analysis_id]], .x,
            focus_on2 = focus_on,
            focus_on2_subset = as.numeric(as.character(sData[[focus_on]][i]))
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot2(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[focus_on]][i])], "肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c_list
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        F_list[as.character(sData[[focus_on]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})
addWorksheet(wb, "11.sign")
writeData(wb, "11.sign", significant_features)
addWorksheet(wb, "11.data")
writeData(wb, "11.data", Data)

# 12.不同免疫状态在不同肺炎类型中病原体的检出情况 #########################################################
analysis_id <- 12
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
data14 <- data14 %>%
    mutate(feature65 = case_when(
        feature65 < 1.5 ~ 0,
        feature65 >= 1.5 ~ 1,
        TRUE ~ NA_real_
    ))

Data <- prepare_feature_data2(
    data = data14,
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames # 明确传递列名信息
)

sData <- Data %>%
    group_by(!!sym(focus_on)) %>%
    nest()
c_list <- c("0" = "免疫低下", "1" = "免疫正常")
F_list <- c("0" = "普通肺炎", "1" = "重症肺炎")
lapply(1:nrow(sData), function(i) { # i <- 1
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ { # x <- feature_colnames$new_name[1]
        tbl <- create_contingency_table(list_data2[[analysis_id]], .x,
            focus_on2 = focus_on,
            focus_on2_subset = as.numeric(as.character(sData[[focus_on]][i]))
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot2(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[focus_on]][i])], "肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c_list
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        F_list[as.character(sData[[focus_on]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})

addWorksheet(wb, "12.sign")
writeData(wb, "12.sign", significant_features)
addWorksheet(wb, "12.data")
writeData(wb, "12.data", Data)

# 13.影像学累计不同肺叶数量在不同肺炎类型中病原体的检出情况 #########################################################

analysis_id <- 13
feature_colnames <- tNGS_colnames
focus_on <- "feature13"
data14 <- data14 %>%
    mutate(feature27 = case_when(
        feature27 == 1 ~ 0,
        feature27 >= 2 ~ 1,
        TRUE ~ NA_real_
    ))
Data <- prepare_feature_data2(
    data = data14,
    focus_on = focus_on, # 分析特征
    focus_on2 = list_data2[[analysis_id]], # 分析特征
    feature_colnames = feature_colnames # 明确传递列名信息
) |> select(-all_of(c("level_NA","level_NA_per")))
c_list <- c("0" = "单个", "1" = "多个")
F_list <- c("0" = "普通肺炎", "1" = "重症肺炎")
lapply(1:nrow(sData), function(i) { # i <- 1
    # 使用修改后的函数
    significant_features <- map_df(feature_colnames$new_name, ~ { # x <- feature_colnames$new_name[1]
        tbl <- create_contingency_table(list_data2[[analysis_id]], .x,
            focus_on2 = focus_on,
            focus_on2_subset = as.numeric(as.character(sData[[focus_on]][i]))
        )
        perform_statistical_test(tbl)
    })

    PP <- create_feature_plot2(
        plot_data = sData$data[[i]] %>%
            arrange(desc(total_per)) %>% head(10),
        sig_features = significant_features,
        title = paste0(c_list[as.character(sData[[focus_on]][i])], "肺炎病原体分布统计图"),
        x_label = "病原体",
        y_label = "检出率(%)",
        color_labels = c_list
    )
    showtext_auto() # 自动启用中文支持
    ggsave(paste0(
        "./", ifelse(analysis_id < 10, paste0("0", analysis_id), analysis_id),
        F_list[as.character(sData[[focus_on]][i])],
        list_data[[1]], ".pdf"
    ), PP, width = 10, height = 8)
})
addWorksheet(wb, "13.sign")
writeData(wb, "13.sign", significant_features)
addWorksheet(wb, "13.data")
writeData(wb, "13.data", Data)

#########
saveWorkbook(wb, "model_report.xlsx", overwrite = TRUE)
