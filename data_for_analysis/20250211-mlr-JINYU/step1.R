## 环境准备
library(tidyverse)
library(DataExplorer)
library(skimr)
library(dtplyr)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

feature_names <- list(
  id = "feature04",
  gender = "feature09",
  age = "feature10",
  group = "feature13",
  tissue = "feature14",
  tNGS = "feature17",
  clinic = "feature24"
)

## 读取数据
data0 = read_tsv("select.list.tsv") |>
  filter(YN == "Yes")
data1 = read_tsv("input.txt")

# 定义NA标识和替换规则
na_values <- c("Na", "na", "N/A", "/")
replace_patterns <- c(
  "\\.\\." = ".",  # 使用转义来匹配真实的点号
  "/" = ".",
  "," = ".",
  "，" = ".",
  "〈" = "<",
  "〉" = ">"
)

### 需要额外处理的列
data11 = data1 |> 
  select(data0 |> filter(YN == "Yes") |> pull(feature_name)) |>
  rename("SampleID" := !!feature_names$id) |>  
  mutate(SampleID = str_replace_all(SampleID, "^`", "S")) |>
  # NA处理：统一处理各种形式的NA
  mutate(across(where(is.character),
                ~{
                  # 先处理NA值
                  x <- if_else(. %in% na_values, NA_character_, .)
                  # 再进行符号替换
                  str_replace_all(x, replace_patterns)
                })) |>
  # 处理attention为replace的列
  mutate(across(any_of(data0 |> 
                    filter(attention == "replace") |> 
                    pull(feature_name)),
                ~str_replace_all(., "[><=']", ""))) |>
  # 新增信号灯转换（处理transfer列）
  mutate(across(any_of(data0 |> 
                    filter(attention == "transfer") |> 
                    pull(feature_name)),
                ~ifelse(as.numeric(.) > 0, 1, as.numeric(.)))) |>
  # 新增统一说法（处理standardize列）
  mutate(across(any_of(data0 |> 
                    filter(attention == "standardize") |> 
                    pull(feature_name)),
                ~case_when(
                  . %in% c("是", "有") ~ "有",
                  . == "无" ~ "无",
                  TRUE ~ .))) |>
  # 性别数字化
  mutate(across(all_of(feature_names$gender),
                ~case_when(
                  . == "男" ~ 1,
                  . == "女" ~ 0,
                  TRUE ~ NA_real_))) |>
  # 年龄单位转换
  mutate(across(all_of(feature_names$age),
              ~{
                sapply(., function(x) {
                  # 判断是否包含时间单位
                  if (str_detect(x, "岁|月|天")) {
                    # 处理带单位的年龄（保留原始单位）
                    units <- str_extract_all(x, "\\d+(岁|月|天)")[[1]]
                    total <- 0
                    for(u in units) {
                      num <- as.numeric(str_extract(u, "\\d+"))
                      if(str_detect(u, "岁")) total <- total + num
                      if(str_detect(u, "月")) total <- total + num/12
                      if(str_detect(u, "天")) total <- total + num/365
                    }
                    round(total, 2)
                  } else {
                    # 处理纯数字的情况（先清洗后转换）
                    clean_age <- str_replace_all(x, "[^0-9.]", "")
                    ifelse(!is.na(suppressWarnings(as.numeric(clean_age))),
                           round(as.numeric(clean_age), 2),
                           NA_real_)
                  }
                })
              }))

#### 删除高缺失列和行
data12 = data11 |> select(-feature_names$tissue, -feature_names$tNGS, -feature_names$clinic)

## 可以开始合并数据了
### 分析字符串列且只有两个唯一值的列
#### 定义一个函数来获取指定唯一值数量的字符列
get_char_cols_with_unique_vals <- function(data, unique_vals) {
  # 获取符合条件的字符列
  cols <- data |>
    select(where(is.character)) |>
    names() |>
    keep(~{
      data |> 
        pull(.) |> 
        unique() |>
        na.omit() |>
        length() == unique_vals
    })
  
  # 打印每个列的类别
  if(length(cols) > 0) {
    cat("找到", length(cols), "个具有", unique_vals, "个唯一值的字符列：\n")
    for(col in cols) {
      cat("列名：", col, "\n")
      cat("类别：", paste(unique(na.omit(data[[col]])), collapse = ", "), "\n\n")
    }
  } else {
    cat("没有找到具有", unique_vals, "个唯一值的字符列\n")
  }
  
  return(cols)
}

#### 分析字符串列且只有三个唯一值的列
three_char_cols <- get_char_cols_with_unique_vals(data12, 3)
data12 = data12 |> mutate(across(all_of(three_char_cols),~str_replace_all(., "有", "是")))

#### 分析字符串列且只有两个唯一值的列
binary_char_cols <- get_char_cols_with_unique_vals(data12, 2)

#### 定义值映射关系
value_pairs <- list(
  "0" = c("无", "无效", "否"),
  "1" = c("有", "好转", "是")
)

#### 自动生成value_mapping
value_mapping <- binary_char_cols |>
  set_names() |>
  map(function(col) {
    # 获取当前列的唯一值（除去NA）
    unique_vals <- unique(na.omit(data12[[col]]))
    # 找到每个值对应的映射
    mapping <- map_chr(unique_vals, function(val) {
      if(val %in% value_pairs[["0"]]) "0"
      else if(val %in% value_pairs[["1"]]) "1"
      else NA_character_
    })
    # 创建命名向量
    setNames(as.numeric(mapping), unique_vals)
  })

#### 应用转换
data13 <- data12 |>
  mutate(across(where(is.character), 
                ~na_if(., "无"))) |>
  mutate(across(all_of(names(value_mapping)),
                ~case_when(
                  . %in% value_pairs[["0"]] ~ 0,
                  . %in% value_pairs[["1"]] ~ 1,
                  TRUE ~ NA_real_  # 非预期值设为NA
                )))

# 将除SampleID外的所有列转换为数值类型，并记录转换为NA的值
conversion_log <- data13 |>
  select(-SampleID) |>
  summarise(across(everything(), 
                  ~list(na_values = unique(.[!is.na(.) & is.na(as.numeric(.))]))))
glimpse(conversion_log)

data13 = data13 |>
  mutate_at(vars(starts_with("feature")), as.numeric) |>
  # 删除高缺失列（超过50% NA的列）
  select(where(~mean(is.na(.)) < 0.5)) |>
  # 删除高缺失行（超过50% NA的样本）
  filter(rowMeans(is.na(pick(everything()))) < 0.5)

### 处理两个需要拆分的列
#### tissue
data_tissue <- data11 |> inner_join(data13[,1], by = "SampleID") |>
  select(SampleID, feature_names$tissue) |>
  mutate(value = 1L) |> 
  pivot_wider(names_from = feature_names$tissue, 
  names_prefix = paste0(feature_names$tissue, "_"),
  values_from = value, values_fill = 0L)

#### tNGS
data_tNGS <- data11 |> inner_join(data13[,1], by = "SampleID") |>
  select(SampleID, feature_names$tNGS) |>
  separate_rows(feature_names$tNGS, sep = ";\\s*") |>
  filter(feature_names$tNGS != "") |>
  separate(feature_names$tNGS, into = c("pathogen", "details"), sep = ":", extra = "merge") |>
  mutate(pathogen = paste0("feature_17_", str_trim(pathogen))) |>
  group_by(SampleID, pathogen) |>
  #summarise(value = paste(details, collapse = ":"), .groups = "drop") |>
  mutate(value = 1L) |>
  pivot_wider(-details,names_from = pathogen, values_from = value, values_fill = 0L) |>
  ungroup()
tNGS_colnames = tibble(
    old_name = colnames(data_tNGS)[-1], 
    new_name = c(map_chr(1:(ncol(data_tNGS)-1),
                          ~paste0(feature_names$tNGS, "_", .x)))
    )
colnames(data_tNGS) = c("SampleID",tNGS_colnames$new_name)  

#### clinic
data_clinic <- data11 |> inner_join(data13[,1], by = "SampleID") |>
  select(SampleID, feature_names$clinic) |>
  mutate(across(feature_names$clinic, ~str_replace_all(., "\\.|，|/|、", ","))) |>
  separate_rows(feature_names$clinic, sep = ",") |>
  mutate(value = 1L) |>
  pivot_wider(
    names_from = feature_names$clinic,
    names_prefix = paste0(feature_names$clinic, "_"),
    values_from = value,
    values_fill = 0L)
clinic_colnames = tibble(
    old_name = colnames(data_clinic)[-1], 
    new_name = c(map_chr(1:(ncol(data_clinic)-1),
                       ~paste0(feature_names$clinic, "_", .x)))
    )
colnames(data_clinic) = c("SampleID",clinic_colnames$new_name)

#### 合并数据
data14 = data13 |>
  left_join(data_tissue, by = "SampleID") |>
  left_join(data_tNGS, by = "SampleID") |>
  left_join(data_clinic, by = "SampleID") 
data14 = as.data.frame(data14)
rownames(data14) = data14$SampleID
data14 = data14 |> select(-SampleID)
## 数据探索分析
### 使用skimr进行基础统计
skim_result <- skim(data14)
print(skim_result)

### 使用DataExplorer进行可视化分析
# 创建分析报告目录
#if(!dir.exists("analysis_report")) dir.create("analysis_report")
#
## 生成缺失值报告
#p1 <- data14 %>% plot_intro()
#ggsave("analysis_report/introduction.pdf", p1, width = 10, height = 30)
#
#p2 <- plot_missing(data14)
#ggsave("analysis_report/missing_values.pdf", p2, width = 10, height = 30)
#
## 生成基础描述性统计报告
#create_report(data14, 
#             output_dir = "analysis_report",
#             output_file = "data_report.html",
#             report_title = "数据探索性分析报告")
#
## 查看数据分布
#p3 <- plot_histogram(data14)
#ggsave("analysis_report/distributions.pndf", p3, width = 20, height = 20)
#
## 查看相关性
#p4 <- plot_correlation(data14, 
#                maxcat = 5,  # 类别变量最大类别数
#                title = "变量相关性分析")
#ggsave("analysis_report/correlations.pdf", p4, width = 20, height = 20)

# 打印基本信息摘要
cat("\n数据集基本信息：\n")
cat("行数：", nrow(data14), "\n")
cat("列数：", ncol(data14), "\n")
cat("数值型变量数：", sum(sapply(data14, is.numeric)), "\n")
cat("字符型变量数：", sum(sapply(data14, is.character)), "\n")
cat("缺失值比例：", mean(is.na(data14)) * 100, "%\n")
#saveRDS(data14, "data14.rds")
save.image("step1.RData")
data14 %>%
  rownames_to_column("SampleID") %>%
  write_tsv("step1_data14.xls")