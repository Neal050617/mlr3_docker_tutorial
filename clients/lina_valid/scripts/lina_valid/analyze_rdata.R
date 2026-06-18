# =============================================================================
# RData文件分析脚本
# 用于分析efs.RData文件内容，帮助理解和重构原始分析流程
# =============================================================================

# 清空环境
rm(list = ls())

# 设置工作目录（请根据实际情况修改）
# setwd("/Users/colinliu/Desktop/lina")

cat("开始加载RData文件...\n")

# 加载RData文件
load("efs.RData")

cat("RData文件加载完成！\n\n")

# =============================================================================
# 1. 查看所有对象
# =============================================================================
cat("=== 1. 工作空间中的所有对象 ===\n")
all_objects <- ls()
print(all_objects)
cat(paste("总共有", length(all_objects), "个对象\n\n"))

# =============================================================================
# 2. 按类型分类对象
# =============================================================================
cat("=== 2. 对象类型分析 ===\n")
object_info <- data.frame(
  Name = all_objects,
  Class = sapply(all_objects, function(x) class(get(x))[1]),
  Type = sapply(all_objects, function(x) typeof(get(x))),
  Size = sapply(all_objects, function(x) object.size(get(x))),
  stringsAsFactors = FALSE
)

# 按类型排序
object_info <- object_info[order(object_info$Class),]
print(object_info)

# =============================================================================
# 3. 详细分析主要对象
# =============================================================================
cat("\n=== 3. 主要对象详细信息 ===\n")

# 查找数据框对象
data_frames <- all_objects[sapply(all_objects, function(x) is.data.frame(get(x)))]
if(length(data_frames) > 0) {
  cat("\n--- 数据框对象 ---\n")
  for(df_name in data_frames) {
    df <- get(df_name)
    cat(paste("对象名称:", df_name, "\n"))
    cat(paste("维度:", nrow(df), "行 x", ncol(df), "列\n"))
    cat("列名称:", paste(colnames(df), collapse = ", "), "\n")
    cat("前几行数据:\n")
    print(head(df, 3))
    cat("\n")
  }
}

# 查找模型对象
model_objects <- all_objects[sapply(all_objects, function(x) {
  obj_class <- class(get(x))[1]
  obj_class %in% c("lm", "glm", "randomForest", "svm", "train", "xgb.Booster", "lgb.Booster")
})]

if(length(model_objects) > 0) {
  cat("\n--- 模型对象 ---\n")
  for(model_name in model_objects) {
    model <- get(model_name)
    cat(paste("模型名称:", model_name, "\n"))
    cat(paste("模型类型:", class(model)[1], "\n"))
    if("summary" %in% methods(class(model))) {
      cat("模型摘要:\n")
      try(print(summary(model)), silent = TRUE)
    }
    cat("\n")
  }
}

# 查找列表对象
list_objects <- all_objects[sapply(all_objects, function(x) is.list(get(x)) && !is.data.frame(get(x)))]
if(length(list_objects) > 0) {
  cat("\n--- 列表对象 ---\n")
  for(list_name in list_objects) {
    list_obj <- get(list_name)
    cat(paste("列表名称:", list_name, "\n"))
    cat(paste("元素数量:", length(list_obj), "\n"))
    if(length(names(list_obj)) > 0) {
      cat("元素名称:", paste(names(list_obj), collapse = ", "), "\n")
    }
    cat("\n")
  }
}

# =============================================================================
# 4. 查找函数定义
# =============================================================================
cat("\n=== 4. 用户定义的函数 ===\n")
functions <- all_objects[sapply(all_objects, function(x) is.function(get(x)))]
if(length(functions) > 0) {
  for(func_name in functions) {
    cat(paste("函数名称:", func_name, "\n"))
    func <- get(func_name)
    cat("函数参数:", paste(names(formals(func)), collapse = ", "), "\n")
    cat("函数代码:\n")
    print(func)
    cat("\n")
  }
} else {
  cat("未发现用户定义的函数\n")
}

# =============================================================================
# 5. 生成分析报告
# =============================================================================
cat("\n=== 5. 分析报告生成 ===\n")

# 创建分析报告
report <- list(
  总对象数 = length(all_objects),
  数据框数量 = length(data_frames),
  模型数量 = length(model_objects),
  函数数量 = length(functions),
  列表数量 = length(list_objects),
  对象详情 = object_info
)

# 保存分析报告
save(report, file = "rdata_analysis_report.RData")
write.csv(object_info, "rdata_objects_info.csv", row.names = FALSE)

cat("分析完成！\n")
cat("分析报告已保存到: rdata_analysis_report.RData\n")
cat("对象信息已保存到: rdata_objects_info.csv\n")

# =============================================================================
# 6. 建议的重构步骤
# =============================================================================
cat("\n=== 6. 脚本重构建议 ===\n")
cat("基于RData文件分析，建议按以下步骤重构脚本：\n")
cat("1. 数据加载和预处理\n")
cat("2. 特征选择和工程\n") 
cat("3. 模型训练和验证\n")
cat("4. 结果分析和可视化\n")
cat("5. 模型保存和报告生成\n\n")

cat("请查看生成的CSV文件了解所有对象的详细信息。\n") 