#!/bin/bash

echo "验证环境配置..."

# 检查R环境
Rscript -e 'installed.packages()' > /dev/null 2>&1 || { echo "R环境配置有误"; exit 1; }

# 检查必要的R包
# Rscript -e '
# required <- c("tidyverse", "devtools", "rmarkdown", "knitr")
# missing <- required[!required %in% installed.packages()[,"Package"]]
# if(length(missing) > 0) {
#     cat("缺少必要的R包:", paste(missing, collapse=", "), "\n")
#     quit(status=1)
# }'
 
# 检查Quarto环境
# quarto check > /dev/null 2>&1 || { echo "Quarto环境配置有误"; exit 1; }

# 检查VS Code扩展
code --list-extensions > /tmp/installed_extensions
diff .vscode/extensions.json /tmp/installed_extensions > /dev/null 2>&1 || { 
    echo "VS Code扩展不完整"; 
    exit 1; 
}

echo "环境验证完成" 