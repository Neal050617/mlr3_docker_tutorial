#!/bin/bash
set -euo pipefail

echo "开始验证环境配置..."

# 检查R环境
echo "检查 R 环境..."
if ! Rscript -e 'installed.packages()' > /dev/null 2>&1; then
    echo "❌ R环境配置有误"
    exit 1
fi
echo "✅ R 环境正常"

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

# 在容器内跳过 VS Code 扩展检查
if [ -f "/.dockerenv" ]; then
    echo "⚠️ 容器内跳过 VS Code 扩展检查"
else
    # 检查VS Code扩展
    echo "检查 VS Code 扩展..."
    if ! code --list-extensions > /dev/null 2>&1; then
        echo "❌ VS Code 命令行工具不可用"
        exit 1
    fi

    if ! diff .vscode/extensions.json /tmp/installed_extensions > /dev/null 2>&1; then
        echo "❌ VS Code 扩展不完整"
        exit 1
    fi
    echo "✅ VS Code 扩展正常"
fi

echo "✅ 环境验证完成" 