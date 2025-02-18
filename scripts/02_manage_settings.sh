#!/bin/bash

# 使用方式：
# ./02_manage_settings.sh export  # 导出设置和扩展
# ./02_manage_settings.sh import  # 导入设置和扩展

ACTION=$1

if [ -z "$ACTION" ]; then
    echo "请指定操作: export 或 import"
    exit 1
fi

# 创建必要的目录
mkdir -p .vscode

# 检测是否在容器内
IN_CONTAINER=0
if [ -f "/.dockerenv" ]; then
    IN_CONTAINER=1
fi

case "$ACTION" in
    "export")
        echo "正在导出设置和扩展列表..."
        
        # 导出 VS Code 设置
        if [ -f "$HOME/Library/Application Support/Code/User/settings.json" ]; then
            # macOS
            cp "$HOME/Library/Application Support/Code/User/settings.json" .vscode/settings.json
        elif [ -f "$HOME/.config/Code/User/settings.json" ]; then
            # Linux
            cp "$HOME/.config/Code/User/settings.json" .vscode/settings.json
        elif [ -f "$APPDATA/Code/User/settings.json" ]; then
            # Windows
            cp "$APPDATA/Code/User/settings.json" .vscode/settings.json
        fi

        # 导出 Cursor 设置
        if [ -f "$HOME/Library/Application Support/Cursor/User/settings.json" ]; then
            # macOS
            cp "$HOME/Library/Application Support/Cursor/User/settings.json" .cursor/settings.json
        elif [ -f "$HOME/.config/Cursor/User/settings.json" ]; then
            # Linux
            cp "$HOME/.config/Cursor/User/settings.json" .cursor/settings.json
        elif [ -f "$APPDATA/Cursor/User/settings.json" ]; then
            # Windows
            cp "$APPDATA/Cursor/User/settings.json" .cursor/settings.json
        fi

        # 导出扩展列表并转换为JSON格式
        {
            echo '{'
            echo '  "recommendations": ['
            # 合并 VS Code 和 Cursor 的扩展列表并去重
            { code --list-extensions; cursor --list-extensions; } | sort -u | \
            sed 's/^/    "/' | sed 's/$/",/' | sed '$s/,$//'
            echo '    ]'
            echo '}'
        } > .vscode/extensions.json

        echo "✅ 设置和扩展列表已导出到 .vscode 目录"
        ;;
        
    "import")
        echo "正在导入设置..."
        
        # 导入 VS Code 设置
        if [ -f ".vscode/settings.json" ]; then
            if [ $IN_CONTAINER -eq 1 ]; then
                mkdir -p ~/.local/share/code-server/User/
                ln -sf ~/analysis/.vscode/settings.json ~/.local/share/code-server/User/settings.json
            elif [ -d "$HOME/Library/Application Support/Code/User" ]; then
                cp .vscode/settings.json "$HOME/Library/Application Support/Code/User/settings.json"
            elif [ -d "$HOME/.config/Code/User" ]; then
                cp .vscode/settings.json "$HOME/.config/Code/User/settings.json"
            elif [ -d "$APPDATA/Code/User" ]; then
                cp .vscode/settings.json "$APPDATA/Code/User/settings.json"
            fi
        fi

        # 在容器内不尝试安装扩展
        if [ $IN_CONTAINER -eq 1 ]; then
            echo "⚠️ 容器内跳过扩展安装，将由 VS Code 自动处理"
        else
            # 在本地安装扩展
            if [ -f ".vscode/extensions.json" ]; then
                jq -r '.recommendations[]' .vscode/extensions.json | while read ext; do
                    code --install-extension "$ext"
                done
            fi
        fi
        
        echo "✅ 设置已导入"
        ;;
        
    *)
        echo "❌ 无效的操作: $ACTION"
        echo "使用方式: $0 [export|import]"
        exit 1
        ;;
esac 