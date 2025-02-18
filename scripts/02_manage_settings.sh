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
        
        if [ $IN_CONTAINER -eq 1 ]; then
            echo "👉 当前用户: $(whoami) (uid=$(id -u))"
            
            # 在容器内预先创建所有必需的目录
            mkdir -p ~/.config
            mkdir -p ~/.local/share/code-server/User/
            mkdir -p ~/.local/share/code-server/extensions

            # 检查目录是否创建成功并显示权限
            for dir in ~/.config ~/.local ~/.local/share/code-server/User ~/.local/share/code-server/extensions; do
                if [ ! -d "$dir" ]; then
                    echo "❌ 错误：目录 $dir 创建失败"
                    exit 1
                fi
                
                # 显示目录权限信息
                echo "📁 检查目录 $dir:"
                echo "   所有者: $(stat -c '%U(%u)' "$dir")"
                echo "   权限: $(stat -c '%A' "$dir")"
                
                # 检查是否有写权限
                if [ ! -w "$dir" ]; then
                    echo "⚠️ 警告：没有 $dir 的写入权限"
                fi
            done
        fi
        
        # 导入 VS Code 设置
        if [ -f ".vscode/settings.json" ]; then
            if [ $IN_CONTAINER -eq 1 ]; then
                ln -sf ~/analysis/.vscode/settings.json ~/.local/share/code-server/User/settings.json
            elif [ -d "$HOME/Library/Application Support/Code/User" ]; then
                # macos
                cp .vscode/settings.json "$HOME/Library/Application Support/Code/User/settings.json"
            elif [ -d "$HOME/.config/Code/User" ]; then
                # linux
                cp .vscode/settings.json "$HOME/.config/Code/User/settings.json"
            elif [ -d "$APPDATA/Code/User" ]; then
                cp .vscode/settings.json "$APPDATA/Code/User/settings.json"
            fi
        fi

        # 在容器内安装扩展
        if [ $IN_CONTAINER -eq 1 ]; then
            if [ -f ".vscode/extensions.json" ]; then
                echo "在容器内安装扩展..."
                echo "验证 code-server 路径: $(which code-server)"
                jq -r '.recommendations[]' .vscode/extensions.json | while read ext; do
                    code-server --install-extension "$ext"
                done
            fi
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