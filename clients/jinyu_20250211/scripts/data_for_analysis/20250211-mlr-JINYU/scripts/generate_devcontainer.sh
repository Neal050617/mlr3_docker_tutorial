#!/bin/bash

# 确保 .devcontainer 目录存在
mkdir -p .devcontainer

# 获取用户配置
USER_NAME=${USER_NAME:-$(whoami)}

# 生成 devcontainer.json 文件
cat <<EOF > .devcontainer/devcontainer.json
{
    "name": "R Development",
    "dockerComposeFile": "../docker-compose.yml",
    "service": "dev",
    "workspaceFolder": "/home/\${localEnv:USER_NAME}/analysis",
    // 确保用户权限正确
    "remoteUser": "\${localEnv:USER_NAME}",
    "updateRemoteUserUID": true,
    
    // 初始化命令 - 在容器创建前执行，确保 .env 文件存在
    "initializeCommand": "bash -c 'if [ ! -f .env ]; then bash scripts/00_init-env.sh --non-interactive; fi'",

    // 环境变量设置
    "remoteEnv": {
        "USER_NAME": "\${localEnv:USER_NAME}",
        "PASSWORD": "\${localEnv:PASSWORD}",
        "USER_ID": "\${localEnv:USER_ID}",
        "GROUP_ID": "\${localEnv:GROUP_ID}",
        "R_LIBS_USER": "/usr/local/lib/R/site-library",
        "PATH": "\${containerEnv:PATH}:/usr/local/bin/R"
    },

    // 端口转发配置
    "forwardPorts": [8787, 8080],
    
    // 关闭VS Code窗口时停止容器
    "shutdownAction": "stopCompose",

    // VS Code 设置
    "customizations": {
        "vscode": {
            "settings": {
                "remote.containers.copyGitConfig": true,
                "remote.containers.gitCredentialsHelper": "cache",
                "r.rterm.linux": "/usr/local/bin/R",
                "r.bracketedPaste": true,
                "r.sessionWatcher": true
            },
            "extensions": [
                // VS Code 远程开发扩展
                "ms-vscode-remote.remote-containers",
                "ms-vscode-remote.remote-ssh",
                "ms-vscode-remote.remote-ssh-edit",
                
                // R 语言支持
                "REditorSupport.r",
                "Ikuyadeu.r",
                "reditorsupport.r-lsp",
                "rdebugger.r-debugger",
                
                // Quarto 支持
                "quarto.quarto",
                
                // 常用工具
                "GitHub.copilot",
                "mechatroner.rainbow-csv",
                "purocean.drawio-preview",
                "redhat.vscode-yaml",
                "yzhang.markdown-all-in-one",
                "ms-azuretools.vscode-docker",
                
                // Python 支持
                "ms-python.python",
                "ms-toolsai.jupyter"
            ]
        }
    },
    
    // 容器创建后执行的命令
    "postCreateCommand": "bash scripts/02_manage_settings.sh import && bash scripts/03_verify_environment.sh",
    
    // 关闭欢迎页面
    "showWelcomeNotification": false
}
EOF

echo "✅ devcontainer.json 已生成"

# 检查 .env 文件是否存在，如果不存在，则创建
if [ ! -f .env ]; then
    echo "⚠️ .env 文件不存在，正在创建..."
    bash scripts/00_init-env.sh --non-interactive
    echo "✅ .env 文件已创建"
else
    echo "✅ .env 文件已存在，跳过创建"
fi

# 检查必要的脚本是否存在
for script in "00_init-env.sh" "02_manage_settings.sh" "03_verify_environment.sh"; do
    if [ ! -f "scripts/$script" ]; then
        echo "⚠️ scripts/$script 不存在，请检查脚本文件是否完整"
    else
        echo "✅ scripts/$script 已存在"
    fi
done

echo "✅ 开发容器配置完成，可以使用 VS Code 的 'Remote-Containers: Reopen in Container' 命令启动容器" 