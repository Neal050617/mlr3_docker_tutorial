#!/bin/bash

# 设置错误时退出
set -e

# 增加 Git 缓冲区大小
git config --global http.postBuffer 524288000
git config --global http.maxRequestBuffer 100M
git config --global core.compression 9
# 使用 HTTPS 而不是 git 协议
git config --global url."https://".insteadOf git://
# 设置较长的超时时间
git config --global http.lowSpeedLimit 1000
git config --global http.lowSpeedTime 300

# 检查是否有超大文件（超过 50MB）
check_large_files() {
    local large_files=$(find . -type f -size +50M ! -path "./.git/*")
    if [ ! -z "$large_files" ]; then
        echo "⚠️ 警告：发现以下大文件（>50MB）："
        echo "$large_files"
        echo "GitHub 不建议上传大文件。建议："
        echo "1. 使用 Git LFS 管理大文件"
        echo "2. 或将这些文件添加到 .gitignore"
        read -p "是否继续？[y/N] " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi
}

# 从配置文件读取 GitHub 信息
if [ -f ".env" ]; then
    source .env
else
    echo "请先创建 .env 文件并设置以下变量："
    echo "GITHUB_TOKEN=your_token"
    echo "GITHUB_USER=your_username"
    echo "GITHUB_EMAIL=your_email"
    exit 1
fi

# 获取项目名称
PROJECT_NAME=${1:-$(basename "$(pwd)")}
echo $PROJECT_NAME

# 检查远程仓库是否已存在
if gh repo view "$GITHUB_USER/$PROJECT_NAME" &> /dev/null; then
    echo "⚠️ 仓库 $GITHUB_USER/$PROJECT_NAME 已存在"
    read -p "是否继续？[y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# 检查 gh 是否安装
if ! command -v gh &> /dev/null; then
    echo "GitHub CLI (gh) 未安装，正在安装..."
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # 配置 Homebrew 镜像源
        git -C "$(brew --repo)" remote set-url origin https://mirrors.ustc.edu.cn/brew.git
        git -C "$(brew --repo homebrew/core)" remote set-url origin https://mirrors.ustc.edu.cn/homebrew-core.git
        brew update
        brew install gh
    else
        echo "请先安装 GitHub CLI: https://cli.github.com/"
        exit 1
    fi
fi

# 检查是否已登录 GitHub
if ! gh auth status &> /dev/null; then
    echo "使用 token 登录 GitHub..."
    echo "$GITHUB_TOKEN" | gh auth login --with-token
    # 设置 Git 配置
    git config --global user.name "$GITHUB_USER"
    git config --global user.email "$GITHUB_EMAIL"
fi

# 检查是否已经是 git 仓库
if [ -d .git ]; then
    echo "目录已经是 Git 仓库，跳过初始化..."
else
    echo "初始化 Git 仓库..."
    git init
    # 确保有文件可以提交
    touch README.md
    # 创建第一次提交后再重命名分支
    git add .
    git commit -m "chore: 初始化项目" || true
    # 确保当前在 master 分支上
    git checkout -b master || true
    git branch -M main
    # 确保切换到 main 分支
    git checkout main || true
fi

# 创建 .gitignore 如果不存在
if [ ! -f .gitignore ]; then
    echo "创建 .gitignore..."
    cat > .gitignore << 'EOF'
    # 不要忽略这些配置文件夹
    !.vscode/
    !.github/
    !scripts/
    
    # 但是要忽略临时文件
    .vscode/extensions_list.txt
    
    # 系统文件
    .DS_Store
    Thumbs.db
    
    # 环境文件
    .env
    *.log

    # R 相关文件
    .Rproj.user/
    .Rhistory
    .RData
    .Ruserdata
    *.Rproj
EOF
fi

# 创建或更新远程仓库
echo "创建 GitHub 仓库: $PROJECT_NAME..."
if ! gh repo create "$PROJECT_NAME" --public --source=. --remote=origin --push; then
    echo "仓库可能已存在，尝试设置远程..."
    git remote add origin "https://github.com/$(gh api user | jq -r '.login')/$PROJECT_NAME.git" || true
fi

# 添加所有文件并提交
echo "提交文件..."
check_large_files

# 移除已经被跟踪的 .specstory 目录
if [ -d ".specstory" ]; then
    echo "移除 Git 跟踪的 .specstory 目录..."
    if git ls-files --error-unmatch .specstory/ &> /dev/null; then
        git rm -r --cached .specstory/
    else
        echo ".specstory/ 目录未被 Git 跟踪，无需移除"
    fi
fi

# 移除已经被跟踪的 .env 文件
if [ -f ".env" ]; then
    echo "移除 Git 跟踪的 .env 文件..."
    if git ls-files --error-unmatch .env &> /dev/null; then
        git rm --cached .env
    else
        echo ".env 文件未被 Git 跟踪，无需移除"
    fi
fi

git add .
git commit -m "feat: 添加项目基础配置" || git commit --amend -m "feat: 添加项目基础配置"

# 确保在推送前位于 main 分支
git checkout main 2>/dev/null || git checkout -b main

# 推送到 GitHub
echo "推送到 GitHub..."
# 尝试推送，如果失败则清理历史
try_push() {
    # 先尝试普通推送
    if git push -u origin main; then
        return 0
    fi

    # 如果失败，检查是否是因为敏感信息
    if [[ $(git push -u origin main 2>&1) == *"Push cannot contain secrets"* ]]; then
        echo "检测到敏感信息，尝试清理历史..."
        
        # 创建新的干净分支
        git checkout --orphan temp_clean
        
        # 添加所有文件
        git add .
        
        # 提交
        git commit -m "feat: 初始化项目"
        
        # 删除旧的 main 分支
        git branch -D main || true
        
        # 重命名当前分支为 main
        git branch -m main
        
        # 强制推送
        git push -f origin main
    else
        # 其他错误，直接返回失败
        return 1
    fi
}

try_push

# 输出成功信息
echo "✅ 仓库创建成功！"
echo "📂 GitHub 仓库地址: https://github.com/$(gh api user | jq -r '.login')/$PROJECT_NAME"
echo "🔧 本地仓库路径: $(pwd)"
echo ""
echo "下一步："
echo "1. 编辑 .github/SECURITY.md 中的联系邮箱"
echo "2. 检查 .devcontainer/devcontainer.json 中的配置"
echo "3. 运行 './scripts/01_check_container.sh' 设置开发环境" 