# MLR3 Docker 教程

这是一个基于 Docker 的 R 语言机器学习环境，使用 MLR3 框架。

## 快速开始

### 前置要求

1. 安装 [Docker Desktop](https://www.docker.com/products/docker-desktop/)
2. 安装 [Git](https://git-scm.com/downloads)
3. 安装 [VS Code](https://code.visualstudio.com/) 或 [Cursor](https://cursor.sh/)（推荐）

### 环境初始化

首次使用时，需要运行初始化脚本来设置环境：

```bash
# 添加执行权限
chmod +x scripts/00_init-env.sh

# 运行初始化脚本
./scripts/00_init-env.sh
```

这个脚本会：
- 让你选择合适的 Docker 镜像源（10秒内未选择将使用默认源）
- 自动获取当前用户信息
- 创建 .env 配置文件

### 两种使用方式

#### 方式一：使用 docker compose（推荐）

简单两步即可启动环境：

```bash
# 1. 初始化环境（如果还没运行过），生成.env文件
./scripts/00_init-env.sh

# 2. 启动环境（会自动构建镜像）
docker compose up -d
```

常用命令：
```bash
# 查看容器状态
docker compose ps

# 停止环境
docker compose down

# 重新构建（修改配置后）
docker compose build --no-cache

# 重新构建（如果需要强制重新构建）
docker compose up -d --build
```

#### 方式二：使用容器检查脚本

这种方式提供更多的错误检查和提示：

```bash
# 1. 初始化环境（如果还没运行过），生成.env文件
./scripts/00_init-env.sh

# 2. 运行容器检查脚本
./scripts/01_check_container.sh
```

这个脚本会：
- 检查并创建必要的文件
- 构建 mobior:v0.0.1 镜像
- 创建并启动 rstudio_test 容器
- 检查端口占用情况
- 提供详细的状态信息

### 访问 RStudio

无论使用哪种方式，都可以通过以下方式访问 RStudio：

- 打开浏览器访问 http://localhost:7878
- 使用以下凭据登录：
  - 用户名：当前系统用户名（自动获取）
  - 密码：MoBio888（默认密码）

### 配置说明

环境配置存储在 .env 文件中，包括：
- 用户信息（USER_ID, GROUP_ID, USER_NAME, GROUP_NAME）
- 密码设置（PASSWORD, ROOT_PASSWORD）
- 系统配置（DISABLE_AUTH, ROOT）
- R包路径（R_SITE_LIBRARY）
- 端口映射（PORT）
- Docker镜像源（DOCKER_MIRROR）

### 开发工具设置

1. 使用 VS Code 或 Cursor
```bash
# 导入编辑器设置和扩展
./scripts/02_manage_settings.sh import
```

2. 验证开发环境
```bash
./scripts/03_verify_environment.sh
```

## 目录结构

```
.
├── .devcontainer/    # 开发容器配置
├── .github/          # GitHub 配置和工作流
├── .vscode/          # VS Code 设置
├── scripts/          # 辅助脚本
├── data_for_analysis/# 数据分析文件
├── Dockerfile        # 容器定义
└── docker-compose.yml# 服务配置
```

## 常用命令

```bash
# 启动环境
docker compose up -d

# 停止环境
docker compose down

# 查看日志
docker compose logs

# 重新构建（修改 Dockerfile 后）
docker compose build --no-cache
```

## 注意事项

1. 首次运行可能需要几分钟下载镜像
2. 确保 Docker Desktop 已经启动
3. 端口 7878 不能被其他程序占用
4. 数据分析文件保存在 data_for_analysis/ 目录
5. R 包库路径需要根据实际情况修改
   - 在 docker-compose.yml 中修改 site-library 的挂载路径
   - 默认路径：`/Users/colinliu/Desktop/20250211-mlr3/R_package_mobio/site-library`

## 环境说明

1. 基础镜像
   - 使用 rocker/rstudio 最新版本
   - 配置了清华镜像源加速

2. 预装工具
   - 基本开发工具（curl, wget, build-essential）
   - R 包开发依赖
   - Rust 开发环境（配置了中科大镜像源）

3. 用户配置
   - 自动使用当前系统用户信息
   - 支持 sudo 权限
   - 工作目录：/home/用户名/analysis

## 问题排查

1. 如果无法访问 RStudio
   - 检查 Docker 是否运行
   - 确认端口 7878 是否可用
   - 查看容器日志：`docker compose logs`

2. 如果密码登录失败
   - 确认 .env 文件配置正确
   - 检查用户名是否与系统用户名匹配

## 贡献

欢迎提交 Issue 和 Pull Request！

## 许可

MIT License

## CI/CD

本项目使用 GitHub Actions 进行持续集成：

### 开发环境验证

每次推送到 main 分支或创建 Pull Request 时，会自动运行开发环境验证：

```yaml
name: 验证开发环境

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: 验证开发环境
        run: |
          docker compose build
          docker compose up -d
          docker compose ps
          docker compose down
```

这个工作流会：
1. 检出代码
2. 构建开发环境镜像
3. 启动容器验证配置
4. 停止并清理容器

如果工作流执行成功，说明开发环境配置正确。 