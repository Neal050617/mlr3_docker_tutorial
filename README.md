# MLR3 Docker 教程

这是一个基于 Docker 的 R 语言机器学习环境，使用 MLR3 框架。

## 快速开始

### 前置要求

1. 安装 [Docker Desktop](https://www.docker.com/products/docker-desktop/)
2. 安装 [Git](https://git-scm.com/downloads)
3. 安装 [VS Code](https://code.visualstudio.com/) 或 [Cursor](https://cursor.sh/)（推荐）

### 两种使用方式

#### 方式一：使用 docker compose（推荐）

1. 克隆仓库
```bash
git clone https://github.com/Neal050617/mlr3_docker_tutorial.git
cd mlr3_docker_tutorial
```

2. 创建配置文件
```bash
# 复制示例配置文件
cp .env.example .env

# 编辑 .env 文件，设置你的密码
# USER_PASSWORD: RStudio 用户密码
# ROOT_PASSWORD: RStudio root 用户密码
```

3. 启动环境
```bash
# 构建并启动容器
docker compose up -d

# 查看容器状态
docker compose ps
```

#### 方式二：使用检查脚本（用于开发测试）

```bash
# 运行环境检查脚本
./scripts/01_check_container.sh
```

这个脚本会：
- 创建一个独立的测试容器
- 使用固定的容器名称 rstudio_test
- 适合开发和测试时使用

> 注意：不要同时使用两种方式，以避免端口冲突

### 访问 RStudio

- 打开浏览器访问 http://localhost:7878
- 使用以下凭据登录：
  - 用户名：当前系统用户名
  - 密码：在 .env 中设置的 USER_PASSWORD

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