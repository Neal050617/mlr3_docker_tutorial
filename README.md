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

### 访问 code-server

- 打开浏览器访问 http://localhost:8080
- 使用以下凭据登录：
  - 密码：${CODE_SERVER_PASSWORD:-MoBio888}（默认密码，可在.env文件中修改）

#### code-server 安装说明

由于网络原因，我们使用本地脚本安装 code-server：

```bash
# 脚本位置
scripts/code_server_install.sh

# 安装参数
--method=standalone --prefix=/usr/local
```

如果安装失败，可以尝试以下解决方案：
1. 检查网络连接
2. 使用代理设置：
   ```bash
   export https_proxy=http://your-proxy:port
   export http_proxy=http://your-proxy:port
   ```
3. 手动下载安装包：
   ```bash
   # 从GitHub下载指定版本
   curl -fsSL https://ghproxy.com/https://github.com/coder/code-server/releases/download/v4.19.1/code-server_4.19.1_amd64.deb -o code-server.deb
   
   # 安装
   sudo dpkg -i code-server.deb
   ```

#### 维护说明

code-server 相关文件位置：
- 可执行文件：`/usr/local/bin/code-server`
- 配置文件：`~/.local/share/code-server/User/settings.json`
- 扩展目录：`~/.local/share/code-server/extensions`

如需重新安装：
```bash
# 清理旧文件
rm -rf ~/.local/share/code-server
rm -rf ~/.cache/code-server

# 重新运行安装脚本
/tmp/code_server_install.sh --method=standalone --prefix=/usr/local
```

功能特点：
1. 与容器环境深度集成
2. 支持 VS Code 扩展的自动安装
3. 终端直接访问容器环境
4. 文件浏览器可直接修改容器内文件

### 修复脚本目录权限

如果 `scripts` 目录中的文件缺少执行权限，可以通过以下步骤修复：

1. 切换到你的用户：
   ```bash
   su - ${USER_NAME}
   ```

2. 进入工作目录：
   ```bash
   cd ~/analysis
   ```

3. 修复 `scripts` 目录的权限：
   ```bash
   chmod -R 755 scripts/
   ```

这些命令会：
- 确保你以正确的用户身份操作
- 进入项目工作目录
- 为 `scripts` 目录及其中的所有文件设置正确的执行权限（755）

### 配置说明

环境配置存储在 .env 文件中，包括：
- 用户信息（USER_ID, GROUP_ID, USER_NAME, GROUP_NAME）
- 密码设置（PASSWORD, ROOT_PASSWORD）
- 系统配置（DISABLE_AUTH, ROOT）
- R包路径（R_SITE_LIBRARY）
- 端口映射（PORT）
- Docker镜像源（DOCKER_MIRROR）

### VS Code 开发环境设置

在容器启动后，你可以使用 VS Code 进行远程开发：

1. **基础扩展** (自动安装):
   - R 语言支持 (REditorSupport.r)
   - R 增强功能 (Ikuyadeu.r)
   - Quarto 文档支持 (quarto.quarto)
   - GitHub Copilot
   - R LSP 支持

2. 连接到容器：
   - 打开 VS Code
   - 按下 `F1` 或 `Cmd/Ctrl + Shift + P`
   - 输入并选择 `Dev Containers: Attach to Running Container`
   - 选择 `rstudio_test` 容器

3. 切换到正确的用户：
   - VS Code 默认以 root 用户连接容器
   - 使用以下命令切换到你的用户：
```bash
# 切换到你的用户（与 RStudio 使用相同的用户）
su - ${USER_NAME}

```

4. **个性化扩展** (可选):
```bash
# 导入你的个性化扩展配置
./scripts/02_manage_settings.sh import
```
   这会安装你之前通过 `./scripts/02_manage_settings.sh export` 导出的扩展列表
   
```bash
# 现在可以运行设置脚本
./scripts/02_manage_settings.sh import
./scripts/03_verify_environment.sh
```

这些脚本会：
- 同步你的 VS Code 设置到容器
- 安装必要的 VS Code 扩展
- 验证 R 语言环境
- 检查必要的开发工具

现在你可以：
- 在 VS Code 中编辑代码
- 使用集成终端
- 运行 R 脚本
- 使用 Git 进行版本控制

注意：这些设置脚本需要在 VS Code 连接到容器后，在容器内的终端中运行。

### VS Code 数据持久化

VS Code 的服务器和扩展数据会通过 Docker 命名卷持久化保存：

1. VS Code Server：保存在 `vscode-server` 卷中
2. VS Code 扩展：保存在 `vscode-extensions` 卷中

这意味着：
- VS Code Server 只需要下载一次
- 扩展只需要安装一次
- 容器重启后这些配置会保留
- 开发环境启动更快

如果需要清理这些数据：
```bash
# 删除特定的命名卷
docker volume rm mobior_vscode-server mobior_vscode-extensions

# 或者删除所有未使用的卷
docker volume prune
```

注意：清理数据后，下次连接容器时需要重新下载 VS Code Server 和安装扩展。

如果发现某些本地安装的扩展没有在容器中自动安装，可以：
```bash
# 1. 导出本地扩展列表到项目
./scripts/02_manage_settings.sh export

# 2. 检查 .vscode/extensions.json 是否包含需要的扩展
cat .vscode/extensions.json

# 3. 在容器中重新导入扩展
./scripts/02_manage_settings.sh import
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
          docker compose up -d --build
          docker compose ps
          docker compose down
```

这个工作流会：
1. 检出代码
2. 构建并启动环境（使用 --build 确保重新构建）
3. 验证容器状态
4. 停止并清理容器 

### 结果输出说明

最终生成的Excel报告包含以下工作表：

| 工作表名称          | 内容描述                                                                 |
|---------------------|--------------------------------------------------------------------------|
| 模型总览            | 包含整体性能指标、样本量等关键信息                                       |
| 特征分析            | 列出所有保留特征的系数值及相对重要性百分比                               |
| 参数配置            | 显示最优超参数及其技术说明                                               |
| 每个fold的最佳参数  | 记录交叉验证各fold的参数选择详情                                         |
| 平均性能指标        | 展示所有评估指标的统计分布（均值、标准差等）                             |

生成命令：
```bash
Rscript data_for_analysis/20250211-mlr-JINYU/test2.R
```

输出位置：
```bash
ls -lh model_report.xlsx
-rw-r--r--  1 user  staff   128K Jun 15 10:30 model_report.xlsx
```

各工作表数据来源对应代码段：
```r
# 模型总览表
model_summary <- data.table(项目 = c("交叉验证AUC", ...), 值 = c(...))

# 特征分析表  
feature_table <- coeffs_dt[, .(特征名称 = feature, 系数值 = ...)]

# 参数配置表
param_table <- data.table(参数 = names(best_params), 描述 = ...)

# fold参数表
fold_params <- map_dfr(seq_len(rr$iters), function(i) { ... })

# 性能指标表
aggregate_scores <- performance_data |> group_by(metric) |> summarise(...)
```
