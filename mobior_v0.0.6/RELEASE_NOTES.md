# MoBior v0.0.6 版本说明

发布日期：2026-06-08

## 版本定位

`mobior:v0.0.6` 是基于 `mobior:v0.0.5` 的环境升级版，目标是解决 v0.0.5 中 Docker 构建、R 包 ABI、Java、`nanonext`/`mlr3` 依赖链和包库分散的问题。

本版本的核心目标：

- 更新到 R 4.6.x。
- 使用 Bioconductor 3.23。
- 将 `lvyan_R_package_mobio`、`chaoliu_R_package_mobio`、`R_package_mobio`、metabo 文档和项目脚本依赖统一到 `R_packages_mobio`。
- 所有包在新 R 环境中重新安装和加载验证，不直接复用旧二进制包目录。

## 主要变化

### R 与 Bioconductor

- R 目标版本：R 4.6.x。
- Bioconductor 目标版本：3.23。
- 构建时会检查 R 版本，如果不是 R 4.6.x，构建直接失败。
- 默认构建 profile 为 `core`，只安装关键分析包和脚本依赖；`full` 或 `MOBIOR_INCLUDE_LIBRARY=1` 才安装旧包库全集。

### 包库融合

新增统一包库目录：

```text
/work/users/chaoliu/Test/20250320-mlr3-test/R_packages_mobio/site-library
```

新增 manifest：

- `mobior_v0.0.6/package_manifest.tsv`
- `R_packages_mobio/package_manifest.tsv`
- `R_packages_mobio/script_dependency_manifest.tsv`

当前 manifest 统计：

- 有效包名：712
- 项目脚本依赖记录：133
- core profile 包：66

包来源包括：

- `lvyan_R_package_mobio/library`
- `chaoliu_R_package_mobio/site-library`
- `R_package_mobio/site-library`
- `metabo_R_packagess.md`
- `metabo_database_download.md`
- `mobior_v0.0.5/R_scripts`
- 项目 `scripts/` 和 `lina_*/*/scripts/`

### Docker 构建修复前置

v0.0.5 中临时用于修复 Docker 问题的脚本不再保留：

- `build_with_retry.sh`
- `fix_network.sh`
- `fix_libstdc.sh`
- `diagnose.sh`
- `test_mirrors.sh`
- `testsh`
- `Dockerfile.lilei`

其中已验证有效的修复项已并入 v0.0.6 主 `Dockerfile`：

- `default-jre`
- `default-jdk`
- `cmake`
- `libmbedtls-dev`
- `libuv1-dev`
- `R CMD javareconf`

其中 `libuv1-dev` 用于解决 R 4.6 下 `fs` 2.1.0 配置阶段找不到 `libuv.pc`，否则会连锁导致 `sass`、`bslib`、`rmarkdown`、`ANCOMBC`、`mia` 等安装失败。

RStudio Server 重新下载改为可选：

```bash
--build-arg INSTALL_RSTUDIO=true
```

默认 `INSTALL_RSTUDIO=false`，避免 RStudio 下载源在构建中长时间阻塞。默认镜像保留基础镜像中的 RStudio 组件。

### 安装与检查脚本

新增：

- `generate_package_manifest.R`
- `install_mobior_v006_packages.R`
- `check_mobior_v006_packages.R`

检查逻辑使用 `loadNamespace()`，不只检查包目录是否存在。

## 已知特殊处理

### mimosa

GitHub 仓库为：

```text
borenstein-lab/mimosa2
```

但脚本中实际加载名为：

```r
library(mimosa)
```

因此 manifest 只保留加载名 `mimosa`；旧的 `mimosa2` 仓库名误报已移除。

### GitHub vendor fallback

Docker build 中 GitHub 实时下载偶发超时，尤其是 mlr3 扩展包链：

- `mlr3cmprsk`
- `mlr3proba`
- `mlr3extralearners`

v0.0.6 已将这些包的已验证安装目录加入 `vendor_packages`，构建时优先使用本地 fallback，并继续用 `loadNamespace()` 严格验证。

`MMUPHin` 已从 GitHub HEAD/fallback 改为 Bioconductor 3.23 正式包：

```text
MMUPHin 2.0.0
```

### KEGG.db

`metabo_R_packagess.md` 中提到的 `KEGG.db` 当前不是 CRAN/Bioconductor 标准安装包。它需要通过 `createKEGGdb` 生成或提供本地 `KEGG.db_*.tar.gz`。

当前标记为：

```text
local_or_generated
```

## 推荐构建命令

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test/mobior_v0.0.6
docker build --network=host -t mobior:v0.0.6 .
```

当前服务器 Docker 默认网络曾出现 DNS 解析失败，因此推荐保留 `--network=host`。

## 推荐包库融合命令

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test
mkdir -p R_packages_mobio/site-library

docker run --rm --network=host \
  --user "$(id -u):$(id -g)" \
  -e R_SITE_LIBRARY=/usr/local/lib/R/site-library \
  -e MOBIOR_PROFILE=core \
  -v "$PWD/R_packages_mobio/site-library:/usr/local/lib/R/site-library" \
  -v "$PWD:/workspace" \
  -w /workspace/mobior_v0.0.6 \
  mobior:v0.0.6 \
  Rscript install_mobior_v006_packages.R
```

## 推荐检查命令

```bash
docker run --rm --network=host \
  --user "$(id -u):$(id -g)" \
  -v "$PWD/R_packages_mobio/site-library:/usr/local/lib/R/site-library" \
  -v "$PWD:/workspace" \
  -w /workspace/mobior_v0.0.6 \
  mobior:v0.0.6 \
  Rscript check_mobior_v006_packages.R \
    --manifest=package_manifest.tsv \
    --site-lib=/usr/local/lib/R/site-library \
    --out=/workspace/R_packages_mobio/package_check.tsv \
    --profile=core \
    --strict=true
```

## 当前状态

已完成：

- v0.0.6 发布目录生成。
- R_scripts 迁移。
- package manifest 生成。
- 脚本依赖 manifest 生成。
- Dockerfile/entrypoint/docker-compose/README 生成。
- R 脚本语法检查通过。
- `mobior:v0.0.6` 镜像构建完成。
- 镜像 ID：`e0acbe8f4769`。
- 镜像大小：`7.47 GB`。
- R 运行时验证通过：`R 4.6.0 (2026-04-24)`。
- Bioconductor 运行时验证通过：`3.23`。
- core profile 严格加载检查通过：66 个包 OK。
- core profile 检查报告已写入：`R_packages_mobio/package_check_core_v0.0.6.tsv`。
- core 更新扫描完成：328 个已安装包，0 个 outdated。
- core 更新扫描报告已写入：`R_packages_mobio/package_update_scan_core_v0.0.6.tsv`。
- `nanonext` 已在构建中成功编译并链接 `mbedtls`。
- `fs` 已通过 `libuv1-dev` 修复，`sass/bslib/rmarkdown/ANCOMBC/mia` 依赖链已通过 core 构建。
- full overlay 安装和检查已完成：689 个包 OK，23 个包 MISSING，0 个 BROKEN。
- full profile 检查报告已写入：`R_packages_mobio/package_check_full_after_available_install_v0.0.6.tsv`。
- full 更新扫描完成：436 个已安装包，0 个 outdated。
- full 更新扫描报告已写入：`R_packages_mobio/package_update_scan_full_v0.0.6.tsv`。
- full 未解析分类报告已写入：`R_packages_mobio/full_unresolved_v0.0.6.tsv`。
- full 可安装性分类报告已写入：`R_packages_mobio/full_missing_availability_v0.0.6.tsv`。
- `MetaboAnalystR 4.3.0` 已通过直接 GitHub tarball 安装成功；失败原因由缺少 `qs2` 和 GitHub API DNS 超时修复为可复现安装路径。
- `qs2 0.2.2` 已安装，用于替代当前 R 4.6 下不可安装的旧 `qs`。
- `mimosa2` manifest 误报已修正：`borenstein-lab/mimosa2` 安装后的 R namespace 是 `mimosa`，不是 `mimosa2`。
- `mlr3cmprsk 0.0.5`、`mlr3proba 0.8.10`、`mlr3extralearners 1.5.2` 已通过 core strict check；本地 `vendor_packages` fallback 保证构建可复现。
- `MMUPHin 2.0.0` 已通过 core strict check，来源为 Bioconductor 3.23。
- full overlay 在新镜像下补齐 `cmprsk`、`diagram`、`lava`、`plotrix`、`polspline`、`prodlim`、`SQUAREM`、`timereg` 后，无 BROKEN 包，最终为 689 OK / 23 MISSING。
- `highs` 已从 `1.12.0-3` 更新到 CRAN 当前版本 `1.14.0-2`，full 更新扫描已清零。

## R 包和库是否还需要更新

结论：当前 v0.0.6 不需要继续更新 R 或 R 包库。

- R 已是本版本目标：`R 4.6.0 (2026-04-24)`。
- Bioconductor 已是对应版本：`3.23`。
- core 包库更新扫描：0 个 outdated。
- full 外部包库更新扫描：0 个 outdated。
- 剩余 23 个 MISSING 不是可直接从当前 CRAN/Bioconductor 更新解决的包；它们需要本地生成、上游兼容修复、旧环境兼容，或从 reproducible runtime 依赖中排除。

未解析项：

- `GseaVis`：GitHub HEAD 在 R 4.6/Bioc 3.23 下 lazy-load 失败，错误点为找不到 `get_organism`，需要 pin 兼容 commit 或等待上游修复。
- `KEGG.db`：不是当前 CRAN/Bioconductor 标准安装包，需要 `createKEGGdb` 生成或提供本地 `KEGG.db_*.tar.gz`。
- `qs`：当前 CRAN 源不可用，archive `qs 0.27.2` 与 R 4.6 内部 API 不兼容；建议迁移到 `qs2`。
- 其余多为旧包库带入的测试包、调试工具、生成包或无命名空间组件，详见 `R_packages_mobio/full_unresolved_v0.0.6.tsv`。
- 如必须使用新版 RStudio Server，可用 `--build-arg INSTALL_RSTUDIO=true` 单独验证该可选层。
