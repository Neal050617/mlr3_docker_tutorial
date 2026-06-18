# MoBior v0.0.6

`mobior:v0.0.6` 基于 `mobior:v0.0.5`，但会升级到 R 4.6.x、Bioconductor 3.23，并清空旧 R 包库后重新安装统一依赖。

默认 `MOBIOR_PROFILE=core` 只安装关键分析包和脚本依赖；旧包库全集保留在 `package_manifest.tsv`，使用 `MOBIOR_PROFILE=full` 或 `MOBIOR_INCLUDE_LIBRARY=1` 时才安装。

## 与 v0.0.5 的主要差异

- 不再保留 `build_with_retry.sh`, `fix_network.sh`, `fix_libstdc.sh`, `diagnose.sh`, `test_mirrors.sh`, `testsh`, `Dockerfile.lilei`。
- Java/JDK、`cmake`、`libmbedtls-dev`、`libuv1-dev` 等已知修复项已并入主 `Dockerfile`。
- 所有 R 包从 `package_manifest.tsv` 安装并用 `loadNamespace()` 检查。
- 统一外部包库目录为 `../R_packages_mobio/site-library`。
- 默认跳过 RStudio Server 重新下载，避免可选下载源阻塞镜像构建；需要重装时显式设置 `INSTALL_RSTUDIO=true`。
- `mlr3cmprsk`、`mlr3proba`、`mlr3extralearners` 提供 `vendor_packages` fallback，避免 Docker build 因 GitHub 实时下载超时失败。
- `MMUPHin` 已改为 Bioconductor 3.23 正式包 `2.0.0`，不再依赖 GitHub HEAD fallback。

## 构建

当前服务器 Docker 默认网络 DNS 曾失败，建议使用 host 网络构建：

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test/mobior_v0.0.6
docker build --network=host -t mobior:v0.0.6 .
```

如需完整扩展包：

```bash
docker build --network=host --build-arg MOBIOR_PROFILE=full -t mobior:v0.0.6-full .
```

当前 Dockerfile 默认 `MOBIOR_PROFILE=core`。`full` 会同时纳入旧包库全集，编译时间明显更长，建议 core 验证通过后再扩展。

当前已验证：

- 镜像：`mobior:v0.0.6`
- 镜像 ID：`e0acbe8f4769`
- 镜像大小：`7.47 GB`
- R：`R 4.6.0 (2026-04-24)`
- Bioconductor：`3.23`
- package manifest：712 个包
- core profile：66 个包严格加载检查通过
- 检查报告：`../R_packages_mobio/package_check_core_v0.0.6.tsv`
- core 更新扫描：328 个已安装包，0 个 outdated
- core 更新报告：`../R_packages_mobio/package_update_scan_core_v0.0.6.tsv`
- full overlay：689 个包可加载，23 个旧包库/特殊包未解析，0 个 BROKEN
- full 检查报告：`../R_packages_mobio/package_check_full_after_available_install_v0.0.6.tsv`
- full 更新扫描：436 个已安装包，0 个 outdated
- full 更新报告：`../R_packages_mobio/package_update_scan_full_v0.0.6.tsv`
- full 未解析报告：`../R_packages_mobio/full_unresolved_v0.0.6.tsv`

如需在构建时尝试重装 RStudio Server：

```bash
docker build --network=host \
  --build-arg INSTALL_RSTUDIO=true \
  -t mobior:v0.0.6-rstudio .
```

## 安装统一包库 core

新增或调整 R 包时，先看 `PACKAGE_INSTALL_RUNBOOK.md`；不要从本地 macOS arm64 复制 R 包库到服务器 Linux amd64 运行环境。

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

## 安装统一包库 full

旧包库全集融合到 `R_packages_mobio/site-library` 时使用。推荐使用已针对 full overlay 调整过的安装脚本，它会设置可写 `HOME`、用户级 Rust、`R_LIBS*` 子进程继承，并补装 `qs2` 等 R 4.6 依赖：

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test
mkdir -p R_packages_mobio/site-library

docker run --rm --network=host \
  --user "$(id -u):$(id -g)" \
  -e R_SITE_LIBRARY=/opt/mobior-full/site-library \
  -e NCPUS=1 \
  -e FULL_INSTALL_PASSES=3 \
  -v "$PWD/R_packages_mobio/site-library:/opt/mobior-full/site-library" \
  -v "$PWD:/workspace" \
  -w /workspace/mobior_v0.0.6 \
  mobior:v0.0.6 \
  Rscript install_full_available_v006_packages.R
```

当前 full overlay 已完成验证：`689 OK / 23 MISSING / 0 BROKEN`。23 个未解析项中多数来自旧包库的测试包、调试工具或无命名空间组件；业务相关的特殊项主要是 `GseaVis`、`KEGG.db` 和旧 `qs`。详细分类见 `../R_packages_mobio/full_unresolved_v0.0.6.tsv`。

当前 `R_packages_mobio/site-library` 已补齐 `cmprsk`、`prodlim`、`timereg` 等 full 依赖，并把 `highs` 更新到 CRAN 当前版本 `1.14.0-2`。full 更新扫描结果为 0 个 outdated。

## 检查 core

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

检查旧包库全集时使用：

```bash
docker run --rm --network=host \
  -v "$PWD/R_packages_mobio/site-library:/opt/mobior-full/site-library" \
  -v "$PWD:/workspace" \
  -w /workspace \
  -e R_SITE_LIBRARY=/opt/mobior-full/site-library \
  mobior:v0.0.6 \
  Rscript /opt/mobior/check_mobior_v006_packages.R \
    --manifest=/workspace/mobior_v0.0.6/package_manifest.tsv \
    --site-lib=/opt/mobior-full/site-library \
    --out=/workspace/R_packages_mobio/package_check_full_after_available_install_v0.0.6.tsv \
    --profile=full \
    --strict=false
```

如果要把 23 个未解析项视为失败，可把 `--strict=false` 改为 `--strict=true`。

## RStudio

默认构建不重新下载 RStudio Server，保留基础镜像中的 RStudio 组件。启动命令保持不变：

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test/mobior_v0.0.6
docker compose up -d rstudio
```

默认访问端口：

```text
http://localhost:7878
```

默认密码：

```text
MoBio888
```

## 重新生成 manifest

```bash
cd /work/users/chaoliu/Test/20250320-mlr3-test/mobior_v0.0.6
Rscript generate_package_manifest.R
```

生成文件：

- `package_manifest.tsv`
- `../R_packages_mobio/package_manifest.tsv`
- `../R_packages_mobio/script_dependency_manifest.tsv`
