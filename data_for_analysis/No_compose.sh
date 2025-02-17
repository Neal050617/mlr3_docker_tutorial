
cat << 'EOF' > Dockerfile
FROM docker.1ms.run/rocker/rstudio:latest

# 定义构建参数
ARG GROUPID
ARG GROUPNAME
ARG USERNAME
ARG USERID

# 使用清华镜像源替换默认源（Ubuntu 24.04 Noble）
RUN echo '\
deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ noble main restricted universe multiverse\n\
deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ noble-updates main restricted universe multiverse\n\
deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ noble-backports main restricted universe multiverse\n\
deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ noble-security main restricted universe multiverse\n\
' > /etc/apt/sources.list

# 更新软件包并安装系统依赖
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    build-essential \
    libxml2-dev \
    libcurl4-openssl-dev \
    libglpk-dev \
    libnetcdf-dev \
    libssl-dev \
    zlib1g-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libgsl0-dev \
    gdebi-core \
    && apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# 设置 RUSTUP_DIST_SERVER 和 RUSTUP_UPDATE_ROOT 环境变量
ENV RUSTUP_DIST_SERVER="https://mirrors.ustc.edu.cn/rust-static" \
    RUSTUP_UPDATE_ROOT="https://mirrors.ustc.edu.cn/rust-static/rustup"

# 安装 Rustup 和 Cargo
RUN curl --proto '=https' --tlsv1.2 -sSf https://mirrors.ustc.edu.cn/rust-static/rustup/rustup-init.sh | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# 配置 Cargo 使用中科大镜像源
RUN mkdir -p ~/.cargo && \
    echo '[source.crates-io]' > ~/.cargo/config && \
    echo 'registry = "https://github.com/rust-lang/crates.io-index"' >> ~/.cargo/config && \
    echo 'replace-with = "ustc"' >> ~/.cargo/config && \
    echo '[source.ustc]' >> ~/.cargo/config && \
    echo 'registry = "git://mirrors.ustc.edu.cn/crates.io-index"' >> ~/.cargo/config

# 复制 entrypoint 脚本
COPY entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh

# 设置工作目录
WORKDIR /home/${USERNAME}

# 设置 entrypoint，账户管理
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["/init"]
EOF

cat << 'EOF' > entrypoint.sh
#!/bin/bash

# 如果组不存在则创建
if ! getent group ${GROUPNAME} > /dev/null 2>&1; then
    groupadd -g ${GROUPID} ${GROUPNAME}
fi

# 创建用户
useradd -m -u ${USERID} -g ${GROUPID} -s /bin/bash ${USERNAME} --non-unique

# 设置密码
echo "${USERNAME}:${PASSWORD}" | chpasswd
echo "root:${ROOT_PASSWORD}" | chpasswd

# 设置 sudo 权限
echo "${USERNAME} ALL=(ALL) ALL" > /etc/sudoers.d/${USERNAME}
chmod 0440 /etc/sudoers.d/${USERNAME}

# 确保用户目录权限正确
chown -R ${USERNAME}:${GROUPNAME} /home/${USERNAME}

# 执行原始的 init 命令
exec "$@" 
EOF

# 构建镜像
docker build \
  --build-arg USERNAME=$(whoami) \
  --build-arg USERID=$(id -u) \
  --build-arg GROUPID=$(id -g) \
  --build-arg GROUPNAME=$(id -gn) \
  -t mobior .


# 检查端口是否被占用（通用命令）
if command -v lsof >/dev/null 2>&1; then
    lsof -i :7878 || echo "端口 7878 可用"
elif command -v netstat >/dev/null 2>&1; then
    netstat -an | grep 7878 || echo "端口 7878 可用"
else
    echo "无法检查端口状态，请确保端口 7878 未被占用"
fi

# R包挂载过来了，这样包就不用重复安装了
# 其他路径自己挂载哈，尤其是kegg背景数据
docker run -itd -p 7878:8787 --name rstudio_test \
  -e DISABLE_AUTH=false \
  -e USERID=$(id -u) \
  -e GROUPID=$(id -g) \
  -e USERNAME=$(whoami) \
  -e GROUPNAME=$(id -gn) \
  -e PASSWORD=$(grep USER_PASSWORD .env | cut -d '=' -f2) \
  -e ROOT=TRUE \
  -e ROOT_PASSWORD=$(grep ROOT_PASSWORD .env | cut -d '=' -f2) \
  -v /Users/colinliu/Desktop/20250211-mlr3/R_package_mobio/site-library:/usr/local/lib/R/site-library/ \
  -v "$(pwd)":/home/$(whoami)/analysis/ \
  mobior
