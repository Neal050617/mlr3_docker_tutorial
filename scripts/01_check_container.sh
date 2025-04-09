#!/bin/bash

# 检查.env文件是否存在
if [ ! -f .env ]; then
    echo ".env 文件不存在，请先运行 ./scripts/00_init-env.sh"
    exit 1
fi

# 从.env文件读取配置
source .env

# 创建必要的文件
create_dockerfile() {
    cat << EOF > Dockerfile
# 必须在 FROM 之前声明 DOCKER_MIRROR
ARG DOCKER_MIRROR

FROM \${DOCKER_MIRROR}/rocker/rstudio:4.4.2

# 定义构建参数
ARG USER_ID
ARG GROUP_ID
ARG USER_NAME
ARG GROUP_NAME
ARG PASSWORD
ARG ROOT_PASSWORD
ARG ROOT

# 使用清华镜像源
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
ENV PATH="/root/.cargo/bin:\${PATH}"

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
WORKDIR /home/\${USER_NAME}

# 设置 entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["/init"]
EOF
}

create_entrypoint() {
    cat << EOF > entrypoint.sh
#!/bin/bash
set -euo pipefail

# 创建用户组（如果不存在）
getent group \${GROUP_NAME} > /dev/null 2>&1 || groupadd -g \${GROUP_ID} \${GROUP_NAME}

# 创建用户
useradd -m -u \${USER_ID} -g \${GROUP_ID} -s /bin/bash \${USER_NAME} --non-unique

# 设置密码
echo "\${USER_NAME}:\${PASSWORD}" | chpasswd
echo "root:\${ROOT_PASSWORD}" | chpasswd

# 设置sudo权限
echo "\${USER_NAME} ALL=(ALL) ALL" > /etc/sudoers.d/\${USER_NAME}
chmod 0440 /etc/sudoers.d/\${USER_NAME}

# 设置目录权限
chown -R \${USER_NAME}:\${GROUP_NAME} /home/\${USER_NAME}

exec "\$@"
EOF
    chmod +x entrypoint.sh
}

# 检查mobior镜像是否存在
if ! docker images | grep -q "mobior"; then
    echo "mobior镜像不存在，正在创建必要文件..."
    create_dockerfile
    create_entrypoint
    
    echo "正在构建镜像..."
    docker build \
        --build-arg DOCKER_MIRROR="${DOCKER_MIRROR}" \
        --build-arg USER_ID="${USER_ID}" \
        --build-arg GROUP_ID="${GROUP_ID}" \
        --build-arg USER_NAME="${USER_NAME}" \
        --build-arg GROUP_NAME="${GROUP_NAME}" \
        --build-arg PASSWORD="${PASSWORD}" \
        --build-arg ROOT_PASSWORD="${ROOT_PASSWORD}" \
        --build-arg ROOT="${ROOT}" \
        -t mobior:v0.0.1 .
fi

# 检查容器是否存在并运行
CONTAINER_ID=$(docker ps -a | grep rstudio_test | awk '{print $1}')

if [ -z "$CONTAINER_ID" ]; then
    echo "容器不存在，正在创建..."
    # 检查端口是否被占用
    if command -v lsof >/dev/null 2>&1; then
        lsof -i :${PORT} || echo "端口 ${PORT} 可用"
    elif command -v netstat >/dev/null 2>&1; then
        netstat -an | grep ${PORT} || echo "端口 ${PORT} 可用"
    else
        echo "无法检查端口状态，请确保端口 ${PORT} 未被占用"
    fi

    # 创建并运行容器
    docker run -d \
        -p ${PORT}:8787 \
        --name rstudio_test \
        -e DISABLE_AUTH=${DISABLE_AUTH} \
        -e USER_ID="${USER_ID}" \
        -e GROUP_ID="${GROUP_ID}" \
        -e USER_NAME="${USER_NAME}" \
        -e GROUP_NAME="${GROUP_NAME}" \
        -e PASSWORD="${PASSWORD}" \
        -e ROOT="${ROOT}" \
        -e ROOT_PASSWORD="${ROOT_PASSWORD}" \
        -v "${R_SITE_LIBRARY}":/usr/local/lib/R/site-library/ \
        -v "$(pwd)":/home/${USER_NAME}/analysis/ \
        mobior:v0.0.1

    # 获取新创建的容器ID
    CONTAINER_ID=$(docker ps -a | grep rstudio_test | awk '{print $1}')
else
    # 检查容器是否在运行
    if [ "$(docker inspect -f '{{.State.Running}}' $CONTAINER_ID)" = "false" ]; then
        echo "容器存在但未运行，正在启动..."
        docker start $CONTAINER_ID
    fi
fi

# 等待容器完全启动
echo "等待容器启动..."
sleep 5

echo "容器已经准备就绪！"
echo "RStudio服务器地址: http://localhost:${PORT}"
echo "用户名: ${USER_NAME}"
echo "密码: ${PASSWORD}" 