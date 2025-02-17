#!/bin/bash

# 在宿主机上获取用户信息并固定下来
HOST_USERNAME=$(whoami)
HOST_USERID=$(id -u)
HOST_GROUPID=$(id -g)
HOST_GROUPNAME=$(id -gn)

# 使用固定的用户信息
USERNAME=$HOST_USERNAME
USERID=$HOST_USERID
GROUPID=$HOST_GROUPID
GROUPNAME=$HOST_GROUPNAME
#PASSWORD=${PASSWORD:-"MoBio888"}
#ROOT_PASSWORD=${ROOT_PASSWORD:-"MoBio888"}

# 创建必要的文件
create_dockerfile() {
    cat << EOF > Dockerfile
FROM docker.1ms.run/rocker/rstudio:latest

# 定义构建参数并设置默认值
ARG GROUPID=${HOST_GROUPID}
ARG GROUPNAME=${HOST_GROUPNAME}
ARG USERNAME=${HOST_USERNAME}
ARG USERID=${HOST_USERID}

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

# 设置工作目录时使用明确的用户名
WORKDIR /home/${HOST_USERNAME}

# 设置 entrypoint，账户管理
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["/init"]
EOF
}

create_entrypoint() {
    cat << EOF > entrypoint.sh
#!/bin/bash

# 使用明确的变量名，不使用 \${USERNAME}
if ! getent group ${HOST_GROUPNAME} > /dev/null 2>&1; then
    groupadd -g ${HOST_GROUPID} ${HOST_GROUPNAME}
fi

useradd -m -u ${HOST_USERID} -g ${HOST_GROUPID} -s /bin/bash ${HOST_USERNAME} --non-unique

echo "${HOST_USERNAME}:\${PASSWORD}" | chpasswd
echo "root:\${ROOT_PASSWORD}" | chpasswd

echo "${HOST_USERNAME} ALL=(ALL) ALL" > /etc/sudoers.d/${HOST_USERNAME}
chmod 0440 /etc/sudoers.d/${HOST_USERNAME}

chown -R ${HOST_USERNAME}:${HOST_GROUPNAME} /home/${HOST_USERNAME}

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
        --build-arg USERNAME=$USERNAME \
        --build-arg USERID=$USERID \
        --build-arg GROUPID=$GROUPID \
        --build-arg GROUPNAME=$GROUPNAME \
        -t mobior .
fi

# 检查容器是否存在并运行
CONTAINER_ID=$(docker ps -a | grep mobior | awk '{print $1}')

if [ -z "$CONTAINER_ID" ]; then
    echo "容器不存在，正在创建..."
    # 检查端口是否被占用
    if command -v lsof >/dev/null 2>&1; then
        lsof -i :7878 || echo "端口 7878 可用"
    elif command -v netstat >/dev/null 2>&1; then
        netstat -an | grep 7878 || echo "端口 7878 可用"
    else
        echo "无法检查端口状态，请确保端口 7878 未被占用"
    fi

    # 读取密码
    USER_PASSWORD=$(grep USER_PASSWORD .env | cut -d '=' -f2)
    ROOT_PASSWORD=$(grep ROOT_PASSWORD .env | cut -d '=' -f2)

    # 创建并运行容器
    docker run -itd -p 7878:8787 --name rstudio_test \
        -e DISABLE_AUTH=false \
        -e USERID=$HOST_USERID \
        -e GROUPID=$HOST_GROUPID \
        -e USERNAME=$HOST_USERNAME \
        -e GROUPNAME=$HOST_GROUPNAME \
        -e PASSWORD="$USER_PASSWORD" \
        -e ROOT=TRUE \
        -e ROOT_PASSWORD="$ROOT_PASSWORD" \
        -v /Users/colinliu/Desktop/20250211-mlr3/R_package_mobio/site-library:/usr/local/lib/R/site-library/ \
        -v "$(pwd)":/home/$USERNAME/analysis/ \
        mobior

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

# 复制插件配置到容器
echo "正在复制VS Code和Cursor配置到容器..."
docker cp .vscode $CONTAINER_ID:/home/$USERNAME/
docker cp .cursor $CONTAINER_ID:/home/$USERNAME/

# 设置正确的权限
docker exec $CONTAINER_ID chown -R $USERNAME:$GROUPNAME /home/$USERNAME/.vscode
docker exec $CONTAINER_ID chown -R $USERNAME:$GROUPNAME /home/$USERNAME/.cursor

echo "容器已经准备就绪！"
echo "RStudio服务器地址: http://localhost:7878"
echo "用户名: $HOST_USERNAME"
echo "密码: $USER_PASSWORD" 