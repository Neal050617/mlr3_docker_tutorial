# 必须在 FROM 之前声明 DOCKER_MIRROR
ARG DOCKER_MIRROR

FROM ${DOCKER_MIRROR}/rocker/rstudio:4.4.2

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
    jq \
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
ENV RUSTUP_DIST_SERVER="https://mirrors.ustc.edu.cn/rust-static"     RUSTUP_UPDATE_ROOT="https://mirrors.ustc.edu.cn/rust-static/rustup"

# 安装 Rustup 和 Cargo
RUN curl --proto '=https' --tlsv1.2 -sSf https://mirrors.ustc.edu.cn/rust-static/rustup/rustup-init.sh | sh -s -- -y
ENV PATH="/root/.cargo/bin::/Applications/quarto/bin:/Library/Frameworks/Python.framework/Versions/3.11/bin:/usr/local/bin:/usr/local/sbin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/opt/X11/bin:/usr/local/share/dotnet:~/.dotnet/tools:/Applications/quarto/bin:/Library/Frameworks/Python.framework/Versions/3.11/bin:/Applications/Visual Studio Code.app/Contents/Resources/app/bin:/Users/colinliu/Library/Application Support/Cursor/User/globalStorage/github.copilot-chat/debugCommand:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

# 配置 Cargo 使用中科大镜像源
RUN mkdir -p ~/.cargo &&     echo '[source.crates-io]' > ~/.cargo/config &&     echo 'registry = "https://github.com/rust-lang/crates.io-index"' >> ~/.cargo/config &&     echo 'replace-with = "ustc"' >> ~/.cargo/config &&     echo '[source.ustc]' >> ~/.cargo/config &&     echo 'registry = "git://mirrors.ustc.edu.cn/crates.io-index"' >> ~/.cargo/config

# 复制 entrypoint 脚本
COPY entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/entrypoint.sh

# 设置工作目录
WORKDIR /home/${USERNAME}

# 设置 entrypoint，账户管理
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["/init"]
