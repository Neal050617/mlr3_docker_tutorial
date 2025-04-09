#!/bin/bash
set -euo pipefail

# 安装git和zsh (仅在首次运行时安装)
if [ ! -f "/.git_zsh_installed" ]; then
    echo "安装git和zsh..."
    apt-get update
    # 尝试安装zsh
    apt-get install -y git zsh gh || true
    # 不安装fonts-powerline
    touch "/.git_zsh_installed"
fi

# 调整 UID 范围限制
echo "调整 UID 范围限制"
sed -i 's/^UID_MIN.*/UID_MIN 500/' /etc/login.defs
sed -i 's/^UID_MAX.*/UID_MAX 60000/' /etc/login.defs

# 创建用户组（如果不存在）
if ! getent group ${GROUP_NAME} > /dev/null 2>&1; then
    echo "创建用户组 ${GROUP_NAME}"
    groupadd -g ${GROUP_ID} ${GROUP_NAME} || true
fi

# 创建用户（如果不存在）
if ! id ${USER_NAME} > /dev/null 2>&1; then
    echo "创建用户 ${USER_NAME}"
    useradd -m -u ${USER_ID} -g ${GROUP_ID} -s /bin/bash ${USER_NAME} --non-unique || true
fi

# 设置密码
echo "设置用户密码"
echo "${USER_NAME}:${PASSWORD}" | chpasswd
echo "root:${ROOT_PASSWORD}" | chpasswd

# 设置sudo权限
echo "配置 sudo 权限"
echo "${USER_NAME} ALL=(ALL) ALL" > /etc/sudoers.d/${USER_NAME}
chmod 0440 /etc/sudoers.d/${USER_NAME}

# 设置工作目录权限
if [ -d "/home/${USER_NAME}/analysis" ]; then
    echo "设置工作目录权限"
    find /home/${USER_NAME}/analysis -type d -exec chmod 755 {} \;
    find /home/${USER_NAME}/analysis -type f -exec chmod 644 {} \;
    chown -R ${USER_NAME}:${GROUP_NAME} /home/${USER_NAME}/analysis
fi

# 设置 VS Code 配置目录权限
echo "设置 VS Code 配置目录权限"
mkdir -p /home/${USER_NAME}/.local/share/code-server
mkdir -p /home/${USER_NAME}/.vscode-server
chown -R ${USER_NAME}:${GROUP_NAME} /home/${USER_NAME}
chown -R ${USER_NAME}:${GROUP_NAME} /home/${USER_NAME}/.local
chown -R ${USER_NAME}:${GROUP_NAME} /home/${USER_NAME}/.vscode-server

echo "验证目录权限:"
ls -ld /home/${USER_NAME}/.vscode-server
ls -ld /home/${USER_NAME}/.local/share/code-server

echo "初始化完成"

# 配置 R 库路径
mkdir -p /usr/local/lib/R/site-library
chown -R ${USER_NAME}:${GROUP_NAME} /usr/local/lib/R/site-library

# 设置默认umask，确保新建文件权限为664，目录为775
echo "设置默认umask为0002"
echo "umask 0002" >> /home/${USER_NAME}/.bashrc
echo "umask 0002" >> /home/${USER_NAME}/.profile

# 创建或更新 .Rprofile
cat > /home/${USER_NAME}/.Rprofile << EOF
.libPaths("/usr/local/lib/R/site-library")
# 设置R创建文件的默认权限
Sys.umask(mode="0002")
EOF

chown ${USER_NAME}:${GROUP_NAME} /home/${USER_NAME}/.Rprofile

# 安装 miniforge 和 radian（以用户身份）
MINIFORGE_INSTALLED="/home/${USER_NAME}/.miniforge_installed"
if [ ! -f "$MINIFORGE_INSTALLED" ]; then
    echo "以用户身份安装 miniforge 和 radian..."
    su - ${USER_NAME} -c "wget -q -c https://mirrors.tuna.tsinghua.edu.cn/github-release/conda-forge/miniforge/LatestRelease/Miniforge3-Linux-x86_64.sh && \
        bash Miniforge3-Linux-x86_64.sh -b && \
        rm Miniforge3-Linux-x86_64.sh && \
        ~/miniforge3/bin/mamba install -y -c conda-forge radian && \
        ~/miniforge3/bin/mamba clean -a -y"
    
    # 添加环境变量到用户的 .bashrc
    echo "配置 conda 环境..."
    echo 'export PATH="$HOME/miniforge3/bin:$PATH"' >> /home/${USER_NAME}/.bashrc
    echo 'source ~/miniforge3/etc/profile.d/conda.sh' >> /home/${USER_NAME}/.bashrc
    echo 'conda activate base' >> /home/${USER_NAME}/.bashrc
    
    # 创建标记文件以避免重复安装
    touch "$MINIFORGE_INSTALLED"
    chown ${USER_NAME}:${GROUP_NAME} "$MINIFORGE_INSTALLED"
fi

echo "初始化完成"

# 启动 code-server
# echo "code-server 路径: $(which code-server)"
# su - ${USER_NAME} -c "code-server --bind-addr 0.0.0.0:8080 --auth password --password ${CODE_SERVER_PASSWORD:-MoBio888}" & 

# 设置RStudio默认用户
echo "设置RStudio默认用户为 ${USER_NAME}"
echo "auth-default-user=${USER_NAME}" >> /etc/rstudio/rserver.conf

# 如果需要锁定只允许特定用户登录，可以添加
echo "auth-required-user-group=${GROUP_NAME}" >> /etc/rstudio/rserver.conf

# 确保RStudio会话目录权限正确
mkdir -p /var/lib/rstudio-server
chmod 1777 /var/lib/rstudio-server

# 确保所有服务以非root用户运行
if [ "${ROOT}" = "TRUE" ]; then
  # 启动 RStudio 服务（以root用户，但服务会自动切换到用户）
  exec /init
else
  # 修改/init脚本权限
  chmod +x /init
  # 以非root用户启动所有服务
  exec setpriv --reuid=${USER_ID} --regid=${GROUP_ID} --init-groups /init
fi
