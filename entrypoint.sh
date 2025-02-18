#!/bin/bash
set -euo pipefail

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
exec "$@"

# 启动 code-server
echo "code-server 路径: $(which code-server)"
su - ${USER_NAME} -c "code-server --bind-addr 0.0.0.0:8080 --auth password --password ${CODE_SERVER_PASSWORD:-MoBio888}" & 
