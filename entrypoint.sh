#!/bin/bash
set -euo pipefail

# 创建用户组（如果不存在）
getent group ${GROUP_NAME} > /dev/null 2>&1 || groupadd -g ${GROUP_ID} ${GROUP_NAME}

# 创建用户
useradd -m -u ${USER_ID} -g ${GROUP_ID} -s /bin/bash ${USER_NAME} --non-unique

# 设置密码
echo "${USER_NAME}:${PASSWORD}" | chpasswd
echo "root:${ROOT_PASSWORD}" | chpasswd

# 设置sudo权限
echo "${USER_NAME} ALL=(ALL) ALL" > /etc/sudoers.d/${USER_NAME}
chmod 0440 /etc/sudoers.d/${USER_NAME}

# 设置目录权限
chown -R ${USER_NAME}:${GROUP_NAME} /home/${USER_NAME}

log "初始化完成"
exec "$@" 
