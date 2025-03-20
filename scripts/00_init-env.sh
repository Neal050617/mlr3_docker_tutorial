#!/bin/bash

# 检查是否使用非交互式模式
NON_INTERACTIVE=false
if [[ "$1" == "--non-interactive" ]]; then
    NON_INTERACTIVE=true
fi

# 定义可用的镜像源
MIRRORS=(
    "docker.io"
    "registry.cn-hangzhou.aliyuncs.com"
    "registry.docker-cn.com"
    "docker.mirrors.ustc.edu.cn"
    "docker.1ms.run"
)

# 选择镜像源
select_mirror() {
    if [[ "$NON_INTERACTIVE" == true ]]; then
        # 非交互式模式，使用默认镜像源
        DOCKER_MIRROR="${MIRRORS[0]}"
        echo "非交互式模式：使用默认镜像源 $DOCKER_MIRROR"
        return
    fi

    echo "=== Docker镜像源选择 ==="
    echo "可用的镜像源:"
    for i in "${!MIRRORS[@]}"; do
        echo "[$i] ${MIRRORS[$i]}"
    done
    echo
    echo "10秒后将自动选择默认镜像源[0]"
    
    # 使用 read 命令的 -t 参数设置超时
    read -t 10 -p "请选择镜像源编号 [0-$((${#MIRRORS[@]}-1))] (默认 0): " choice
    
    if [[ -z "$choice" ]]; then
        echo -e "\n超时或未输入，使用默认镜像源 ${MIRRORS[0]}"
        choice=0
    fi
    
    if [[ "$choice" =~ ^[0-9]+$ ]] && [ "$choice" -lt "${#MIRRORS[@]}" ]; then
        DOCKER_MIRROR="${MIRRORS[$choice]}"
        echo "已选择镜像源: $DOCKER_MIRROR"
    else
        echo "无效的选择，使用默认镜像源 ${MIRRORS[0]}"
        DOCKER_MIRROR="${MIRRORS[0]}"
    fi
}

# 处理路径配置
configure_paths() {
    local default_mount_path="$(PWD)"
    local default_lib_path="/usr/local/lib/R/site-library/"
    
    if [[ "$NON_INTERACTIVE" == true ]]; then
        MOUNT_PATH="$default_mount_path"
        LIB_PATH="$default_lib_path"
        echo "非交互式模式：使用默认挂载路径 ${MOUNT_PATH}"
        echo "非交互式模式：使用默认R包路径 ${LIB_PATH}"
        return
    fi

    # 提示用户是否修改挂载路径
    echo "当前挂载路径为: ${default_mount_path}"
    read -t 30 -p "是否修改挂载路径？[y/N] (30秒后默认不修改): " change_mount

    if [[ "$change_mount" =~ ^[Yy]$ ]]; then
        read -p "请输入新的挂载路径: " new_mount_path
        if [[ -d "$new_mount_path" ]]; then
            MOUNT_PATH="$new_mount_path"
            echo "挂载路径已修改为: ${MOUNT_PATH}"
        else
            MOUNT_PATH="$default_mount_path"
            echo "路径不存在，保持默认挂载路径: ${MOUNT_PATH}"
        fi
    else
        MOUNT_PATH="$default_mount_path"
        echo "保持默认挂载路径: ${MOUNT_PATH}"
    fi

    # 提示用户是否修改R包路径
    echo "当前R包路径为: ${default_lib_path}"
    read -t 30 -p "是否修改R包路径？[y/N] (30秒后默认不修改): " change_lib

    if [[ "$change_lib" =~ ^[Yy]$ ]]; then
        read -p "请输入新的R包路径: " new_lib_path
        if [[ -d "$new_lib_path" ]]; then
            LIB_PATH="$new_lib_path"
            echo "R包路径已修改为: ${LIB_PATH}"
        else
            LIB_PATH="$default_lib_path"
            echo "路径不存在，保持默认R包路径: ${LIB_PATH}"
        fi
    else
        LIB_PATH="$default_lib_path"
        echo "保持默认R包路径: ${LIB_PATH}"
    fi
}

# 获取当前用户信息
USER_ID=$(id -u)
GROUP_ID=$(id -g)
USER_NAME=$(whoami)
GROUP_NAME=$(id -gn)

# 设置默认密码
PASSWORD="MoBio888"
ROOT_PASSWORD="MoBio888"
CODE_SERVER_PASSWORD="MoBio888"

# 选择镜像源
select_mirror

# 配置路径
configure_paths

# 创建.env文件
cat > .env << EOF
# 用户和组配置（自动获取当前用户信息）
USER_ID=${USER_ID}
GROUP_ID=${GROUP_ID}
USER_NAME=${USER_NAME}
GROUP_NAME=${GROUP_NAME}

# 密码配置
PASSWORD=${PASSWORD}
ROOT_PASSWORD=${ROOT_PASSWORD}
CODE_SERVER_PASSWORD=${CODE_SERVER_PASSWORD}

# 系统配置
DISABLE_AUTH=false
ROOT=TRUE

# R包路径
R_SITE_LIBRARY=${LIB_PATH}

# 端口映射
PORT=7878

# 挂载路径配置
MOUNT_PATH=${MOUNT_PATH}

# Docker镜像源配置
DOCKER_MIRROR=${DOCKER_MIRROR}
EOF

echo "已创建 .env 文件，配置信息如下："
echo "- 用户信息：${USER_NAME}(${USER_ID}):${GROUP_NAME}(${GROUP_ID})"
echo "- 挂载路径：${MOUNT_PATH}"
echo "- R包路径：${LIB_PATH}"
echo "- Docker镜像源：${DOCKER_MIRROR}"
echo "- RStudio端口：7878"