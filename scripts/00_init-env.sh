#!/bin/bash

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

# 获取当前用户信息
USER_ID=$(id -u)
GROUP_ID=$(id -g)
USER_NAME=$(whoami)
GROUP_NAME=$(id -gn)

# 设置默认密码
PASSWORD="MoBio888"
ROOT_PASSWORD="MoBio888"

# 选择镜像源
select_mirror

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

# 系统配置
DISABLE_AUTH=false
ROOT=TRUE

# R包路径
R_SITE_LIBRARY=/Users/colinliu/Desktop/20250211-mlr3/R_package_mobio/site-library/

# 端口映射
PORT=7878

# Docker镜像源配置
DOCKER_MIRROR=${DOCKER_MIRROR}
EOF

echo "已创建 .env 文件，配置信息如下："
echo "- 用户信息：${USER_NAME}(${USER_ID}):${GROUP_NAME}(${GROUP_ID})"
echo "- Docker镜像源：${DOCKER_MIRROR}"
echo "- RStudio端口：7878"