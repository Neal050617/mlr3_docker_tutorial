#!/bin/bash

# 获取当前宿主机用户信息
USER_ID=$(id -u)
GROUP_ID=$(id -g)
USER_NAME=$(whoami)
GROUP_NAME=$(id -gn | awk '{print $1}') # 获取第一个组名作为主要组名
PASSWORD="MoBio888" # 设置一个默认密码，可以根据需要修改
# PORT="8787" # 移除端口定义，不再需要端口映射
R_SITE_LIBRARY="/work/users/chaoliu/R_package_mobio/site-library" # R 包库路径，可以根据需要修改

# 检查 R_site_library 目录是否存在，不存在则创建
# 检查并创建 R 包库目录，如果目录不存在则创建，并设置适当的权限
if [ ! -d "${R_SITE_LIBRARY}" ]; then
    mkdir -p "${R_SITE_LIBRARY}"
    chmod 777 "${R_SITE_LIBRARY}"  # 设置目录权限为755，确保用户可以访问
fi

# Docker run 命令，创建用完即删的容器 rstudio_modio，移除端口映射
docker run -itd \
    --name rstudio_mobio \
    -e DISABLE_AUTH=false \
    -e USER_ID="${USER_ID}" \
    -e GROUP_ID="${GROUP_ID}" \
    -e USER_NAME="${USER_NAME}" \
    -e GROUP_NAME="${GROUP_NAME}" \
    -e PASSWORD="${PASSWORD}" \
    -e ROOT=true \
    -e ROOT_PASSWORD="${PASSWORD}" \
    -v "${R_SITE_LIBRARY}":/usr/local/lib/R/site-library/ \
    -v "$(pwd)":/home/${USER_NAME}/analysis/ \
    docker.1ms.run/mobior:v0.0.1
# --rm
    # -p ${PORT}:8787 # 移除端口映射
# echo "RStudio Server is running at http://localhost:${PORT}" # 移除 RStudio Server 访问提示
echo "Username: ${USER_NAME}" # 保留用户信息提示，虽然 RStudio Server 不可用，但用户信息仍然传递到容器内
echo "Password: ${PASSWORD}"
echo "Container rstudio_modio will be removed automatically after exit."
echo "You can use 'docker exec -it rstudio_mobio bash' to enter the container." # 添加 docker exec 使用提示 