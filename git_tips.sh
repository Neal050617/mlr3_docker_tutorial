# 方案一：安装SSH客户端并配置秘钥（在容器内）
# 安装SSH客户端
apt-get update && apt-get install -y openssh-client

# 生成新SSH密钥
ssh-keygen -t ed25519 -C "your_email@example.com"

# 查看公钥并添加到GitHub
cat ~/.ssh/id_ed25519.pub
# 将显示的内容复制到GitHub：Settings > SSH and GPG keys > New SSH key

# 方案二：使用HTTPS方式（在容器内）
git remote set-url origin https://github.com/Neal050617/mlr3_docker_tutorial.git
git remote -v
#origin  https://github.com/Neal050617/mlr3_docker_tutorial.git (fetch)
#origin  https://github.com/Neal050617/mlr3_docker_tutorial.git (push)

# 方案三：挂载主机SSH配置（最佳长期解决方案）
修改devcontainer.json
{
  "mounts": [
    "source=${localEnv:HOME}/.ssh,target=/home/chaoliu/.ssh,type=bind"
  ],
  "postCreateCommand": "chmod 700 ~/.ssh && chmod 600 ~/.ssh/*"
}
然后重建容器（F1 > Dev Containers: Rebuild Container）。

# 方案四：临时使用github cli
gh auth login
gh auth refresh -h github.com -s admin:org,admin:public_key,admin:enterprise_user,admin:enterprise,gist,notifications,user,workflow
gh auth status
# https://github.com/settings/tokens
# The minimum required scopes are 'repo', 'read:org', 'admin:public_key'.