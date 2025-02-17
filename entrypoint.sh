#!/bin/bash

# 使用明确的变量名，不使用 ${USERNAME}
if ! getent group staff > /dev/null 2>&1; then
    groupadd -g 20 staff
fi

useradd -m -u 501 -g 20 -s /bin/bash colinliu --non-unique

echo "colinliu:${PASSWORD}" | chpasswd
echo "root:${ROOT_PASSWORD}" | chpasswd

echo "colinliu ALL=(ALL) ALL" > /etc/sudoers.d/colinliu
chmod 0440 /etc/sudoers.d/colinliu

chown -R colinliu:staff /home/colinliu

exec "$@" 
