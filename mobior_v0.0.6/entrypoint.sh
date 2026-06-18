#!/usr/bin/env bash
set -euo pipefail

if [ "$(id -u)" -ne 0 ]; then
  exec "$@"
fi

if [ "$#" -gt 0 ] && [ "$1" != "rstudio" ]; then
  exec "$@"
fi

RUSER="${USER:-rstudio}"
RPASSWORD="${PASSWORD:-MoBio888}"
RUSER_ID="${USER_ID:-1000}"
RGROUP_ID="${GROUP_ID:-1000}"
RGROUP_NAME="${GROUP_NAME:-users}"

if ! getent group "${RGROUP_NAME}" >/dev/null 2>&1; then
  groupadd -g "${RGROUP_ID}" "${RGROUP_NAME}" 2>/dev/null || RGROUP_NAME="users"
fi

if ! id "${RUSER}" >/dev/null 2>&1; then
  useradd -m -s /bin/bash -u "${RUSER_ID}" -g "${RGROUP_NAME}" "${RUSER}"
fi

echo "${RUSER}:${RPASSWORD}" | chpasswd
usermod -a -G sudo "${RUSER}" 2>/dev/null || true

if [ -x /usr/lib/rstudio-server/bin/rserver ]; then
  exec /usr/lib/rstudio-server/bin/rserver --server-daemonize=0
fi

echo "RStudio Server is not installed in this image; starting bash instead." >&2
exec bash
