FROM mobior:v0.0.5

ARG DEBIAN_FRONTEND=noninteractive
ARG UBUNTU_MIRROR=http://mirrors.aliyun.com/ubuntu/
ARG CRAN_APT=https://cloud.r-project.org/bin/linux/ubuntu
ARG CRAN_REPO=https://mirrors.tuna.tsinghua.edu.cn/CRAN/
ARG BIOC_MIRROR=https://mirrors.tuna.tsinghua.edu.cn/bioconductor
ARG RSTUDIO_DEB_URL=https://rstudio.org/download/latest/stable/server/jammy/rstudio-server-latest-amd64.deb
ARG MOBIOR_PROFILE=core

ENV TZ=Asia/Shanghai
ENV LANG=zh_CN.UTF-8
ENV LC_ALL=zh_CN.UTF-8
ENV R_LIBS_USER=/usr/local/lib/R/site-library
ENV R_LIBS=/usr/local/lib/R/site-library
ENV R_SITE_LIBRARY=/usr/local/lib/R/site-library
ENV CRAN_REPO=${CRAN_REPO}
ENV BIOC_MIRROR=${BIOC_MIRROR}
ENV MOBIOR_PROFILE=${MOBIOR_PROFILE}
ENV MOBIOR_PACKAGE_MANIFEST=/opt/mobior/package_manifest.tsv

RUN printf '%s\n' \
    "deb ${UBUNTU_MIRROR} noble main restricted universe multiverse" \
    "deb ${UBUNTU_MIRROR} noble-updates main restricted universe multiverse" \
    "deb ${UBUNTU_MIRROR} noble-backports main restricted universe multiverse" \
    "deb ${UBUNTU_MIRROR} noble-security main restricted universe multiverse" \
    > /etc/apt/sources.list

RUN apt-get update && apt-get install -y --no-install-recommends \
    apt-transport-https \
    build-essential \
    ca-certificates \
    cmake \
    cargo \
    curl \
    default-jdk \
    default-jre \
    dirmngr \
    fontconfig \
    fonts-noto-cjk \
    g++ \
    gcc \
    gfortran \
    gdebi-core \
    git \
    gnupg \
    jq \
    less \
    libbz2-dev \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libgit2-dev \
    libglpk-dev \
    libgsl-dev \
    libharfbuzz-dev \
    libhdf5-dev \
    libicu-dev \
    libjpeg-dev \
    liblzma-dev \
    libmagick++-dev \
    libmbedtls-dev \
    libmpfr-dev \
    libnetcdf-dev \
    libpcre2-dev \
    libpng-dev \
    libproj-dev \
    libreadline-dev \
    libsecret-1-dev \
    libsqlite3-dev \
    libssh2-1-dev \
    libssl-dev \
    libtiff-dev \
    libudunits2-dev \
    libuv1-dev \
    libx11-dev \
    libxml2-dev \
    libxt-dev \
    locales \
    graphviz \
    openssh-client \
    pkg-config \
    rustc \
    sudo \
    tmux \
    vim \
    wget \
    zlib1g-dev \
    && locale-gen zh_CN.UTF-8 en_US.UTF-8 \
    && update-locale LANG=zh_CN.UTF-8 LC_ALL=zh_CN.UTF-8 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN set -eux; \
    rm -f /etc/apt/sources.list.d/r.list /etc/apt/sources.list.d/r-project.list; \
    wget -qO- "${CRAN_APT}/marutter_pubkey.asc" | gpg --dearmor > /usr/share/keyrings/r-project.gpg; \
    echo "deb [signed-by=/usr/share/keyrings/r-project.gpg] ${CRAN_APT} noble-cran40/" > /etc/apt/sources.list.d/r-project.list; \
    apt-get update; \
    apt-get install -y --no-install-recommends r-base r-base-dev r-recommended; \
    R --version; \
    Rscript -e 'if (!grepl("^R version 4[.]6[.]", R.version.string)) stop("R 4.6.x required, got: ", R.version.string)'; \
    R CMD javareconf; \
    rm -rf /usr/local/lib/R/site-library/*; \
    mkdir -p /usr/local/lib/R/site-library /etc/R; \
    chmod 777 /usr/local/lib/R/site-library; \
    echo '.libPaths("/usr/local/lib/R/site-library")' > /etc/R/Rprofile.site; \
    apt-get clean; \
    rm -rf /var/lib/apt/lists/*

COPY package_manifest.tsv /opt/mobior/package_manifest.tsv
COPY install_mobior_v006_packages.R /opt/mobior/install_mobior_v006_packages.R
COPY check_mobior_v006_packages.R /opt/mobior/check_mobior_v006_packages.R
COPY vendor_packages/mlr3cmprsk /opt/mobior/vendor_packages/mlr3cmprsk
COPY vendor_packages/mlr3extralearners /opt/mobior/vendor_packages/mlr3extralearners
COPY vendor_packages/mlr3proba /opt/mobior/vendor_packages/mlr3proba
COPY R_scripts /opt/mobior/R_scripts

RUN chmod +x /opt/mobior/*.R \
    && Rscript /opt/mobior/install_mobior_v006_packages.R \
    && Rscript /opt/mobior/check_mobior_v006_packages.R \
      --manifest=/opt/mobior/package_manifest.tsv \
      --out=/opt/mobior/package_check.tsv \
      --profile=core \
      --strict=true

COPY install_full_available_v006_packages.R /opt/mobior/install_full_available_v006_packages.R
RUN chmod +x /opt/mobior/install_full_available_v006_packages.R

ARG INSTALL_RSTUDIO=false
RUN if [ "${INSTALL_RSTUDIO}" = "true" ]; then \
      apt-get update \
      && apt-get install -y --no-install-recommends gdebi-core \
      && timeout 240 wget -q --timeout=60 --tries=2 "${RSTUDIO_DEB_URL}" -O /tmp/rstudio-server.deb \
      && gdebi -n /tmp/rstudio-server.deb \
      && rm -f /tmp/rstudio-server.deb \
      && apt-get clean \
      && rm -rf /var/lib/apt/lists/*; \
    else \
      echo "Skipping optional RStudio Server reinstall. Set --build-arg INSTALL_RSTUDIO=true to enable."; \
    fi

RUN mkdir -p /workspace \
    && chmod 777 /workspace \
    && (groupadd -g 100 users 2>/dev/null || true) \
    && (groupadd -g 20 staff 2>/dev/null || true)

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

WORKDIR /workspace
EXPOSE 8787

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["bash"]
