FROM rocker/tidyverse:4.3.0

ENV TZ=Europe/Dublin

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
  && echo $TZ > /etc/timezone \
  && apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    byobu \
    graphviz \
    less \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev \
    libxml2-dev \
    zlib1g-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    caTools \
    DT \
    evir \
    knitr \
    poweRlaw \
    rmdformats \
    rprojroot \
    sf \
    sp \
    sweep \
    xts


WORKDIR /tmp

COPY build/docker_install_sys_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_sys_rpkgs.R


WORKDIR /home/rstudio
USER rstudio

COPY build/docker_install_user_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_user_rpkgs.R
