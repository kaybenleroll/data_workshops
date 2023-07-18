FROM rocker/tidyverse:4.3.0

ENV TZ=Europe/Dublin

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
  && echo $TZ > /etc/timezone \
  && apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    byobu \
    graphviz \
    htop \
    less \
    libgdal-dev \
    libproj-dev \
    libudunits2-dev \
    libxml2-dev \
    p7zip-full \
    pbzip2 \
    zlib1g-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    caTools \
    cowplot \
    DataExplorer \
    DT \
    evir \
    knitr \
    parallelly \
    poweRlaw \
    rmdformats \
    rprojroot \
    sf \
    snakecase \
    sp \
    xts


WORKDIR /tmp

COPY build/docker_install_sys_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_sys_rpkgs.R


WORKDIR /home/rstudio
USER rstudio

COPY build/conffiles.7z /tmp
RUN 7z x /tmp/conffiles.7z       \
  && cp conffiles/.bash*     .   \
  && cp conffiles/.gitconfig .   \
  && cp conffiles/.Renviron  .   \
  && cp conffiles/.Rprofile  .   \
  && mkdir -p .config/rstudio    \
  && cp conffiles/rstudio-prefs.json .config/rstudio/  \
  && rm -rfv conffiles/ \
  && touch /home/rstudio/.bash_eternal_history

COPY build/docker_install_user_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_user_rpkgs.R


USER root

RUN chown -R rstudio:rstudio /home/rstudio \
  && chmod ugo+rx /home/rstudio
