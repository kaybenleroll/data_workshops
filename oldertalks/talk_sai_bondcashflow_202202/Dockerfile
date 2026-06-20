FROM rocker/verse:4.1.1

RUN apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    byobu \
    ditaa \
    graphviz \
    htop \
    libgsl-dev \
    less \
    p7zip-full \
    pbzip2 \
    tk8.6 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    anytime \
    conflicted \
    cowplot \
    DataExplorer \
    DT \
    fs \
    furrr \
    kableExtra \
    pryr \
    revealjs \
    rmdformats \
    sessioninfo \
    snakecase \
    tictoc \
    tidyquant

COPY build/conffiles.7z               /tmp
COPY build/docker_install_sys_rpkgs.R /tmp

WORKDIR /tmp

RUN git clone https://github.com/lindenb/makefile2graph.git \
  && cd makefile2graph \
  && make \
  && make install

RUN Rscript /tmp/docker_install_sys_rpkgs.R


WORKDIR /home/rstudio

RUN 7z x /tmp/conffiles.7z     \
  && cp conffiles/.bash*     . \
  && cp conffiles/.gitconfig . \
  && cp conffiles/.Renviron  . \
  && cp conffiles/.Rprofile  . \
  && mkdir -p .config/rstudio  \
  && cp conffiles/rstudio-prefs.json .config/rstudio/ \
  && chown -R rstudio:rstudio /home/rstudio           \
  && rm -rfv conffiles/
