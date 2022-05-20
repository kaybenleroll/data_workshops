FROM rocker/verse:4.1.3

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    byobu \
    libgdal-dev \
    libproj15 \
    libudunits2-0 \
    p7zip-full \
    pbzip2 \
  && apt-get clean \
  && install2.r --error \
    conflicted \
    cowplot \
    knitr \
    markdown \
    revealjs \
    rmarkdown \
    rsample \
    snakecase \
    sf \
    tmap

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


USER root

RUN chown -R rstudio:rstudio /home/rstudio
