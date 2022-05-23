FROM rocker/verse:4.1.3

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    byobu \
    libgdal-dev \
    libproj15 \
    libudunits2-0 \
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

USER root

RUN chown -R rstudio:rstudio /home/rstudio
