FROM rocker/verse:4.0.2

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    less \
    liblapack-dev \
    p7zip-full \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    conflicted \
    cowplot \
    revealjs \
    rmdformats \
    rsample \
    sp \
    xts

COPY build/docker_install_rpkgs.R /tmp/

RUN Rscript /tmp/docker_install_rpkgs.R

WORKDIR /home/rstudio

