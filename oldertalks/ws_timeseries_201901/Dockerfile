FROM rocker/verse:3.5.2


RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libxml2-dev \
    zlib1g-dev \
    libicu-dev \
    libgit2-dev \
    libssh2-1-dev \
    libsm-dev \
    libxt-dev \
    liblapack-dev \
  && apt-get clean \
  && install2.r --error \
    tidyverse \
    caTools \
    rprojroot \
    sessioninfo \
    conflicted \
    cowplot \
    cranlogs \
    timetk \
    tidyquant \
    sweep \
    seasonal \
    forecast \
    broom

