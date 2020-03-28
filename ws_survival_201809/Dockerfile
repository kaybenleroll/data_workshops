FROM rocker/rstudio:3.5.1


RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libxml2-dev \
    zlib1g-dev \
  && apt-get clean \
  && install2.r --error \
    tidyverse \
    caTools \
    rprojroot \
    sessioninfo \
    conflicted \
    cowplot \
    survival \
    survminer \
    muhaz \
    broom \
    asaur

