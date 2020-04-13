FROM rocker/verse:3.5.1


RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    clang \
    libclang-dev \
  && apt-get clean \
  && install2.r --error \
    tidyverse \
    data.table \
    dtplyr \
    GGally \
    feather \
    Boruta \
    poweRlaw \
    caTools \
    xts \
    sp \
    rprojroot \
    sessioninfo \
    arm \
    modelr \
    broom \
    cowplot \
    revealjs


RUN cd /tmp \
  && wget http://cas.uqam.ca/pub/R/src/contrib/CASdatasets_1.0-9.tar.gz \
  && R CMD INSTALL CASdatasets_1.0-9.tar.gz


