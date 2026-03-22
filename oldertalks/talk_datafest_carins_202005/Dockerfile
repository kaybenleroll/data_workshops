FROM rocker/verse:4.0.0

RUN apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    byobu \
    htop \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    broom \
    caTools \
    conflicted \
    cowplot \
    furrr \
    knitr \
    modelr \
    MASS \
    poweRlaw \
    revealjs \
    rprojroot \
    sessioninfo \
    snakecase \
    sp \
    xts

RUN cd /tmp \
  && wget http://cas.uqam.ca/pub/R/src/contrib/CASdatasets_1.0-9.tar.gz \
  && R CMD INSTALL CASdatasets_1.0-9.tar.gz \
  && rm /tmp/*.tar.gz
