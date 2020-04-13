FROM rocker/tidyverse:3.5.2


RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    liblapack-dev \
  && apt-get clean \
  && install2.r --error \
    caTools \
    rprojroot \
    conflicted \
    cowplot \
    snakecase \
    MASS \
    broom \
    modelr \
    knitr \
    revealjs \
    import \
    poweRlaw \
    xts \
    sp \
    sessioninfo

RUN cd /tmp \
  && wget http://cas.uqam.ca/pub/R/src/contrib/CASdatasets_1.0-9.tar.gz \
  && R CMD INSTALL CASdatasets_1.0-9.tar.gz \
  && rm /tmp/*.tar.gz
