FROM rocker/rstudio:3.6.0


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
    furrr \
    rstan \
    tidybayes \
    bayesplot \
    rstanarm \
    tidybayes \
    snakecase \
    fs \
    revealjs \
    knitr
