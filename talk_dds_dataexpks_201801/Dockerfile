FROM rocker/verse:3.5.1


RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    byobu \
  && apt-get clean \
  && install2.r --error \
    revealjs \
    knitr \
    cowplot \
    GGally \
    Rtsne
