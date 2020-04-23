FROM rocker/verse:3.5.1


RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    byobu \
  && apt-get clean \
  && install2.r --error \
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

