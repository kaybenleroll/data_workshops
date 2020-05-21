FROM rocker/verse:3.5.3


RUN echo "deb http://deb.debian.org/debian unstable main" >> /etc/apt/sources.list \
  && apt-get update -qq \
  && apt-get -y upgrade \
  && apt-get install -y --no-install-recommends \
    byobu \
  && apt-get clean \
  && apt-get -y autoremove \
  && install2.r --error \
    import \
    cowplot \
    knitr \
    feather
