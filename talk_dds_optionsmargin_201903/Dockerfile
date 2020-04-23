FROM rocker/verse:3.5.2


RUN echo "deb http://deb.debian.org/debian unstable main" >> /etc/apt/sources.list \
  && apt-get update -qq \
  && apt-get -y upgrade \
  && apt-get install -y --no-install-recommends \
    byobu \
    ccache \
    libquantlib0-dev/unstable \
  && apt-get clean \
  && apt-get -y autoremove \
  && install2.r --error \
    conflicted \
    revealjs \
    knitr \
    kableExtra \
    RQuantLib \
    cowplot

