FROM rocker/tidyverse:4.5.1

ENV TZ=Europe/Dublin

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
  && echo $TZ > /etc/timezone \
  && apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    byobu \
    bzip2 \
    clang \
    ditaa \
    gh \
    graphviz \
    htop \
    just \
    less \
    libclang-dev \
    libglpk-dev \
    libgsl-dev \
    libnlopt-dev \
    libomp-dev \
    p7zip-full \
    pbzip2 \
    plocate \
    rsyslog \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && mkdir -p $HOME/.R \
  && echo "" > $HOME/.R/Makevars \
  && echo "CC=clang"                                               >> $HOME/.R/Makevars \
  && echo "CXX=clang++"                                            >> $HOME/.R/Makevars \
  && echo "CXXFLAGS=-Os"                                           >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unused-variable -Wno-unused-function"   >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined"   >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && echo "CC11=clang"                                             >> $HOME/.R/Makevars \
  && echo "CXX11=clang++"                                          >> $HOME/.R/Makevars \
  && echo "CXX11FLAGS=-Os"                                         >> $HOME/.R/Makevars \
  && echo "CXX11FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX11FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && echo "CC14=clang"                                             >> $HOME/.R/Makevars \
  && echo "CXX14=clang++"                                          >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS=-Os"                                         >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && echo "CC17=clang++"                                           >> $HOME/.R/Makevars \
  && echo "CXX17=clang++"                                          >> $HOME/.R/Makevars \
  && echo "CXX17FLAGS=-Os"                                         >> $HOME/.R/Makevars \
  && echo "CXX17FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX17FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && install2.r --error \
    anytime \
    arrow \
    argparse \
    broom \
    conflicted \
    copula \
    cowplot \
    DataExplorer \
    directlabels \
    fitdistrplus \
    fs \
    furrr \
    GGally \
    gt \
    gtsummary \
    jsonlite \
    kableExtra \
    lattice \
    lubridate \
    nanoparquet \
    pryr \
    qs2 \
    quarto \
    scatterplot3d \
    sessioninfo \
    snakecase \
    tictoc \
    tidybayes \
    tidymodels \
    VineCopula

RUN mkdir -p /etc/rstudio && \
  echo "copilot-enabled=1" >> /etc/rstudio/rsession.conf && \
  echo "session-timeout-minutes=120" >> /etc/rstudio/rsession.conf && \
  echo "session-timeout-suspend=0" >> /etc/rstudio/rsession.conf && \
  echo "auth-timeout-minutes=0" >> /etc/rstudio/rserver.conf

WORKDIR /home/rstudio
USER rstudio

COPY build/rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json

USER root
RUN chown -R rstudio:rstudio /home/rstudio

USER rstudio

ARG BUILD_DATE

LABEL org.opencontainers.image.source="https://github.com/kaybenleroll/data_workshops/ws_copula_basics_202602" \
      org.opencontainers.image.authors="Mick Cooney <mickcooney@gmail.com>" \
      org.label-schema.build-date=$BUILD_DATE
