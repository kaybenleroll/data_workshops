FROM rocker/tidyverse:4.5.1

#COPY build/Rprofile.site /usr/local/lib/R/etc/
#COPY build/Renviron.site /usr/local/lib/R/etc/

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
    asaur \
    bayesplot \
    benchmarkme \
    brms \
    broom \
    conflicted \
    cowplot \
    DataExplorer \
    directlabels \
    fitdistrplus \
    fs \
    furrr \
    ggsurvfit \
    gt \
    gtsummary \
    kableExtra \
    Hmisc \
    lifecontingencies \
    loo \
    markdown \
    modeltime \
    muhaz \
    nanoparquet \
    posterior \
    projpred \
    prophet \
    pryr \
    qs \
    quarto \
    regplot \
    rstan \
    sessioninfo \
    shinybrms \
    shinystan \
    snakecase \
    splines2 \
    survival \
    survivalmodels \
    survminer \
    survMisc \
    survstan \
    tictoc \
    tidybayes \
    tidygraph \
    tidymodels \
    tidyquant \
    timetk


WORKDIR /tmp

COPY build/docker_install_sys_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_sys_rpkgs.R

RUN git clone https://github.com/lindenb/makefile2graph.git \
  && cd makefile2graph \
  && make \
  && make install

RUN cp -r $HOME/.R /home/rstudio \
  && chown -R rstudio:rstudio /home/rstudio/.R

# Enable GitHub Copilot and configure session timeouts
RUN mkdir -p /etc/rstudio && \
  echo "copilot-enabled=1" >> /etc/rstudio/rsession.conf && \
  echo "session-timeout-minutes=120" >> /etc/rstudio/rsession.conf && \
  echo "session-timeout-suspend=0" >> /etc/rstudio/rsession.conf && \
  echo "auth-timeout-minutes=0" >> /etc/rstudio/rserver.conf

WORKDIR /home/rstudio
USER rstudio

COPY build/conffiles.7z /tmp
RUN 7z x /tmp/conffiles.7z       \
  && cp conffiles/.bash*     .   \
  && cp conffiles/.gitconfig .   \
  && cp conffiles/.Renviron  .   \
  && cp conffiles/.Rprofile  .   \
  && mkdir -p .config/rstudio    \
  && cp conffiles/rstudio-prefs.json .config/rstudio/  \
  && rm -rfv conffiles/ \
  && touch /home/rstudio/.bash_eternal_history

COPY build/docker_install_user_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_user_rpkgs.R


USER root

RUN chown -R rstudio:rstudio /home/rstudio \
  && chmod ugo+rx /home/rstudio \
  && chown -R rstudio:rstudio /home/rstudio/.cmdstan


ARG BUILD_DATE

LABEL org.opencontainers.image.source="https://github.com/kaybenleroll/bayesian_survival_analysis" \
      org.opencontainers.image.authors="Mick Cooney <mickcooney@gmail.com>" \
      org.label-schema.build-date=$BUILD_DATE


