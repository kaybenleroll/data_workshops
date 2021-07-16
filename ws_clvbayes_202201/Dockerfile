FROM rocker/verse:4.0.5

RUN apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    byobu \
    clang \
    clang-9 \
    ditaa \
    htop \
    less \
    libclang-dev \
    libglpk-dev \
    libgsl-dev \
    libnlopt-dev \
    p7zip-full \
    pbzip2 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && mkdir -p $HOME/.R \
  && echo "" > $HOME/.R/Makevars \
  && echo "CXX=clang++-9"                                          >> $HOME/.R/Makevars \
  && echo "CXXFLAGS=-Os -mtune=native -march=native"               >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unused-variable -Wno-unused-function"   >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined"   >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && echo "CXX14=clang++-9"                                        >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS=-Os -mtune=native -march=native"             >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && cp -r $HOME/.R /home/rstudio \
  && chown rstudio:rstudio /home/rstudio/.R \
  && install2.r --error \
    bayesplot \
    brms \
    broom \
    broom.mixed \
    broomExtra \
    BTYD \
    BTYDplus \
    conflicted \
    cowplot \
    CLVTools \
    DataExplorer \
    directlabels \
    drake \
    evir \
    fs \
    furrr \
    ggrepel \
    loo \
    modeltime \
    projpred \
    prophet \
    pryr \
    rmdformats \
    rstan \
    rstanarm \
    sessioninfo \
    shinybrms \
    shinystan \
    snakecase \
    tictoc \
    tidybayes \
    tidygraph \
    tidymodels \
    tidyquant \
    tidytext \
    timetk


WORKDIR /tmp

RUN git clone https://github.com/lindenb/makefile2graph.git \
  && cd makefile2graph \
  && make \
  && make install

WORKDIR /home/rstudio


COPY build/docker_install_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_rpkgs.R


COPY build/conffiles.7z /tmp

RUN 7z x /tmp/conffiles.7z \
  && cp conffiles/.bash*     . \
  && cp conffiles/.gitconfig . \
  && cp conffiles/.Renviron  . \
  && cp conffiles/.Rprofile  . \
  && cp conffiles/user-settings .rstudio/monitored/user-settings/ \
  && chown -R rstudio:rstudio /home/rstudio \
  && rm -rfv conffiles/


