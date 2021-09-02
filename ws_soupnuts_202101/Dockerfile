FROM rocker/verse:4.1.0

RUN apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    byobu \
    ditaa \
    graphviz \
    htop \
    libgsl-dev \
    less \
    p7zip-full \
    pbzip2 \
    tk8.6 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    anytime \
    arules \
    arulesCBA \
    arulesNBMiner \
    arulesSequences \
    arulesViz \
    BiocManager \
    BTYD \
    BTYDplus \
    CLVTools \
    conflicted \
    cowplot \
    DataExplorer \
    descriptr \
    directlabels \
    evir \
    factoextra \
    FactoInvestigate \
    FactoMineR \
    Factoshiny \
    fitdistrplus \
    fs \
    furrr \
    ggraph \
    ggwordcloud \
    kableExtra \
    pryr \
    rfm \
    rmdformats \
    sessioninfo \
    shinyBS \
    shinycssloaders \
    shinythemes \
    snakecase \
    survival \
    survminer \
    tictoc \
    tidygraph \
    tidyquant \
    tidytext \
    visNetwork \
    xplorerr

RUN Rscript -e 'BiocManager::install("Rgraphviz")'

COPY build/conffiles.7z           /tmp

WORKDIR /tmp

RUN git clone https://github.com/lindenb/makefile2graph.git \
  && cd makefile2graph \
  && make \
  && make install

WORKDIR /home/rstudio

RUN 7z x /tmp/conffiles.7z     \
  && cp conffiles/.bash*     . \
  && cp conffiles/.gitconfig . \
  && cp conffiles/.Renviron  . \
  && cp conffiles/.Rprofile  . \
  && mkdir -p .config/rstudio  \
  && cp conffiles/rstudio-prefs.json .config/rstudio/ \
  && chown -R rstudio:rstudio /home/rstudio           \
  && rm -rfv conffiles/


