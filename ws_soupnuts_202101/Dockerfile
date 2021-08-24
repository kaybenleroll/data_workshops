FROM kaybenleroll/r_baseimage:base202108

RUN apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    graphviz \
    libgsl-dev \
    liblapack-dev \
    libtk8.6 \
    tk8.6 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    actuar \
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
    descriptr \
    evir \
    factoextra \
    FactoInvestigate \
    FactoMineR \
    Factoshiny \
    fitdistrplus \
    ggraph \
    kableExtra \
    rfm \
    shinyBS \
    shinycssloaders \
    shinythemes \
    survival \
    survminer \
    visNetwork \
    xplorerr


RUN Rscript -e 'BiocManager::install("Rgraphviz")'
