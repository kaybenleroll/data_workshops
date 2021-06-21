FROM kaybenleroll/r_baseimage:base202104

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
    arules \
    arulesCBA \
    arulesNBMiner \
    arulesSequences \
    arulesViz \
    BiocManager \
    BTYD \
    BTYDplus \
    CLVTools \
    evir \
    fitdistrplus \
    ggraph \
    rfm \
    shinythemes \
    survival \
    survminer \
    visNetwork


RUN Rscript -e 'BiocManager::install("Rgraphviz")'
