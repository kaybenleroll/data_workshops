FROM kaybenleroll/r_baseimage:base202104

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
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
    evir \
    fitdistrplus \
    ggraph \
    rfm \
    survival \
    survminer

RUN Rscript -e 'BiocManager::install("Rgraphviz")'
