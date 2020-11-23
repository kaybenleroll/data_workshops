FROM kaybenleroll/r_baseimage:base202011

RUN install2.r --error BiocManager

COPY build/docker_install_bioc_pkgs.R /tmp/

RUN Rscript /tmp/docker_install_bioc_pkgs.R \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
    htop \
    liblapack-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    bnlearn \
    ggm \
    gRain \
    gRbase \
    gRc \
    gRim \
    GGMselect \
    rbmn \
    tidygraph

