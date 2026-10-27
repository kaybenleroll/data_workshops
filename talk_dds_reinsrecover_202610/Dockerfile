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
    ditaa \
    gh \
    graphviz \
    htop \
    just \
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
  && install2.r --error \
    conflicted \
    cowplot \
    DiagrammeR \
    directlabels \
    fs \
    ggmap \
    ggraph \
    gt \
    igraph \
    quarto \
    sessioninfo \
    tidygraph


WORKDIR /tmp

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

USER root

RUN chown -R rstudio:rstudio /home/rstudio \
  && chmod ugo+rx /home/rstudio


ARG BUILD_DATE

LABEL org.opencontainers.image.source="https://github.com/kaybenleroll/data_workshops" \
      org.opencontainers.image.authors="Mick Cooney <mickcooney@gmail.com>" \
      org.label-schema.build-date=$BUILD_DATE
