FROM rocker/verse:4.0.2

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    less \
    liblapack-dev \
    p7zip-full \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    revealjs \
    rmdformats

WORKDIR /home/rstudio

