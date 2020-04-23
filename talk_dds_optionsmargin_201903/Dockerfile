FROM rocker/verse:3.5.2

ARG QUANTLIB_VERSION=1.15

RUN apt-get update \
  && apt full-upgrade \
  && apt-get install -y --no-install-recommends \
    byobu \
    libboost-test-dev \
    ccache \
  && apt-get clean

RUN wget https://dl.bintray.com/quantlib/releases/QuantLib-${QUANTLIB_VERSION}.tar.gz \
  && tar xfz QuantLib-${QUANTLIB_VERSION}.tar.gz \
  && rm QuantLib-${QUANTLIB_VERSION}.tar.gz \
  && cd QuantLib-${QUANTLIB_VERSION} \
  && ./configure --prefix=/usr --disable-static CXXFLAGS=-O3 \
  && make -j 4 && make check && make install \
  && cd .. && rm -rf QuantLib-${QUANTLIB_VERSION} && ldconfig
  
RUN install2.r --error \
    revealjs \
    knitr \
    kableExtra \
    RQuantLib \
    cowplot

