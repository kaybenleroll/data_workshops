FROM kaybenleroll/r_baseimage:base202003

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    liblapack-dev \
  && apt-get clean \
  && install2.r --error \
    bayesplot