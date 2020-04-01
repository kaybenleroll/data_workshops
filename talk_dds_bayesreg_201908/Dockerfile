FROM kaybenleroll/r_baseimage:base202003


RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  && apt-get clean \
  && install2.r --error \
    revealjs \
    knitr
