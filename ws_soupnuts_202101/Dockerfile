FROM kaybenleroll/r_baseimage:base202102

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    liblapack-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    actuar \
    arules \
    arulesViz \
    BTYD \
    evir \
    fitdistrplus \
    rfm \
    survival \
    survminer

