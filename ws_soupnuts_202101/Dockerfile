FROM kaybenleroll/r_baseimage:base202012

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    liblapack-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && install2.r --error \
    actuar \
    BTYD \
    evir \
    fitdistrplus \
    rfm \
    survival \
    survminer

