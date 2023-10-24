FROM rocker/verse:3.5.0


RUN apt-get update \
  && apt-get -y upgrade \
  && apt-get install -y --no-install-recommends \
    byobu \
  && apt-get clean \
  && mkdir -p $HOME/.R \
  && echo "CC=clang" > $HOME/.R/Makevars \
  && echo "\n" >> $HOME/.R/Makevars \
  && echo "CXX=clang++" > $HOME/.R/Makevars \
  && echo "CXXFLAGS=-Os -mtune=native -march=native" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo "\n" >> $HOME/.R/Makevars \
  && echo "CXX11=clang++" > $HOME/.R/Makevars \
  && echo "CXX11FLAGS=-Os -mtune=native -march=native" >> $HOME/.R/Makevars \
  && echo "CXX11FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX11FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo "\n" >> $HOME/.R/Makevars \
  && echo "CXX14=clang++" > $HOME/.R/Makevars \
  && echo "CXX14FLAGS=-Os -mtune=native -march=native" >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && install2.r --error \
    revealjs \
    knitr \
    cowplot \
    rstan \
    bayesplot \
    ggthemes \
    deSolve

