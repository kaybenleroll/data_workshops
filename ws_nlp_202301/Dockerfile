FROM rocker/verse:4.2.1

ENV RETICULATE_MINICONDA_ENABLED=FALSE

RUN apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y --no-install-recommends \
    byobu \
    clang \
    less \
    libclang-dev \
    libgsl-dev \
    libleptonica-dev \
    libpoppler-cpp-dev \
    libqpdf26 \
    libtesseract-dev \
    p7zip-full \
    pbzip2 \
    python3-pip \
    qpdf \
    tesseract-ocr-eng \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && mkdir -p $HOME/.R \
  && echo "" > $HOME/.R/Makevars \
  && echo "CXX=clang++"                                            >> $HOME/.R/Makevars \
  && echo "CXXFLAGS=-Os -mtune=native -march=native"               >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unused-variable -Wno-unused-function"   >> $HOME/.R/Makevars \
  && echo "CXXFLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined"   >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && echo "CXX14=clang++"                                          >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS=-Os -mtune=native -march=native"             >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
  && echo "CXX14FLAGS+= -Wno-unknown-pragmas -Wno-macro-redefined" >> $HOME/.R/Makevars \
  && echo ""                                                       >> $HOME/.R/Makevars \
  && cp -r $HOME/.R /home/rstudio \
  && chown -R rstudio:rstudio /home/rstudio/ \
  && pip3 install \
    cleannlp \
    spacy \
    stanfordnlp \
  && ln -s /usr/bin/python3 /usr/bin/python \
  && install2.r --error \
    anytime \
    broom \
    cleanNLP \
    conflicted \
    corpus \
    cowplot \
    DataExplorer \
    directlabels \
    fs \
    furrr \
    kernlab \
    lda \
    lobstr \
    lsa \
    ngram \
    ngramrr \
    NLP \
    openNLP \
    polite \
    politeness \
    pryr \
    qdap \
    quanteda \
    rvest \
    sessioninfo \
    snakecase \
    spacyr \
    tesseract \
    text2vec \
    textfeatures \
    textplot \
    tidygraph \
    tidymodels \
    tidytext \
    tm \
    topicmodels \
    udpipe \
    wordcloud \
    zipfR


WORKDIR /tmp

COPY build/test_report.qmd /tmp
RUN quarto render test_report.qmd --to pdf \
  && rm -fv /tmp/test_report.pdf


RUN git clone https://github.com/lindenb/makefile2graph.git \
  && cd makefile2graph \
  && make \
  && make install
  
RUN cp -r $HOME/.R /home/rstudio \
  && chown -R rstudio:rstudio /home/rstudio/.R


COPY build/docker_install_sys_rpkgs.R /tmp
RUN Rscript /tmp/docker_install_sys_rpkgs.R


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

COPY build/docker_install_user_config.R /tmp
RUN Rscript /tmp/docker_install_user_config.R


USER root

RUN chown -R rstudio:rstudio /home/rstudio
