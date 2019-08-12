FROM rocker/tidyverse:3.6.1
MAINTAINER edvald@wuxinextcode.com

COPY --chown=rstudio:rstudio . /home/rstudio/gorr/
WORKDIR /home/rstudio/gorr

RUN make install
