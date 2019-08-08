FROM rocker/tidyverse:3.6.0

ENV RENV_VERSION 0.6.0-61

RUN set -x && \ 
  apt-get update

RUN set -x && \ 
  installGithub.r \ 
    'rstudio/renv@{RENV_VERSION}' && \ 
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
