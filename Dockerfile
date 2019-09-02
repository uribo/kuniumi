FROM rocker/geospatial:3.6.0

RUN set -x && \
  apt-get update

ARG GITHUB_PAT

RUN set -x && \
  echo "GITHUB_PAT=$GITHUB_PAT" >> /usr/local/lib/R/etc/Renviron

RUN set -x && \
  R -e 'install.packages("remotes", repos = c(CRAN = "https://cran.rstudio.com"))' && \
  R -e 'remotes::install_github("rstudio/renv@0.7.0-17")' && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
