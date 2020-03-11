FROM rocker/geospatial:3.6.2

RUN set -x && \
  apt-get update

ARG GITHUB_PAT

RUN set -x && \
  echo "GITHUB_PAT=$GITHUB_PAT" >> /usr/local/lib/R/etc/Renviron

RUN set -x && \
  R -e 'install.packages("renv", repos = c(CRAN = "https://cran.rstudio.com"))' && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
