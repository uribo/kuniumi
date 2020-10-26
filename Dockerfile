FROM rocker/geospatial:4.0.3@sha256:6afe626245b00ee8803045e40db6150ace155e2e871ae56ba35c4e06c4861417

RUN set -x && \
  apt-get update && \
  apt-get clean

ARG GITHUB_PAT

RUN set -x && \
  echo "GITHUB_PAT=$GITHUB_PAT" >> /usr/local/lib/R/etc/Renviron

RUN set -x && \
  mkdir -p /home/rstudio/.local/share/renv/cache && \
  chown -R rstudio:rstudio /home/rstudio

RUN set -x && \
  install2.r --error --ncpus -1 --repos 'https://mran.revolutionanalytics.com/snapshot/2020-10-25' \
    renv && \
  rm -rf /tmp/downloaded_packages/ /tmp/*.rds
