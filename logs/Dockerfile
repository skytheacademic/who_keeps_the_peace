#!/usr/bin/env bash

# Docker Path:
# docker run --platform linux/x86_64 --rm -it -v "$(pwd)/data:/data" -v "$(pwd)/results:/results" -v "$(pwd)/scripts:/scripts" -v "$(pwd)/logs:/logs" pset2

# Set the base environment
FROM ubuntu:18.04
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y software-properties-common=0.96.24.32.14 \
    && sed -i "s/# deb-src/deb-src/g" /etc/apt/sources.list \
    && apt-get install -y libpcre2-dev=10.31-2 \
    && apt-get update \
    && apt-get build-dep r-base-dev -y \
    && apt-get install -y wget=1.19.4-1ubuntu2.2 \
    && wget -c https://cran.r-project.org/src/base/R-4/R-4.1.0.tar.gz \
    && tar -xf R-4.1.0.tar.gz \
    && cd R-4.1.0 \
    && ./configure \
    && make -j9 \
    && make install \
    && cd

# 	&& apt-get install -y r-base-core=3.4.4-1ubuntu1 \

# R Dependencies install #
RUN apt-get update \
    && apt install -y libssl-dev libcurl4-openssl-dev libxml2-dev \
    && apt-get install -y libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.4 \
    && apt-get install -y libssl-dev=1.1.1-1ubuntu2.1~18.04.13 \
    && apt-get install -y libproj-dev=4.9.3-2 \
    && apt-get install -y libgdal-dev=2.2.3+dfsg-2 \
    && apt-get install -y libgeos-dev=3.6.2-1build2 \
    && apt-get install -y libudunits2-dev=2.2.26-1 \
    && apt-get install -y libsndfile1-dev=1.0.28-4ubuntu0.18.04.2 \
    && apt-get install -y ffmpeg=7:3.4.8-0ubuntu0.2


# Get R packages
RUN wget http://cran.r-project.org/src/contrib/Archive/remotes/remotes_2.3.0.tar.gz \
    && R CMD INSTALL remotes_2.3.0.tar.gz \
    && Rscript -e 'remotes::install_version("exactextractr", "0.7.0", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("gdalUtils", "2.0.3.2", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("mapproj", "1.2.7", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("raster", "3.4-13", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("rgeos", "0.5-5", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("rworldmap", "1.3-6", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("sf", "1.0-2", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("sp", "1.4-5", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("tidyverse", "1.3.1", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("viridis", "0.6.1", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("viridisLite", "0.4.0", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("lubridate", "1.7.10", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("spatialEco", "1.3-7", repos = "http://cran.us.r-project.org")' \
    && Rscript -e 'remotes::install_version("gdata", "2.18.0", repos = "http://cran.us.r-project.org")'
