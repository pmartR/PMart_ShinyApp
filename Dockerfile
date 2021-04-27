## pmartR-shiny

## This container is built specifically for the MultiProbe project. 

# Install latest version of rocker image
FROM rocker/shiny:4.0.3

# Load general use libraries
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libssl-dev \
    libcurl4-openssl-dev \
    libcairo2-dev \
    libxt-dev \
    libssh2-1-dev \
    libhiredis-dev \
    libzmq3-dev \
    libxml2-dev
    
WORKDIR /srv/shiny-server/pmart

# only need these three to restore
COPY --chown=shiny:shiny renv ./renv
COPY --chown=shiny:shiny renv.lock .

# everything must be done as shiny, otherwise shiny server will get permission denied
USER shiny

# call restore()
RUN R -e "install.packages('renv', repos = 'https://cran.rstudio.com')"
RUN R -e 'renv::restore()'

# renv attempts to make a directory at root on startup as user shiny.  
# pre-make the directory and give the shiny user ownership.
USER root
RUN mkdir /Library
RUN chown shiny:shiny /Library

COPY --chown=shiny:shiny . .


