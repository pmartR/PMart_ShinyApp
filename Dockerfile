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
    
WORKDIR /srv/shiny-server/

# only need this to restore
COPY renv.lock .

# pre-install renv
RUN R -e "install.packages('renv', repos = 'https://cran.rstudio.com')"

# install all packages listed in renv.lock
RUN R -e 'renv::restore()'

COPY . .

# Make this shiny app available at port 8300
EXPOSE 8300

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 8300, launch.browser = FALSE)"]



