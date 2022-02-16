## pmartR-shiny

# Install latest version of rocker image
FROM code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone/base:1.0

# Add missing libraries 
RUN apt-get update && apt-get install -y \
	libcurl3-gnutls \
	vim python3-venv

COPY . .

# Install python dependencies
USER root 
WORKDIR /srv/shiny-server/
RUN python3 -m venv /venv
RUN /venv/bin/pip install --upgrade pip
RUN /venv/bin/pip install -r requirements.txt

# Add latest versions of mapDataAccess and pmartR, set map_version to 1 
RUN Rscript -e "install.packages('devtools')"
RUN Rscript -e "devtools::install_github('pmartR/pmartR')"
RUN Rscript -e "devtools::install_local('/srv/shiny-server/mapDataAccess', force = TRUE)"
RUN Rscript -e "Sys.setenv('MAP_VERSION' = '1')"

# Make this shiny app available at port 2800
EXPOSE 2800

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 2800, launch.browser = FALSE)"]
