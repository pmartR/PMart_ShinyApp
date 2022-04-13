## pmartR-shiny

# Change this when we bump versions, or if you have some test version of the
# base container you can specify --build-arg base_tag=<yourtag> in docker run.
ARG base_tag=1.1

# Install latest version of rocker image
FROM code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone/base:$base_tag

COPY . .

# Make this shiny app available at port 2800
EXPOSE 2800

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 2800, launch.browser = FALSE)"]
