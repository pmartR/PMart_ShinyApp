## pmartR-shiny

# Install latest version of rocker image
FROM code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone/base:1.0

COPY . .

# Make this shiny app available at port 8300
EXPOSE 8300

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 8300, launch.browser = FALSE)"]
