# INITIALIZE IF YOU DONT HAVE A PACKRAT DIR #
localDir <- c('~/Documents/forked_repos/')

packrat::set_opts(external.packages=c("stringi", "devtools", "roxygen2", "rgdal"),
                  ignored.packages='pmartR', # install pmartR locally, wont unbundle for some reason
                  load.external.packages.on.startup=FALSE,
                  local.repos= localDir)

packrat::init()

###############################

# INSTALL LOCAL PACKAGES
devtools::install_local('~/Documents/forked_repos/pmartR')
devtools::install_local('~/Documents/forked_repos/mime/')

###############################

# RECORD CHANGES
packrat::snapshot()

# CREATE BUNDLE
packrat::bundle(include.bundles = FALSE)
