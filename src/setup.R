# get current TADA from Github:

# if(!"remotes" %in% installed.packages()){
#   install.packages("remotes")
# }
# 
# remotes::install_github("USEPA/TADA", ref = "develop", dependencies = TRUE, force = TRUE)


# Function to check for package installation, then install (if necessary) and load libraries.
# Adapted from code developed by Caitlin Mothes, PhD.
package_loader <- function(x) {
  if (x %in% installed.packages()) {
    library(x, character.only = TRUE)
  } else {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}

packages <- c("tidyverse", "sf", "urltools", "geojsonsf", "leaflet", "viridis", "remotes")

lapply(packages, package_loader)
# install TADA from GitHub using {remotes}
remotes::install_github("USEPA/TADA", ref = "develop", dependencies = TRUE, force = TRUE)
rm(package_loader)
