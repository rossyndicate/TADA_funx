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

packages <- c("TADA", "tidyverse", "sf", "TADA", "urltools", "geojsonsf", "leaflet", "viridis")

lapply(packages, package_loader)

rm(package_loader)
