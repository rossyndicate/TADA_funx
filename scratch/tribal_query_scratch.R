library(tidyverse)
library(arcgis)
library(sf)

# EPATADA group would like a string input option for tribal land queries. Per
# https://github.com/USEPA/EPATADA/issues/345 the preferred method is the EPA's 
# tribal land boundary service, described here:
# https://www.epa.gov/geospatial/guidance-using-tribal-boundaries-areas-and-names-resources
# There are six feature datasets indicated there for "national scale analyses
# and Agency-wide reporting" related to federally recognized tribes. The six
# datasets vary in their column structure, requiring flexible logic.


# Data load/exploration ---------------------------------------------------

# Six tribal features.
# Source: https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer

# Alaska Native Allotments
alaska_native_allotments <- arc_open("https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0") %>%
  arc_select()

# Alaska Native Villages
alaska_native_villages <- arc_open("https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1") %>%
  arc_select()

# American Indian Reservations
american_indian_reservations <- arc_open("https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2") %>%
  arc_select()

# Off-reservation Trust Lands
off_reservation_trust_lands <- arc_open("https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3") %>%
  arc_select()

# Oklahoma Tribal Statistical Areas
ok_tribal_statistical_areas <- arc_open("https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4") %>%
  arc_select()

# Virginia Federally Recognized Tribes
va_federally_recognized_tribes <- arc_open("https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5") %>%
  arc_select()


# Functional internals drafting -------------------------------------------

# Make a reference table for tribal area type + url matching
map_service_urls <- tribble(
  ~tribal_area,                            ~url,
  "Alaska Native Allotments",              "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0",
  "Alaska Native Villages",                "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1",
  "American Indian Reservations",          "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2",
  "Off-reservation Trust Lands",           "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3",
  "Oklahoma Tribal Statistical Areas",     "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4",
  "Virginia Federally Recognized Tribes",  "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5"
)


# Defaults for testing only:
tribal_area_type <- "American Indian Reservations"
tribe_name <- "Fond du Lac"


# These area types allow filtering by TRIBE_NAME (unique within each type)
if(tribal_area_type %in% c("Alaska Native Villages",
                           "American Indian Reservations",
                           "Off-reservation Trust Lands",
                           "Oklahoma Tribal Statistical Areas",
                           "Virginia Federally Recognized Tribes")
){
  # Get the relevant url
  tribal_sf <- filter(map_service_urls,
                      tribal_area == tribal_area_type)$url %>%
    # Pull data
    arc_open() %>%
    # Return sf
    arc_select() %>%
    # Filter for user selection
    filter(
      TRIBE_NAME == tribe_name
    )
  
  # Otherwise filter by PARCEL_NO (Note that values in this col are not unique)
} else if(tribal_area_type == "Alaska Native Allotments"){
  
  tribal_sf <- filter(map_service_urls,
                      tribal_area == tribal_area_type)$url %>%
    arc_open() %>%
    arc_select() %>%
    filter(
      PARCEL_NO == parcel_no
    )
  
} else {
  stop("Tribal area type not recognized. Refer to TADA_TribalOptions() for query options.")
}

tribal_sf %>%
  ggplot() +
  geom_sf() +
  theme_bw()
