#' Access options available for querying tribal spatial data with `TADA_DataRetrieval()`.
#' 
#' @description
#' This function provides access to [six layer datasets](https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer)
#' containing spatial data related to tribal lands: "Alaska Native Allotments",
#' "Alaska Native Villages", "American Indian Reservations", "Off-reservation Trust Lands",
#' "Oklahoma Tribal Statistical Areas", and "Virginia Federally Recognized Tribes".
#' These datasets are used by `TADA_DataRetrieval()` when retrieving spatial data
#' for tribal lands specified by the user. 
#' 
#' The purpose of `TADA_TribalOptions()` is to allow the user to review the available
#' data in those datasets and identify the records they would like to query with
#' `TADA_DataRetrieval()`.
#' 
#' @param tribal_area_type A character string. Must be one of the six tribal
#' spatial layers: "Alaska Native Allotments", "Alaska Native Villages", 
#' "American Indian Reservations", "Off-reservation Trust Lands",
#' "Oklahoma Tribal Statistical Areas", or "Virginia Federally Recognized Tribes".
#' 
#' @param return_sf Logical. Should the function return the dataset as an `sf`
#' object (TRUE) or a data frame (FALSE)? Defaults to FALSE.
#' 
#' @returns A data frame or `sf` object containing the specified layer from the EPA
#' Map Service.
#' 
#' @seealso [TADA_DataRetrieval()]
#' 

TADA_TribalOptions <- function(tribal_area_type, return_sf = FALSE){
  
  # Make a reference table for tribal area type + url matching
  map_service_urls <- tibble::tribble(
    ~tribal_area,                            ~url,
    "Alaska Native Allotments",              "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0",
    "Alaska Native Villages",                "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/1",
    "American Indian Reservations",          "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2",
    "Off-reservation Trust Lands",           "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3",
    "Oklahoma Tribal Statistical Areas",     "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4",
    "Virginia Federally Recognized Tribes",  "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/5"
  )
  
  # Confirm usable string provided
  if( !(tribal_area_type %in% map_service_urls$tribal_area) ){
    stop("tribal_area_type must match one of the six tribal spatial layers.")
  }
  
  # Query Map Service
  tribal_area_sf <- dplyr::filter(map_service_urls,
                                  tribal_area == tribal_area_type)$url %>%
    arcgislayers::arc_open() %>%
    # Return sf
    arcgislayers::arc_select()
  
  # Convert to df if needed, export
  if(return_sf == FALSE){
    return(
      as.data.frame(tribal_area_sf) %>%
        dplyr::select(-geometry)
    )
  } else {
    return(tribal_area_sf)
  }
  
}