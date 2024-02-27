#' Transform a Water Quality Portal dataframe into a geospatial object.
#' 
#' @param data A dataframe created by `TADA_DataRetrieval()`.
#' 
#' @param crs The coordinate reference system (CRS) you would like the returned point features to be in. The default is CRS 4326 (WGS84).
#' 
#' @return The original Water Quality Portal dataframe but as geospatial {sf} point objects. 
#' 
#' @seealso [TADA_DataRetrieval()]

TADA_MakeSpatial <- function(data, crs = 4326){
  
  if (!"LongitudeMeasure" %in% colnames(data) & !"LatitudeMeasure" %in% colnames(data)) {
    stop("The dataframe does not contain WQP-style latitude and longitude data (column names `LatitudeMeasure`, LongitudeMeasure`, and `HorizontalCoordinateReferenceSystemDatumName.")
  } else if (!is.null(data) & inherits(data, "sf")) { stop("Your data is already a spatial object.")
  }
  
  # Make a reference table for CRS and EPSG codes
  epsg_codes <- tribble(
    ~ HorizontalCoordinateReferenceSystemDatumName, ~ epsg,
    "NAD83", 4269,
    "WGS84", 4326,
    "NAD27", 4267,
    # For now assume these are WGS84. USGS has done this, too. 
    "UNKWN", 4326,
    "OTHER", 4326,
    # There's some others I've encountered too, e.g.
    "OLDHI", 4135
  )
  
  suppressMessages(suppressWarnings({
    sf <- data %>%
      mutate(lat = as.numeric(LatitudeMeasure),
             lon = as.numeric(LongitudeMeasure)) %>%
      # Add EPSG codes
      dplyr::left_join(x = .,
                       y = epsg_codes,
                       by = "HorizontalCoordinateReferenceSystemDatumName") %>%
      # Group by CRS:
      split(f = .$HorizontalCoordinateReferenceSystemDatumName) %>%
      # Transform and re-stack:
      purrr::map_df(.x = .,
                    .f = ~ .x %>%
                      sf::st_as_sf(coords = c("lon", "lat"),
                                   crs = unique(.x$epsg)) %>%
                      # transform to the selected CRS:
                      sf::st_transform(sf::st_crs(as.numeric(crs)))) %>%
      dplyr::select(-epsg)
  }))
  
  return(sf)
  
}

