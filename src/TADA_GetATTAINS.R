#' Link catchment-based ATTAINS assessment unit data to water quality observations imported via `TADA_DataRetrieval()`.
#' 
#' @param data A dataframe created by `TADA_DataRetrieval()`. Requires columns "TADA.LongitudeMeasure" and "TADA.LatitudeMeasure".
#' 
#' @return The original `TADA_DataRetrieval()` dataframe with additional columns associated with the ATTAINS assessment unit data. 
#' 
#' @seealso [TADA_DataRetrieval()]

TADA_GetATTAINS <- function(data){
  
  suppressWarnings({
    
    sf::sf_use_s2(FALSE)
    
    TADA_DataRetrieval_data <- data %>%
      # Use the USGS projection code (also used in AquaSat) to improve CRS in future iteration of the function!
      sf::st_as_sf(., coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"), crs = 4326) %>%
      # add index for identifying obs with more than one ATTAINS assessment unit
      tibble::rowid_to_column(var = "index")
    
    # base URL for  ATTAINS catchment API
    baseurl <- "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?"
    
    # bounding box (with some wiggle) of TADA data
    bbox <- TADA_DataRetrieval_data %>% 
      sf::st_buffer(0.001) %>% 
      sf::st_bbox(TADA_DataRetrieval_data)
    
    # convert bounding box to characters
    bbox <- toString(bbox) %>% 
      # encode for use within the API URL
      urltools::url_encode(.)
    
    # EPSG for CRS used by TADA data. THIS NEEDS TO BE IMPROVED!
    epsg <- sf::st_crs(TADA_DataRetrieval_data)$epsg
    
    # set all necessary parameters for the query
    query <- urltools::param_set(baseurl, key = "geometry", value = bbox) %>%
      urltools::param_set(key = "inSR", value = epsg) %>%
      urltools::param_set(key = "resultRecordCount", value = 5000) %>%
      urltools::param_set(key = "spatialRel", value = "esriSpatialRelIntersects") %>%
      urltools::param_set(key = "f", value = "geojson") %>%
      urltools::param_set(key = "outFields", value = "*") %>%
      urltools::param_set(key = "geometryType", value = "esriGeometryEnvelope") %>%
      urltools::param_set(key = "returnGeometry", value = "true") %>%
      urltools::param_set(key = "returnTrueCurves", value = "false") %>%
      urltools::param_set(key = "returnIdsOnly", value = "false") %>%
      urltools::param_set(key = "returnCountOnly", value = "false") %>%
      urltools::param_set(key = "returnZ", value = "false") %>%
      urltools::param_set(key = "returnM", value = "false") %>%
      urltools::param_set(key = "returnDistinctValues", value = "false") %>%
      urltools::param_set(key = "returnExtentOnly", value = "false") %>%
      urltools::param_set(key = "featureEncoding", value = "esriDefault")
    
    # grab the geospatial data!
    nearby_catchments <- geojsonsf::geojson_sf(query) %>%
      # remove unnecessary columns:
      dplyr::select(-c(OBJECTID, GLOBALID)) %>%
      .[TADA_DataRetrieval_data, ] %>%
      dplyr::rename_with(~ paste0("ATTAINS.", .), dplyr::everything()) %>%
      dplyr::distinct(.keep_all = TRUE)
    
    # join TADA sf features to the ATTAINS catchment feature(s) they land on:
    TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
      sf::st_join(., nearby_catchments) %>%
      sf::st_drop_geometry()
    
    return(TADA_with_ATTAINS)
    
  })
  
}
