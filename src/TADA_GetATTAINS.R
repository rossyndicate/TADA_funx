#' Link catchment-based ATTAINS assessment unit data to Water Quality Portal observations often imported via `TADA_DataRetrieval()`.
#' 
#' @param data A dataframe created by `TADA_DataRetrieval()`.
#' @param return Whether to add the ATTAINS shapefile objects in your Global Environment. TRUE (yes, return) or FALSE (no, do not return).
#' 
#' @return The original `TADA_DataRetrieval()` dataframe with additional columns associated with the ATTAINS assessment unit data. 
#' 
#' @seealso [TADA_DataRetrieval()]

TADA_GetATTAINS <- function(data, return = FALSE){
  
  if(nrow(data) == 0){stop("Your Water Quality Portal dataframe has no observations.")}
  
  suppressMessages(suppressWarnings({
    
    sf::sf_use_s2(FALSE)
    
    # If data is already spatial, just make sure it is in the right CRS
    # and add an index...
    if (!is.null(data) & inherits(data, "sf")) {
      TADA_DataRetrieval_data <- data %>%
        sf::st_transform(4326) %>%
        tibble::rowid_to_column(var = "index")
    } else {
      # ... Otherwise transform into a spatial object then do the same thing:
      TADA_DataRetrieval_data <- data %>%
        #convert dataframe to a spatial object
        TADA_MakeSpatial(data = ., crs = 4326) %>%
        # add index for identifying obs with more than one ATTAINS assessment unit
        tibble::rowid_to_column(var = "index")
    }
    
    # base URL for ATTAINS catchment API
    baseurl <- "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?"
    
    # bounding box (with some wiggle) of user's WQP data
    bbox <- TADA_DataRetrieval_data %>% 
      sf::st_buffer(0.001) %>% 
      sf::st_bbox(TADA_DataRetrieval_data) %>%
      # convert bounding box to characters
      toString(.) %>% 
      # encode for use within the API URL
      urltools::url_encode(.)
    
    # EPSG we want our ATTAINS data to be in:
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
    
    # grab the ATTAINS catchments within our WQP bbox:
    # (Wrapped with "try" because it is possible that no ATTAINS data exists in the bbox.)
    try(nearby_catchments <- geojsonsf::geojson_sf(query) %>%
          # remove unnecessary columns:
          dplyr::select(-c(OBJECTID, GLOBALID)) %>%
          # select only catchments that have WQP observations in them:
          .[TADA_DataRetrieval_data, ] %>%
          # prepend "ATTAINS" to ATTAINS data
          dplyr::rename_with(~ paste0("ATTAINS.", .), dplyr::everything()) %>%
          # get rid of dupes (as a precaution)
          dplyr::distinct(.keep_all = TRUE),
        silent = TRUE)
    # if not ATTAINS data return original data frame:
    if(is.null(nearby_catchments)){
      print("There are no ATTAINS features associated with these WQP observations. Returning original dataframe.")
      return(data)
    } else {
      # ... otherwise link WQP features to the ATTAINS catchment feature(s) they land in:
      TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
        sf::st_join(., nearby_catchments) %>%
        sf::st_drop_geometry()
      
      if(return == TRUE){
        
        download_attains <- function(baseurl){
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
          
          final_sf <- geojsonsf::geojson_sf(query)
          
          return(final_sf)
          
        }
        
        # CATCHMENT FEATURES 
        ATTAINS_catchments <<- nearby_catchments
        
        # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
        try(ATTAINS_points <<- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?") %>%
              .[nearby_catchments,],
            silent = TRUE)
        
        # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
        try(ATTAINS_lines <<- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?") %>%
              .[nearby_catchments,] %>%
              distinct(assessmentunitidentifier, .keep_all = TRUE) %>%
              left_join(st_drop_geometry(nearby_catchments), by = c("assessmentunitidentifier" = "ATTAINS.assessmentunitidentifier")),
            silent = TRUE)

        # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
        try(ATTAINS_polygons <<- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?") %>%
              .[nearby_catchments,],
            silent = TRUE)
      }
      
      return(st_drop_geometry(nearby_catchments))
      
    }
    
    }))
  
}
