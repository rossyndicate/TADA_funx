#' Fetch ATTAINS features within a bounding box produced from a set of TADA spatial features.
#' 
#' @param data A dataframe developed using `TADA_DataRetrieval()` or `TADA_MakeSpatial()`.
#' @param type The type of ATTAINS data you would like to fetch ("lines", "points", "polygons", "catchments"). All ATTAINS features are returned in WGS84 (crs = 4326).
#' 
#' @return spatial features that are within the spatial bounding box of water quality observations.
#' 
#' @seealso [TADA_MakeSpatial()]
#' @seealso [TADA_DataRetrieval()]
#' 
#' @examples
#' \dontrun{
#'tada_data <- TADA_DataRetrieval(startDate = "1990-01-01",
#'                                endDate = "1995-12-31",
#'                                characteristicName = "pH",
#'                                statecode = "NV",
#'                                applyautoclean = TRUE)
#'                                  
#'nv_attains_lines <- fetchATTAINS(data = tada_data, type = "lines")
#' }

fetchATTAINS <- function(data, type = NULL) {
  
  if(is.null(data) | nrow(data) == 0){
    stop("There is no data in your `data` object to use as a bounding box for selecting ATTAINS features.")
  }
  
  # If data is already spatial, just make sure it is in the right CRS
  # and add an index as the WQP observations' unique IDs...
  if (!is.null(data) & inherits(data, "sf")) {
    data <- data %>%
      sf::st_transform(4326) 
  } else {
    # ... Otherwise transform into a spatial object then do the same thing:
    data <- data %>%
      #convert dataframe to a spatial object
      TADA_MakeSpatial(data = ., crs = 4326) 
  }
  
  if(is.null(type)){
    stop("Please select the type of ATTAINS data you would like to import ('catchments', 'lines', 'points', or 'polygons').")
  }
  
  # select baseurl based on type of ATTAINS data you want to download:
  if(type == "catchments"){
    baseurl <- "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?"
  }else if(type == "points"){
    baseurl <-  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?"
  }else if(type == "lines"){
    baseurl <-  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?"
  }else if(type == "polygons"){
    baseurl <-  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?"
  }
  
  #starting at feature 1 (i.e., no offset):
  offset <- 0 
  # empty list to store all features in
  all_features <- list()
  
  # EPSG we want our ATTAINS data to be in (always 4326 for this function)
  epsg <- sf::st_crs(data)$epsg
  
  # bounding box (with some minor wiggle) of user's WQP data
  suppressMessages(suppressWarnings({bbox <- data %>% 
    sf::st_buffer(0.001) %>% 
    sf::st_bbox(data) %>%
    # convert bounding box to characters
    toString(.) %>% 
    # encode for use within the API URL
    urltools::url_encode(.)}))
  
  # The ATTAINS API has a limit of 2000 features that can be pulled in at once.
  # Therefore, we must split the call into manageable "chunks" using a moving
  # window of what features to pull in, then munging all the separate API calls 
  # together.
  
  repeat {
   
    query <- urltools::param_set(baseurl, key = "geometry", value = bbox) %>%
      urltools::param_set(key = "inSR", value = epsg) %>%
      # Total of 2000 features at a time...
      urltools::param_set(key = "resultRecordCount", value = 2000) %>%
      # ... starting at the "offset":
      urltools::param_set(key = "resultOffset", value = offset) %>%
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
    
    # Fetch features within the offset window and append to list:
    features <- suppressMessages(suppressWarnings({tryCatch({
      geojsonsf::geojson_sf(query)
    }, error = function(e) {
      NULL
    })}))
    
    
    # Exit loop if no more features or error occurred
    if (is.null(features) || nrow(features) == 0) {
      break
    }
    
    all_features <- c(all_features, list(features))
    # once done, change offset by 2000 features:
    offset <- offset + 2000
    
    if(offset == 4000){print("Your TADA data covers a large spatial range. The ATTAINS pull may take a while.")}
    
  }
  
  all_features <- bind_rows(all_features)
  
  return(all_features)
  
}
