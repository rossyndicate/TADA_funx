TADA_ExportATTAINS <- function(data){
  
  if(nrow(data) == 0){stop("Your WQP dataframe has no observations.")}
  
  suppressMessages(suppressWarnings({
    
    sf::sf_use_s2(FALSE)
    
    # If data is already spatial, just make sure it is in the right CRS
    # and add an index...
    if (!is.null(data) & inherits(data, "sf")) {
      TADA_DataRetrieval_data <- data %>%
        sf::st_transform(4326) %>%
        tibble::rowid_to_column(var = "index")
    } else {
      TADA_DataRetrieval_data <- data %>%
        TADA_MakeSpatial(data = ., crs = 4326) %>%
        # add index for identifying obs with more than one ATTAINS assessment unit
        tibble::rowid_to_column(var = "index")
    }
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
    
    # grab the geospatial data!
    nearby_catchments <- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?") %>%
      # remove unnecessary columns:
      dplyr::select(-c(OBJECTID, GLOBALID)) %>%
      .[TADA_DataRetrieval_data,] %>%
      dplyr::rename_with(~ paste0("ATTAINS.", .), dplyr::everything())
    
    # join TADA sf features to the ATTAINS catchment feature(s) they land on:
    TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
      sf::st_join(., nearby_catchments) %>%
      sf::st_drop_geometry() 
    
    # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
    try(points <- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?") %>%
          .[nearby_catchments,],
        silent = TRUE)
    try(points_mapper <- tibble::tibble(assessmentunitidentifier = unique(lines$assessmentunitidentifier),
                                        colors = inferno(length(unique(lines$assessmentunitidentifier)))) %>%
          dplyr::inner_join(points, by = "assessmentunitidentifier") %>%
          sf::st_as_sf() %>%
          dplyr::mutate(type = "Point"),
        silent = TRUE)
    
    # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
    try(lines <- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?") %>%
          .[nearby_catchments,] %>%
          distinct(assessmentunitidentifier, .keep_all = TRUE),
        silent = TRUE)
    try(lines_mapper <- tibble(assessmentunitidentifier = unique(lines$assessmentunitidentifier),
                               colors = viridis::inferno(length(unique(lines$assessmentunitidentifier)))) %>%
          dplyr::inner_join(lines, by = "assessmentunitidentifier") %>%
          sf::st_as_sf() %>%
          dplyr::mutate(type = "Line"),
        silent = TRUE)
    
    # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
    try(polygons <- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?") %>%
          .[nearby_catchments,],
        silent = TRUE)
    try(polygons_mapper <- tibble::tibble(assessmentunitidentifier = unique(polygons$assessmentunitidentifier),
                                       colors = viridis::inferno(length(unique(polygons$assessmentunitidentifier)))) %>%
          dplyr::inner_join(polygons, by = "assessmentunitidentifier") %>%
          sf::st_as_sf() %>%
          dplyr::mutate(type = "Polygon"),
        silent = TRUE)
  
    full_dataset <- list("WQP" = TADA_with_ATTAINS, 
                         "ATTAINS_points" = points, 
                         "ATTAINS_lines" = lines, 
                         "ATTAINS_polygons" = polygons)
  return(full_dataset)
    
}
  
  