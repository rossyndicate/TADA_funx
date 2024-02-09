#' Finds ATTAINS assessment unit data within the same catchment as water quality observations imported via `TADA_DataRetrieval()`.
#' 
#' @param data A dataframe, often created by `TADA_DataRetrieval()`. Requires columns "TADA.LongitudeMeasure" and "TADA.LatitudeMeasure".
#' 
#' @return A leaflet map visualizing the TADA water quality observations and the linked ATTAINS assessment units. 
#' 
#' @seealso [TADA_DataRetrieval()]
#' 
TADA_ViewATTAINS <- function(data){
  
  suppressWarnings({
    
    sf::sf_use_s2(FALSE)
    
    TADA_DataRetrieval_data <- data %>%
      # Use the USGS projection code (also used in AquaSat) to improve CRS in future iteration of the function!
      sf::st_as_sf(., coords = c("TADA.LongitudeMeasure", "TADA.LatitudeMeasure"), crs = 4326) 
    
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
    
    # POINT FEATURES
    try(points <- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?") %>%
          .[nearby_catchments,])
    try(points_mapper <- tibble::tibble(assessmentunitidentifier = unique(lines$assessmentunitidentifier),
                                        colors = inferno(length(unique(lines$assessmentunitidentifier)))) %>%
          dplyr::inner_join(points, by = "assessmentunitidentifier") %>%
          sf::st_as_sf())
    
    # LINE FEATURES
    try(lines <- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?") %>%
          .[nearby_catchments,] %>%
          distinct(assessmentunitidentifier, .keep_all = TRUE))
    try(lines_mapper <- tibble(assessmentunitidentifier = unique(lines$assessmentunitidentifier),
                               colors = viridis::inferno(length(unique(lines$assessmentunitidentifier)))) %>%
          dplyr::inner_join(lines, by = "assessmentunitidentifier") %>%
          sf::st_as_sf())
    
    # AREA FEATURES
    try(areas <- download_attains(baseurl = "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?") %>%
          .[nearby_catchments,])
    try(areas_mapper <- tibble::tibble(assessmentunitidentifier = unique(areas$assessmentunitidentifier),
                                       colors = viridis::inferno(length(unique(areas$assessmentunitidentifier)))) %>%
          dplyr::inner_join(areas, by = "assessmentunitidentifier") %>%
          sf::st_as_sf())
    
    sumdat <- data  %>% dplyr::group_by(MonitoringLocationIdentifier, 
                                        MonitoringLocationName, TADA.LatitudeMeasure, TADA.LongitudeMeasure) %>% 
      dplyr::summarize(Sample_Count = length(unique(ResultIdentifier)), 
                       Visit_Count = length(unique(ActivityStartDate)), 
                       Parameter_Count = length(unique(TADA.CharacteristicName)), 
                       Organization_Count = length(unique(OrganizationIdentifier)))
    
    
    # Map of WQP points
    map <- leaflet::leaflet() %>% 
      leaflet::addProviderTiles("Esri.WorldTopoMap", 
                                group = "World topo",
                                options = leaflet::providerTileOptions(updateWhenZooming = FALSE, 
                                                                       updateWhenIdle = TRUE)) %>% 
      leaflet::clearShapes() %>% 
      leaflet::fitBounds(lng1 = min(sumdat$TADA.LongitudeMeasure), 
                         lat1 = min(sumdat$TADA.LatitudeMeasure), 
                         lng2 = max(sumdat$TADA.LongitudeMeasure), 
                         lat2 = max(sumdat$TADA.LatitudeMeasure)) %>% 
      leaflet.extras::addResetMapButton() %>% 
      leaflet::addCircleMarkers(data = sumdat, 
                                lng = ~TADA.LongitudeMeasure, lat = ~TADA.LatitudeMeasure, 
                                color = "black", fillColor = "grey", 
                                fillOpacity = 0.7, stroke = TRUE, weight = 1.5, 
                                radius = 5, 
                                popup = paste0("Site ID: ", sumdat$MonitoringLocationIdentifier, 
                                               "<br> Site Name: ", sumdat$MonitoringLocationName, 
                                               "<br> Measurement Count: ", sumdat$Sample_Count, 
                                               "<br> Visit Count: ", sumdat$Visit_Count, 
                                               "<br> Characteristic Count: ", sumdat$Parameter_Count))
    # Add ATTAINS lines features (if they exist):
    try(map <- map %>%
          leaflet::addPolylines(data = lines_mapper,
                                color = ~colors,
                                weight = 2.5,
                                popup = paste0("Assessment Unit Name: ", lines_mapper$assessmentunitname, 
                                               "<br> Assessment Unit ID: ", lines_mapper$assessmentunitidentifier,
                                               "<br> Status: ", lines_mapper$overallstatus)))
    # Add ATTAINS area features (if they exist):
    try(map <- map %>%
          leaflet::addPolygons(data = areas_mapper,
                               color = ~colors,
                               weight = 2.5,
                               popup = paste0("Assessment Unit Name: ", areas_mapper$assessmentunitname, 
                                              "<br> Assessment Unit ID: ", areas_mapper$assessmentunitidentifier,
                                              "<br> Status: ", areas_mapper$overallstatus)))
    
    # Add ATTAINS point features (if they exist):
    try(map <- map %>%
          leaflet::addCircleMarkers(data = points_mapper, 
                                    lng = ~TADA.LongitudeMeasure, lat = ~TADA.LatitudeMeasure, 
                                    color = ~colors, fillColor = ~colors, 
                                    fillOpacity = 0.7, stroke = TRUE, weight = 1.5, 
                                    radius = 5, 
                                    popup = paste0("Assessment Unit Name: ", areas_mapper$assessmentunitname, 
                                                   "<br> Assessment Unit ID: ", areas_mapper$assessmentunitidentifier,
                                                   "<br> Status: ", areas_mapper$overallstatus)))
    
    # Return leaflet map of TADA WQ and its associated ATTAINS data
    return(map)
    
  })
  
}
