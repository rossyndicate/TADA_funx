#' Finds ATTAINS assessment unit data within the same catchment as water quality observations imported via `TADA_DataRetrieval()`.
#' 
#' @param data A dataframe, created by `TADA_DataRetrieval()`. 
#' 
#' @return A leaflet map visualizing the TADA water quality observations and the linked ATTAINS assessment units. All maps are in WGS84.
#' 
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_GetATTAINS()]
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'tada_data <- TADA_DataRetrieval(startDate = "1990-01-01",
#'                                endDate = "1995-12-31",
#'                                characteristicName = "pH",
#'                                statecode = "NV",
#'                                applyautoclean = TRUE)
#'                                  
#'TADA_ViewATTAINS(data = tada_data)
#' }

TADA_ViewATTAINS <- function(data){
  
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
      #... otherwise, make it spatial then do the same thing.
      TADA_DataRetrieval_data <- data %>%
        TADA_MakeSpatial(data = ., crs = 4326) %>%
        # add index for identifying obs with more than one ATTAINS assessment unit
        tibble::rowid_to_column(var = "index")
    }
    
    nearby_catchments <- NULL
    # grab the ATTAINS catchment-level data:
    try(nearby_catchments <- fetchATTAINS(type = "catchments", data = TADA_DataRetrieval_data) %>%
          # remove unnecessary columns:
          dplyr::select(-c(OBJECTID, GLOBALID)) %>%
          # subset catchments to only those with user-supplied WQP observations in them:
          .[TADA_DataRetrieval_data,] %>%
          # tack on ATTAINS to the beginning of every column name:
          dplyr::rename_with(~ paste0("ATTAINS.", .), dplyr::everything()),
        silent = TRUE)
    
    if(is.null(nearby_catchments) == TRUE) {
      stop("There are no ATTAINS features associated with these WQP observations.")
    }
    
    
    # join TADA sf features to the ATTAINS catchment feature(s) they land on:
    TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
      sf::st_join(., nearby_catchments) %>%
      # drop spatial:
      sf::st_drop_geometry() 
    
    colors = data.frame(
      overallstatus = c("Not Supporting", "Fully Supporting", "Not Assessed"),
      col = c("#DC851E", "#059FA4", "#A1A522"),
      dark_col = c("#813B00", "#005258", "#4F5900"),
      priority = c(1, 2, 3))
    
    # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
    points <- NULL
    try(points <- fetchATTAINS(type = "points", data = TADA_DataRetrieval_data) %>%
          .[nearby_catchments,],
        silent = TRUE)
    try(points_mapper <- points %>%
          left_join(., colors, by = "overallstatus") %>%
          mutate(type = "Point Feature"),
        silent = TRUE)
    
    # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
    lines <- NULL
    try(lines <- fetchATTAINS(type = "lines", data = TADA_DataRetrieval_data) %>%
          .[nearby_catchments,],
        silent = TRUE)
    try(lines_mapper <- lines %>%
          left_join(., colors, by = "overallstatus") %>%
          mutate(type = "Line Feature"),
        silent = TRUE)
    
    # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
    polygons <- NULL
    try(polygons <- fetchATTAINS(type = "polygons", data = TADA_DataRetrieval_data) %>%
          .[nearby_catchments,],
        silent = TRUE)
    try(polygons_mapper <- polygons %>%
          left_join(., colors, by = "overallstatus") %>%
          mutate(type = "Polygon Feature"),
        silent = TRUE)
    
    # Rename WQP columns, depending on whether or not the user applied TADA's autoclean to the df:
    try(TADA_with_ATTAINS <- TADA_with_ATTAINS %>%
          rename(TADA.LatitudeMeasure = LatitudeMeasure,
                 TADA.LongitudeMeasure = LongitudeMeasure,
                 TADA.CharacteristicName = CharacteristicName), 
        silent = TRUE)
    # Develop WQP site stats (e.g. count of observations, parameters, per site)
    sumdat <- TADA_with_ATTAINS %>%
      dplyr::group_by(MonitoringLocationIdentifier, MonitoringLocationName, TADA.LatitudeMeasure, TADA.LongitudeMeasure) %>% 
      dplyr::summarize(Sample_Count = length(unique(ResultIdentifier)), 
                       Visit_Count = length(unique(ActivityStartDate)), 
                       Parameter_Count = length(unique(TADA.CharacteristicName)), 
                       Organization_Count = length(unique(OrganizationIdentifier)),
                       ATTAINS_AUs = as.character(list(unique(ATTAINS.assessmentunitidentifier)))) %>%
      mutate(ATTAINS_AUs = ifelse(is.na(ATTAINS_AUs), "None", ATTAINS_AUs),
             TADA.LatitudeMeasure = as.numeric(TADA.LatitudeMeasure),
             TADA.LongitudeMeasure = as.numeric(TADA.LongitudeMeasure))
    
    # Basemap for AOI:
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
      leaflet.extras::addResetMapButton()  %>%
      leaflet::addLegend(position = "bottomright",
                         colors = c("#DC851E", "#059FA4", "#A1A522", "black", NA),
                         labels = c("ATTAINS: Not Supporting", "ATTAINS: Supporting", "ATTAINS: Not Assessed", "Water Quality Observation(s)",
                                    "NHD HR catchments containing water quality observations are represented as clear polygons with black outlines."),
                         opacity = 1,
                         title = "Legend")
    
    # Add ATTAINS catchment outlines (if they exist):
    try(map <- map %>%
          leaflet::addPolygons(data = nearby_catchments,
                               color = "black",
                               weight = 1, fillOpacity = 0,
                               popup = paste0("NHDPlus HR Catchment ID: ", nearby_catchments$ATTAINS.nhdplusid)),
        silent = TRUE)
    
    # Add ATTAINS polygon features (if they exist):
    try(map <- map %>%
          leaflet::addPolygons(data = polygons_mapper,
                               color = ~polygons_mapper$col,
                               fill = ~polygons_mapper$col,
                               weight = 3, fillOpacity = 1,
                               popup = paste0("Assessment Unit Name: ", polygons_mapper$assessmentunitname, 
                                              "<br> Assessment Unit ID: ", polygons_mapper$assessmentunitidentifier,
                                              "<br> Status: ", polygons_mapper$overallstatus,
                                              "<br> Assessment Unit Type: ", polygons_mapper$type,
                                              "<br> <a href=", polygons_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>")),
        silent = TRUE)
    
    # Add ATTAINS lines features (if they exist):
    try(map <- map %>%
          leaflet::addPolylines(data = lines_mapper,
                                color = ~lines_mapper$col,
                                weight = 4, fillOpacity = 1,
                                popup = paste0("Assessment Unit Name: ", lines_mapper$assessmentunitname, 
                                               "<br> Assessment Unit ID: ", lines_mapper$assessmentunitidentifier,
                                               "<br> Status: ", lines_mapper$overallstatus,
                                               "<br> Assessment Unit Type: ", lines_mapper$type,
                                               "<br> <a href=", lines_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>")),
        silent = TRUE)
    
    # Add ATTAINS point features (if they exist):
    try(map <- map %>%
          leaflet::addCircleMarkers(data = points_mapper, 
                                    lng = ~LongitudeMeasure, lat = ~LatitudeMeasure, 
                                    color = ~points_mapper$col, fillColor = ~points_mapper$col, 
                                    fillOpacity = 1, stroke = TRUE, weight = 1.5, radius = 5, 
                                    popup = paste0("Assessment Unit Name: ", points_mapper$assessmentunitname, 
                                                   "<br> Assessment Unit ID: ", points_mapper$assessmentunitidentifier,
                                                   "<br> Status: ", points_mapper$overallstatus,
                                                   "<br> Assessment Unit Type: ", points_mapper$type,
                                                   "<br> <a href=", points_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>")),
        silent = TRUE)
    
    # Add WQP observation features (should always exist):
    try(map <- map %>%
          leaflet::addCircleMarkers(data = sumdat, 
                                    lng = ~sumdat$TADA.LongitudeMeasure, lat = ~sumdat$TADA.LatitudeMeasure, 
                                    color = "grey", fillColor = "black", 
                                    fillOpacity = 0.8, stroke = TRUE, weight = 1.5, radius = 6, 
                                    popup = paste0("Site ID: ", sumdat$MonitoringLocationIdentifier, 
                                                   "<br> Site Name: ", sumdat$MonitoringLocationName, 
                                                   "<br> Measurement Count: ", sumdat$Sample_Count, 
                                                   "<br> Visit Count: ", sumdat$Visit_Count, 
                                                   "<br> Characteristic Count: ", sumdat$Parameter_Count,
                                                   "<br> ATTAINS Assessment Unit(s): ", sumdat$ATTAINS_AUs)),
        silent = TRUE)
    
    if(is.null(lines) == TRUE & is.null(points) == TRUE & is.null(polygons) == TRUE) {
      print("No ATTAINS data associated with this Water Quality Portal data.")
    }
    
    # Return leaflet map of TADA WQ and its associated ATTAINS data
    return(map)
    
  }))
  
}

