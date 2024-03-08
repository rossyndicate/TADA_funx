#' Link catchment-based ATTAINS assessment unit data to Water Quality Portal observations, often imported via `TADA_DataRetrieval()`. This function returns the same raw objects that are mapped in `TADA_ViewATTAINS()`.
#' 
#' @param data A dataframe created by `TADA_DataRetrieval()` or the {sf} equivalent made by `TADA_MakeSpatial()`.
#' @param return Whether to add the associated ATTAINS_catchments, ATTAINS_lines, ATTAINS_points, and ATTAINS_polygons shapefile objects into your Global Environment. TRUE (yes, return) or FALSE (no, do not return). All ATTAINS features are in WGS84 (crs = 4326).
#' 
#' @return The original `TADA_DataRetrieval()` dataframe with additional columns associated with the ATTAINS assessment unit data. 
#' 
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_MakeSpatial()]
#' @seealso [TADA_ViewATTAINS()]
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
#'tada_attains <- TADA_GetATTAINS(data = tada_data, return = TRUE)
#' }

TADA_GetATTAINS <- function(data, return = FALSE){
  
  if(nrow(data) == 0){
    
    print("Your Water Quality Portal dataframe has no observations. Returning an empty dataframe.")
    
    # if no WQP observations, return a modified `data` with empty ATTAINS-related columns:
    no_WQP_data <- data %>%
      dplyr::mutate("ATTAINS.organizationid" = NA, "ATTAINS.submissionid" = NA, "ATTAINS.hasprotectionplan" = NA,
                    "ATTAINS.assessmentunitname" = NA, "ATTAINS.nhdplusid" = NA, "ATTAINS.tas303d" = NA,                                                  
                    "ATTAINS.isthreatened" = NA, "ATTAINS.state" = NA, "ATTAINS.on303dlist" = NA,                                               
                    "ATTAINS.organizationname" = NA, "ATTAINS.region" = NA, "ATTAINS.Shape_Length" = NA,                                             
                    "ATTAINS.reportingcycle" = NA, "ATTAINS.assmnt_joinkey" = NA, "ATTAINS.hastmdl" = NA,                                                  
                    "ATTAINS.orgtype" = NA, "ATTAINS.permid_joinkey" = NA, "ATTAINS.catchmentistribal" = NA,                                        
                    "ATTAINS.ircategory" = NA, "ATTAINS.waterbodyreportlink" = NA, "ATTAINS.assessmentunitidentifier" = NA,                                 
                    "ATTAINS.overallstatus" = NA, "ATTAINS.isassessed" = NA, "ATTAINS.isimpaired" = NA,                                               
                    "ATTAINS.has4bplan" = NA, "ATTAINS.huc12" = NA, "ATTAINS.hasalternativeplan" = NA,                                      
                    "ATTAINS.visionpriority303d" = NA, "ATTAINS.areasqkm" = NA, "ATTAINS.catchmentareasqkm" = NA,                                       
                    "ATTAINS.catchmentstatecode" = NA, "ATTAINS.catchmentresolution" = NA, "ATTAINS.Shape_Area" = NA)
    
    return(no_WQP_data)
    
  }
  
  suppressMessages(suppressWarnings({
    
    sf::sf_use_s2(FALSE)
    
    # If data is already spatial, just make sure it is in the right CRS
    # and add an index as the WQP observations' unique IDs...
    if (!is.null(data) & inherits(data, "sf")) {
      TADA_DataRetrieval_data <- data %>%
        sf::st_transform(4326) %>%
        tibble::rowid_to_column(var = "index")
    } else {
      # ... Otherwise transform into a spatial object then do the same thing:
      TADA_DataRetrieval_data <- data %>%
        #convert dataframe to a spatial object
        TADA_MakeSpatial(data = ., crs = 4326) %>%
        # add unique WQP ID for identifying obs with more than one ATTAINS assessment unit
        tibble::rowid_to_column(var = "index")
    }
    
    # grab the ATTAINS catchments within our WQP bbox:
    nearby_catchments <- NULL
    # (Wrapped with "try" because it is possible that no ATTAINS data exists in the bbox.)
    try(nearby_catchments <- fetchATTAINS(type = "catchments", data = TADA_DataRetrieval_data) %>%
          # remove unnecessary columns:
          dplyr::select(-c(OBJECTID, GLOBALID)) %>%
          # select only catchments that have WQP observations in them:
          .[TADA_DataRetrieval_data, ] %>%
          # add prefix "ATTAINS" to ATTAINS data
          dplyr::rename_with(~ paste0("ATTAINS.", .), dplyr::everything()) %>%
          # get rid of dupes (as a precaution)
          dplyr::distinct(.keep_all = TRUE),
        silent = TRUE)
    
    # if no ATTAINS data, return original dataframe with empty ATTAINS columns:
    if(is.null(nearby_catchments)){
      
      print("There are no ATTAINS features associated with these WQP observations. Returning original dataframe with empty ATTAINS columns.")
      
      # return a modified `data` with empty ATTAINS-related columns:
      no_ATTAINS_data <- data %>%
        dplyr::mutate("ATTAINS.organizationid" = NA, "ATTAINS.submissionid" = NA, "ATTAINS.hasprotectionplan" = NA,
                      "ATTAINS.assessmentunitname" = NA, "ATTAINS.nhdplusid" = NA, "ATTAINS.tas303d" = NA,                                                  
                      "ATTAINS.isthreatened" = NA, "ATTAINS.state" = NA, "ATTAINS.on303dlist" = NA,                                               
                      "ATTAINS.organizationname" = NA, "ATTAINS.region" = NA, "ATTAINS.Shape_Length" = NA,                                             
                      "ATTAINS.reportingcycle" = NA, "ATTAINS.assmnt_joinkey" = NA, "ATTAINS.hastmdl" = NA,                                                  
                      "ATTAINS.orgtype" = NA, "ATTAINS.permid_joinkey" = NA, "ATTAINS.catchmentistribal" = NA,                                        
                      "ATTAINS.ircategory" = NA, "ATTAINS.waterbodyreportlink" = NA, "ATTAINS.assessmentunitidentifier" = NA,                                 
                      "ATTAINS.overallstatus" = NA, "ATTAINS.isassessed" = NA, "ATTAINS.isimpaired" = NA,                                               
                      "ATTAINS.has4bplan" = NA, "ATTAINS.huc12" = NA, "ATTAINS.hasalternativeplan" = NA,                                      
                      "ATTAINS.visionpriority303d" = NA, "ATTAINS.areasqkm" = NA, "ATTAINS.catchmentareasqkm" = NA,                                       
                      "ATTAINS.catchmentstatecode" = NA, "ATTAINS.catchmentresolution" = NA, "ATTAINS.Shape_Area" = NA)
      
      return(no_ATTAINS_data)
      
    } else {
      # ... otherwise link WQP features to the ATTAINS catchment feature(s) they land in:
      TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
        # left join = TRUE to preserve all observations (with or without ATTAINS features):
        sf::st_join(., nearby_catchments, left = TRUE)
      
      if(return == TRUE){
        
        # ... otherwise link WQP features to the ATTAINS catchment feature(s) they land in:
        TADA_data <- TADA_DataRetrieval_data %>%
          # left join = TRUE to preserve all observations (with or without ATTAINS features):
          sf::st_join(., nearby_catchments, left = TRUE)
        
        # CATCHMENT FEATURES 
        # use original catchment pull, but return column names to original
        ATTAINS_catchments <<- nearby_catchments
        colnames(ATTAINS_catchments) <- gsub("ATTAINS.", "", colnames(ATTAINS_catchments)) 
        # due to the rename, must re-set geometry column:
        sf::st_geometry(ATTAINS_catchments) <- "geometry"
        
        # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
        try(ATTAINS_points <<- fetchATTAINS(type = "points", data = TADA_DataRetrieval_data) %>%
              # subset to only ATTAINS point features in the same NHD HR catchments as WQP observations
              .[nearby_catchments,] %>%
              # make sure no duplicate features exist
              distinct(assessmentunitidentifier, .keep_all = TRUE),
            silent = TRUE)
        
        
        # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
        try(ATTAINS_lines <<- fetchATTAINS(type = "lines", data = TADA_DataRetrieval_data) %>%
              # subset to only ATTAINS line features in the same NHD HR catchments as WQP observations
              .[nearby_catchments,] %>%
              # make sure no duplicate line features exist
              distinct(assessmentunitidentifier, .keep_all = TRUE),
            silent = TRUE)
        
        # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
        try(ATTAINS_polygons <<- fetchATTAINS(type = "polygons", data = TADA_DataRetrieval_data) %>%
              # subset to only ATTAINS polygon features in the same NHD HR catchments as WQP observations
              .[nearby_catchments,] %>%
              # make sure no duplicate polygon features exist
              distinct(assessmentunitidentifier, .keep_all = TRUE),
            silent = TRUE)
      }
      
      return(TADA_with_ATTAINS)
      
    }
    
  }))
  
}
