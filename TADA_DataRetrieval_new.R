TADA_DataRetrieval_new <- function (startDate = "null", endDate = "null", countycode = "null", 
                                    huc = "null", sf = "null", siteid = "null", siteType = "null", characteristicName = "null", 
                                    characteristicType = "null", sampleMedia = "null", statecode = "null",
                                    organization = "null", project = "null", providers = "null",
                                    applyautoclean = TRUE) {
  
  # SHAPEFILE(S) INCORPORATION
  if (!is.null(sf) & inherits(sf, "sf")) {
    
    suppressMessages(sf::sf_use_s2(FALSE))
    
    sf_counties <- suppressWarnings(suppressMessages(sf %>%
                                                       sf::st_intersection(x = ., y = sf::st_transform(tigris::counties(), sf::st_crs(sf))) %>%
                                                       dplyr::bind_rows() %>%
                                                       dplyr::select(STATE, STATEFP, NAME, COUNTYFP) %>%
                                                       dplyr::mutate(cc = paste0("US:",STATEFP,":",COUNTYFP))))
    
    sf_hucs <- suppressWarnings(suppressMessages(sf %>%
                                                   dplyr::summarize() %>%
                                                   nhdplusTools::get_huc(AOI = ., type = "huc12") %>%
                                                   dplyr::mutate(huc10 = substr(huc12, 1, 10),
                                                                 huc8 = substr(huc12, 1, 8),
                                                                 huc6 = substr(huc12, 1, 6),
                                                                 huc4 = substr(huc12, 1, 4),
                                                                 huc2 = substr(huc12, 1, 2))))
    
    hucs <- c(sf_hucs$huc2, sf_hucs$huc4, sf_hucs$huc6, sf_hucs$huc8, sf_hucs$huc10, sf_hucs$huc12)
    
    if (countycode != "null" & countycode %in% sf_counties$cc != TRUE) {
      
      stop("Your shapefile does not overlap your county (or counties) of interest.")
      
    }
    
    if (statecode != "null" & statecode %in% sf_counties$STATE != TRUE) {
      
      stop("Your shapefile does not overlap your state(s) of interest.")
      
    }
    
    if (huc != "null" & huc %in% hucs != TRUE) {
      
      stop("Your shapefile does not overlap your HUC(s) of interest.")
      
    }
  }
  
  WQPquery <- list()
  if (length(statecode) > 1) {
    WQPquery <- c(WQPquery, statecode = list(statecode))
  } else if (statecode != "null") {
    WQPquery <- c(WQPquery, statecode = statecode)
  }
  if (length(huc) > 1) {
    WQPquery <- c(WQPquery, huc = list(huc))
  } else if (huc != "null") {
    WQPquery <- c(WQPquery, huc = huc)
  }
  if (length(startDate) > 1) {
    if (is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], 
                                                          orders = "ymd")))) {
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, startDate = list(startDate))
  } else if (startDate != "null") {
    if (is.na(suppressWarnings(lubridate::parse_date_time(startDate, 
                                                          orders = "ymd")))) {
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, startDate = startDate)
  }
  if (length(countycode) > 1) {
    WQPquery <- c(WQPquery, countycode = list(countycode))
  } else if (countycode != "null") {
    WQPquery <- c(WQPquery, countycode = countycode)
  }
  if (length(siteid) > 1) {
    WQPquery <- c(WQPquery, siteid = list(siteid))
  } else if (siteid != "null") {
    WQPquery <- c(WQPquery, siteid = siteid)
  }
  if (length(siteType) > 1) {
    WQPquery <- c(WQPquery, siteType = list(siteType))
  } else if (siteType != "null") {
    WQPquery <- c(WQPquery, siteType = siteType)
  }
  if (length(characteristicName) > 1) {
    WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
  } else if (characteristicName != "null") {
    WQPquery <- c(WQPquery, characteristicName = characteristicName)
  }
  if (length(characteristicType) > 1) {
    WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
  } else if (characteristicType != "null") {
    WQPquery <- c(WQPquery, characteristicType = characteristicType)
  }
  if (length(sampleMedia) > 1) {
    WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia))
  } else if (sampleMedia != "null") {
    WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
  }
  if (length(project) > 1) {
    WQPquery <- c(WQPquery, project = list(project))
  } else if (project != "null") {
    WQPquery <- c(WQPquery, project = project)
  }
  if (length(providers) > 1) {
    WQPquery <- c(WQPquery, providers = list(providers))
  } else if (providers != "null") {
    WQPquery <- c(WQPquery, providers = providers)
  }
  if (length(organization) > 1) {
    WQPquery <- c(WQPquery, organization = list(organization))
  } else if (organization != "null") {
    WQPquery <- c(WQPquery, organization = organization)
  }
  if (length(endDate) > 1) {
    if (is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], 
                                                          orders = "ymd")))) {
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, endDate = list(endDate))
  } else if (endDate != "null") {
    if (is.na(suppressWarnings(lubridate::parse_date_time(endDate, 
                                                          orders = "ymd")))) {
      stop("Incorrect date format. Please use the format YYYY-MM-DD.")
    }
    WQPquery <- c(WQPquery, endDate = endDate)
  }
  print("Downloading WQP query results. This may take some time depending upon the query size.")
  results.DR <- dataRetrieval::readWQPdata(WQPquery, dataProfile = "resultPhysChem", 
                                           ignore_attributes = TRUE)
  if ((nrow(results.DR) > 0) == FALSE) {
    print("Returning empty results dataframe: Your WQP query returned no results (no data available). Try a different query. Removing some of your query filters OR broadening your search area may help.")
    TADAprofile.clean <- results.DR
  } else {
    sites.DR <- dataRetrieval::whatWQPsites(WQPquery)
    projects.DR <- dataRetrieval::readWQPdata(WQPquery, 
                                              ignore_attributes = TRUE, service = "Project")
    TADAprofile <- TADA_JoinWQPProfiles(FullPhysChem = results.DR, 
                                        Sites = sites.DR, Projects = projects.DR)
    cols <- names(TADAprofile)
    TADAprofile <- TADAprofile %>% dplyr::mutate_at(cols, 
                                                    as.character)
    if (applyautoclean == TRUE) {
      print("Data successfully downloaded. Running TADA_AutoClean function.")
      TADAprofile.clean <- TADA_AutoClean(TADAprofile)
    } else {
      TADAprofile.clean <- TADAprofile
    }
  }
  
  if (!is.null(sf) & inherits(sf, "sf")) {
    
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
    
    if(applyautoclean == TRUE){
      
      TADAprofile.clean <- suppressMessages(TADAprofile.clean %>%
                                                dplyr::mutate(lon = TADA.LongitudeMeasure,
                                                              lat = TADA.LatitudeMeasure) %>%
                                                # Add EPSG codes
                                                dplyr::left_join(x = .,
                                                                 y = epsg_codes,
                                                                 by = "HorizontalCoordinateReferenceSystemDatumName") %>%
                                                # Group by CRS 
                                                split(f = .$HorizontalCoordinateReferenceSystemDatumName) %>%
                                                # Transform and re-stack
                                                purrr::map_df(.x = .,
                                                              .f = ~ .x %>%
                                                                sf::st_as_sf(coords = c("lon", "lat"),
                                                                             crs = unique(.x$epsg)) %>%
                                                                # transform to the CRS of the sf object
                                                                sf::st_transform(sf::st_crs(sf))) %>%
                                                .[sf, ] %>%
                                                sf::st_drop_geometry() %>%
                                                dplyr::select(-epsg))
    } else {
      
      TADAprofile.clean <- suppressMessages(TADAprofile.clean %>%
                                                dplyr::mutate(lon = LongitudeMeasure,
                                                              lat = LatitudeMeasure) %>%
                                                # Add EPSG codes
                                                dplyr::left_join(x = .,
                                                                 y = epsg_codes,
                                                                 by = "HorizontalCoordinateReferenceSystemDatumName") %>%
                                                # Group by CRS 
                                                split(f = .$HorizontalCoordinateReferenceSystemDatumName) %>%
                                                # Transform and re-stack
                                                purrr::map_df(.x = .,
                                                              .f = ~ .x %>%
                                                                sf::st_as_sf(coords = c("lon", "lat"),
                                                                             crs = unique(.x$epsg)) %>%
                                                                # transform to the CRS of the sf object
                                                                sf::st_transform(sf::st_crs(sf))) %>%
                                                .[sf, ] %>%
                                                sf::st_drop_geometry() %>%
                                                dplyr::select(-epsg))
    }
    
  }
  return(TADAprofile.clean)
}
