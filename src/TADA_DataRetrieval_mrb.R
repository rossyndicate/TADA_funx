#' Generate TADA-compatible dataframe from WQP Data
#'
#' Retrieve data from Water Quality Portal (WQP) and generate a TADA-compatible
#' dataframe. Note that the inputs (e.g. project, organization, siteType) with the
#' exceptions of endDate and startDate match the web service call format from the
#' online WQP GUI. endDate and startDate match the format suggested in USGS's
#' dataRetrieval package (endDate = "YYYY-MM-DD"), which is a more familiar date
#' format for R users than the WQP GUI's endDateHi = "MM-DD-YYYY".
#'
#' Multiple fields are queried together using AND logic, but multiple values within
#' one field are queried together using OR logic. For example, within
#' characteristicName, if you enter, c("pH", "Dissolved oxygen (DO)), the
#' function will return all results that are "pH" OR "Dissolved oxygen (DO)". Similarly,
#' if you enter c("VA", "IL"), the function will return results from Virginia OR Illinois.
#' But the combo of these fields are ANDs: The function will return any pH and DO data
#' from only Virginia or Illinois; the data must fit into one of the values from BOTH
#' of the query fields.
#' characteristicName and Characteristic Group also work as an AND, therefore the
#' characteristicName must fall within the Characteristic Group when both are entered.
#' sf, huc, countycode, and state also work as ANDs, therefore the huc, countycode, and 
#' state must fall within the sf object when any combination of these are entered.
#'
#'
#' Users can reference the \href{https://www.epa.gov/waterdata/storage-and-retrieval-and-water-quality-exchange-domain-services-and-downloads}{WQX domain tables}
#' to find allowable values for queries, e.g., reference the WQX domain table to find countycode and statecode: https://cdx.epa.gov/wqx/download/DomainValues/County_CSV.zip
#' Alternatively, you can use the WQP services to find areas where data is available in the US: https://www.waterqualitydata.us/Codes/countycode
#'
#' TADA_DataRetrieval automatically runs TADA_AutoClean on the incoming dataset. TADA_AutoClean
#' is important for categorizing result value and detection limit data, as well as
#' harmonizing key columns used in TADA. See ?TADA_AutoClean for more `````````````````````````````information.
#'
#' Note: TADA_DataRetrieval (by leveraging dataRetrieval),  automatically converts
#' the date times to UTC. It also automatically converts the data to dates,
#' datetimes, numerics based on a standard algorithm. See: ?dataRetrieval::readWQPdata
#'
#' @param startDate Start Date string in the format YYYY-MM-DD, for example, "2020-01-01"
#' @param endDate End Date string in the format YYYY-MM-DD, for example, "2020-01-01"
#' @param aoi_sf An sf object to use for a query area of interest
#' @param countycode FIPS county name. Note that a state code must also be supplied (e.g. statecode = "AL", countycode = "Chilton").
#' @param huc A numeric code denoting a hydrologic unit. Example: "04030202". Different size hucs can be entered.
#' @param siteid Unique monitoring location identifier
#' @param aoi_sf A polygon to subset the WQP data by
#' @param siteType Type of waterbody
#' @param tribal_area_type One of the six tribal spatial layers: "Alaska Native Allotments", "Alaska Native Villages",  "American Indian Reservations", "Off-reservation Trust Lands", "Oklahoma Tribal Statistical Areas", or "Virginia Federally Recognized Tribes".
#' @param tribe_name_parcel The name of a tribe corresponding to an entry in the TRIBE_NAME field of the specified tribal_area_type. OR if the type is Alaska Native Allotments" then the corresponding PARCEL_NO.
#' @param characteristicName Name of parameter
#' @param characteristicType Groups of environmental measurements/parameters.
#' @param sampleMedia Sampling substrate such as water, air, or sediment
#' @param statecode FIPS state alpha code that identifies a state (e.g. statecode = "DE" for Delaware)
#' @param organization A string of letters and/or numbers (some additional characters also possible) used to signify an organization with data in the Water Quality Portal
#' @param project A string of letters and/or numbers (some additional characters also possible) used to signify a project with data in the Water Quality Portal
#' @param providers Leave blank to include all, or specify "STEWARDS", "STORET" (i.e., WQX), and/or "NWIS".
#' @param applyautoclean Logical, defaults to TRUE. Applies TADA_AutoClean function on the returned data profile.
#'
#' @return TADA-compatible dataframe
#'
#' @note
#' Alaska Native Villages and Virginia Federally Recognized Tribes are point
#' geometries in the Map Service, not polygons. At the time of this writing they
#' do not return any data when used for WQP bbox queries and so are set to return
#' errors when used with this function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tada1 <- TADA_DataRetrieval(statecode = "WI", countycode = "Dane", characteristicName = "Phosphorus")
#'
#' tada2 <- TADA_DataRetrieval(statecode = "UT", characteristicName = c("Ammonia", "Nitrate", "Nitrogen"))
#'
#' tada3 <- TADA_DataRetrieval(statecode = "SC", countycode = "Abbeville")
#'
#' # example for CT
#' tada4 <- TADA_DataRetrieval(statecode = "CT", startDate = "2020-10-01")
#'
#'
#' # note that countycode queries require a statecode (see example below)
#' tada5 <- TADA_DataRetrieval(countycode = "US:02:020")
#'
#' # example for NM
#' tada6 <- TADA_DataRetrieval(
#'   statecode = "NM",
#'   characteristicName = c(
#'     "Ammonia",
#'     "Nitrate",
#'     "Nitrogen"
#'   ),
#'   startDate = "2020-05-01"
#' )
#'
#' # example for AK project
#' tada7 <- TADA_DataRetrieval(project = "Anchorage Bacteria 20-21")
#'
#' # another example for AK
#' tada8 <- TADA_DataRetrieval(
#'   statecode = "AK",
#'   characteristicName = c(
#'     "Fecal Coliform",
#'     "Escherichia coli",
#'     "Enterococcus",
#'     "Ammonia",
#'     "Nitrate",
#'     "Nitrogen"
#'   ),
#'   startDate = "2018-05-01"
#' )
#'
#' # example for tribes
#' # Download data for many of the ATTAINS participating tribes
#' # Note this query may take about a half hour to run
#' # https://www.itecmembers.org/attains/
#' # ATTAINS participating tribes also have tribal pages in EPA's
#' # How's My Waterway Application
#' # Example: https://mywaterway.epa.gov/tribe/SFNOES
#' #
#' # Sac & Fox Nation, Oklahoma "SFNOES_WQX"
#' # Citizen Potawatomi Nation, Oklahoma "CPNWATER"
#' # Delaware Nation, Oklahoma "DELAWARENATION"
#' # Hoopa Valley Tribe, California "HVTEPA_WQX"
#' # Otoe Missouria Tribe of Oklahoma "O_MTRIBE_WQX"
#' # Minnesota Chippewa Tribe, Minnesota (Fond du Lac Band) "FONDULAC_WQX"
#' # Pueblo of San Ildefonso, New Mexico "SANILDEFONSODECP"
#' # Pueblo of Santa Ana, New Mexico "PUEBLO_SANTAANA"
#' # Pueblo of Tesuque, New Mexico "PUEBLOOFTESUQUE"
#' # Red Lake Band of Chippewa Indians, Minnesota "REDLAKE_WQX"
#' # Seneca-Cayuga Nation "SCEQ"
#' # The Chickasaw Nation "CNENVSER"
#' # The Choctaw Nation of Oklahoma "CHOCNATWQX"
#' # Wyandotte Nation "WNENVDPT_WQX"
#' # Pueblo of Pojoaque "PUEBLO_POJOAQUE"
#'
#' tada9 <- TADA_DataRetrieval(organization = c(
#'   "SFNOES_WQX",
#'   "CPNWATER",
#'   "DELAWARENATION",
#'   "HVTEPA_WQX",
#'   "O_MTRIBE_WQX",
#'   "FONDULAC_WQX",
#'   "SANILDEFONSODECP",
#'   "PUEBLO_SANTAANA",
#'   "PUEBLOOFTESUQUE",
#'   "REDLAKE_WQX",
#'   "SCEQ",
#'   "CNENVSER",
#'   "CHOCNATWQX",
#'   "WNENVDPT_WQX",
#'   "PUEBLO_POJOAQUE"
#' ))
#'
#' # query only NWIS data for a 10 year period in CT
#' tada10 <- TADA_DataRetrieval(
#'   startDate = "2013-01-01",
#'   endDate = "2022-12-31",
#'   sampleMedia = c("Water", "water"),
#'   statecode = "CT", # consider downloading only 1 state at a time
#'   providers = "NWIS",
#'   applyautoclean = FALSE
#' )
#' }
#' 
#' # query using US Census Bureau Tribal Boundaries (or any other shapefile input by the user)
#' sf_example <- tigris::native_areas() %>% dplyr::filter(NAMELSAD == "Gila River Indian Reservation")
#'
#' TADA_selection_1 <- TADA_DataRetrieval_new(startDate = "1995-01-01",
#'                                           endDate = "1998-08-05",
#'                                           characteristicName = "pH",
#'                                           aoi_sf = sf_example)
#' 
#' 
#' 
#' 
#'
TADA_DataRetrieval_test <- function(startDate = NULL,
                                    endDate = NULL,
                                    aoi_sf = NULL,
                                    countrycode = NULL,
                                    countycode = NULL,
                                    huc = NULL,
                                    siteid = NULL,
                                    siteType = NULL,
                                    tribal_area_type = NULL,
                                    tribe_name_parcel = NULL,
                                    characteristicName = NULL,
                                    characteristicType = NULL,
                                    sampleMedia = NULL,
                                    statecode = NULL,
                                    organization = NULL,
                                    project = NULL,
                                    providers = NULL,
                                    applyautoclean = TRUE) {
  
  
  # Check for incomplete or inconsistent inputs:
  
  # If both an sf object and tribe information are provided it's unclear what
  # the priority should be for the query
  if( !is.null(aoi_sf) & (!is.null(tribal_area_type) |
                          !is.null(tribe_name_parcel)) ){
    stop(
      paste0(
        "Both sf data and tribal information have been provided. ",
        "Please use only one of these query options."
      )
    )
  } 
  
  # Check for other arguments that indicate location. Function will ignore
  # these inputs but warn the user
  if( 
    # sf object provided
    (!is.null(aoi_sf) & inherits(aoi_sf, "sf")) & 
    # with additional location info
    any( !is.null(countrycode), !is.null(countycode), !is.null(huc),
         !is.null(siteid), !is.null(statecode) )
  ){
    warning(
      paste0(
        "Location information has been provided in addition to an sf object. ",
        "Only the sf object will be used in the query."
      )
    )
  } else if(
    # Tribe info provided
    !is.null(tribal_area_type) & 
    # with additional location info
    any( !is.null(countrycode), !is.null(countycode), !is.null(huc),
         !is.null(siteid), !is.null(statecode) )
  ){
    warning(
      paste0(
        "Location information has been provided in addition to tribal information. ",
        "Only the tribal information will be used in the query."
      )
    )
  }
  
  # Insufficient tribal info provided
  if( is.null(tribal_area_type) & !is.null(tribe_name_parcel) ){
    stop("A tribal_area_type is required if tribe_name_parcel is provided.")
  }
  
  
  # Set query parameters
  WQPquery <- list()
  
  
  # If an sf object OR tribal info are provided they will be the basis of the query
  # (The tribal data handling uses sf objects as well)
  if( (!is.null(aoi_sf) & inherits(aoi_sf, "sf")) | !is.null(tribal_area_type) ){
    
    sf::sf_use_s2(FALSE)
    
    # Build the non-sf part of the query:
    
    # StartDate
    if (length(startDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = list(startDate))
    } else if (!is.null(startDate)) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate, orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = startDate)
    }
    # SiteType
    if (length(siteType) > 1) {
      WQPquery <- c(WQPquery, siteType = list(siteType))
    } else if (!is.null(siteType)) {
      WQPquery <- c(WQPquery, siteType = siteType)
    }
    # CharacteristicName
    if (length(characteristicName) > 1) {
      WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
    } else if (!is.null(characteristicName)) {
      WQPquery <- c(WQPquery, characteristicName = characteristicName)
    }
    # CharacteristicType
    if (length(characteristicType) > 1) {
      WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
    } else if (!is.null(characteristicType)) {
      WQPquery <- c(WQPquery, characteristicType = characteristicType)
    }
    # SampleMedia
    if (length(sampleMedia) > 1) {
      WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia))
    } else if (!is.null(sampleMedia)) {
      WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
    }
    # Project
    if (length(project) > 1) {
      WQPquery <- c(WQPquery, project = list(project))
    } else if (!is.null(project)) {
      WQPquery <- c(WQPquery, project = project)
    }
    # Provider
    if (length(providers) > 1) {
      WQPquery <- c(WQPquery, providers = list(providers))
    } else if (!is.null(providers)) {
      WQPquery <- c(WQPquery, providers = providers)
    }
    # Organization
    if (length(organization) > 1) {
      WQPquery <- c(WQPquery, organization = list(organization))
    } else if (!is.null(organization)) {
      WQPquery <- c(WQPquery, organization = organization)
    }
    # EndDate
    if (length(endDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = list(endDate))
    } else if (!is.null(endDate)) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate, orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = endDate)
    }
    
    # sf AOI prep for query
    
    # If tribe info is provided then grab the corresponding sf object:
    if(!is.null(tribal_area_type)){
      
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
      
      # Keep to a single type:
      if(length(tribal_area_type) > 1){
        stop("tribal_area_type must be of length 1.")
      }
      
      # These area types allow filtering by TRIBE_NAME (unique within each type)
      if(tribal_area_type %in% c("Alaska Native Villages",
                                 "American Indian Reservations",
                                 "Off-reservation Trust Lands",
                                 "Oklahoma Tribal Statistical Areas",
                                 "Virginia Federally Recognized Tribes")
      ){
        
        # Two layers will not return any data when used for bboxes
        if(tribal_area_type == "Alaska Native Villages"){
          stop("Alaska Native Villages data are centroid points, not spatial boundaries.")
        } else if(tribal_area_type == "Virginia Federally Recognized Tribes") {
          stop("Federally recognized tribal entities in Virginia do not have any available spatial boundaries.")
        }
        
        # Get the relevant url
        aoi_sf <- filter(map_service_urls,
                         tribal_area == tribal_area_type)$url %>%
          # Pull data
          arcgislayers::arc_open() %>%
          # Return sf
          arcgislayers::arc_select() %>%
          # If a value provided, then filter
          {if ((tribe_name_parcel != "null") & (!is.null(tribe_name_parcel))) {
            filter(., TRIBE_NAME %in% tribe_name_parcel)
          } else {
            .
          }}
        
        # Otherwise filter by PARCEL_NO (Note that values in this col are not unique)
      } else if(tribal_area_type == "Alaska Native Allotments"){
        
        aoi_sf <- filter(map_service_urls,
                         tribal_area == tribal_area_type)$url %>%
          arcgislayers::arc_open() %>%
          arcgislayers::arc_select() %>%
          {if ((tribe_name_parcel != "null") & (!is.null(tribe_name_parcel))) {
            filter(., PARCEL_NO %in% tribe_name_parcel)
          } else {
            .
          }}
        
      } else {
        stop("Tribal area type not recognized. Refer to TADA_TribalOptions() for query options.")
      }
      
    }
    
    # Match CRS
    if(sf::st_crs(aoi_sf) != 4326){
      aoi_sf <- sf::st_transform(aoi_sf, crs = 4326)
    }
    
    # Get bbox of the sf object
    input_bbox <- sf::st_bbox(aoi_sf)
    
    # Query site info within the bbox
    bbox_sites <- dataRetrieval::whatWQPsites(
      WQPquery,
      bBox = c(input_bbox$xmin, input_bbox$ymin, input_bbox$xmax, input_bbox$ymax)
    )
    
    # Check if any sites are within the aoi
    if ( (nrow(bbox_sites) > 0 ) == FALSE) {
      stop("No monitoring sites were returned within your area of interest (no data available).")
    }
    
    # Reformat returned info as sf
    bbox_sites_sf <- EPATADA::TADA_MakeSpatial(bbox_sites, crs = 4326)
    
    # Subset sites to only within shapefile and get IDs
    clipped_sites_sf <- bbox_sites_sf[aoi_sf, ]
    
    clipped_site_ids <- clipped_sites_sf$MonitoringLocationIdentifier
    
    # Check number of sites returned. More than 300 will require a map() approach
    if( length(clipped_site_ids) > 300 ) {
      warning(
        paste0(
          "More than 300 sites are matched by the AOI and query terms. ",
          "If your AOI is a county, state, country, or HUC boundary it would be more efficient to provide a code instead of an sf object."
        )
      )
      
      # Split IDs into a list
      id_cluster_list <- split(x = clipped_site_ids,
                               f = ceiling(seq_along(clipped_site_ids) / 300))
      
      print("Downloading WQP query results. This may take some time depending upon the query size.")
      
      # List of query results
      results.DR <- purrr::map(
        .x = id_cluster_list,
        .f = ~suppressMessages(
          dataRetrieval::readWQPdata(
            siteid = .x,
            WQPquery,
            dataProfile = "resultPhysChem",
            ignore_attributes = TRUE
          )
        ) %>%
          # To allow row binds
          mutate(across(everything(), as.character))
      ) %>%
        list_rbind()
      
      # Check if any results were returned
      if ( (nrow(results.DR) > 0 ) == FALSE) {
        print(
          paste0(
            "Returning empty results dataframe: ",
            "Your WQP query returned no results (no data available). ",
            "Try a different query. ",
            "Removing some of your query filters OR broadening your search area may help."
          )
        )
        TADAprofile.clean <- results.DR
      } else {
        
        # Get site metadata
        sites.DR <- clipped_sites_sf %>%
          as_tibble() %>%
          select(-geometry)
        
        # Get project metadata
        projects.DR <- dataRetrieval::readWQPdata(
          siteid = clipped_site_ids,
          WQPquery,
          ignore_attributes = TRUE,
          service = "Project"
        )
        
        # Join results, sites, projects
        TADAprofile <- TADA_JoinWQPProfiles(
          FullPhysChem = results.DR,
          Sites = sites.DR,
          Projects = projects.DR
        )
        
        # need to specify this or throws error when trying to bind rows.
        # Temporary fix for larger issue where data structure for all columns
        # should be specified.
        TADAprofile <- TADAprofile %>% dplyr::mutate(
          across(everything(), as.character)
        )
        
        # run TADA_AutoClean function
        if (applyautoclean == TRUE) {
          print("Data successfully downloaded. Running TADA_AutoClean function.")
          
          TADAprofile.clean <- TADA_AutoClean(TADAprofile)
        } else {
          TADAprofile.clean <- TADAprofile
        }
      }
      
      return(TADAprofile.clean)
      
      # Less than 300 sites:
    } else {
      
      # Retrieve all 3 profiles
      print("Downloading WQP query results. This may take some time depending upon the query size.")
      print(WQPquery)
      
      # Get results
      results.DR <- dataRetrieval::readWQPdata(
        siteid = clipped_site_ids,
        WQPquery,
        dataProfile = "resultPhysChem",
        ignore_attributes = TRUE
      )
      
      # check if any results were returned
      if ((nrow(results.DR) > 0) == FALSE) {
        paste0(
          "Returning empty results dataframe: ",
          "Your WQP query returned no results (no data available). ",
          "Try a different query. ",
          "Removing some of your query filters OR broadening your search area may help."
        )
        TADAprofile.clean <- results.DR
      } else {
        
        # Get site metadata
        sites.DR <- dataRetrieval::whatWQPsites(WQPquery)
        
        # Get project metadata
        projects.DR <- dataRetrieval::readWQPdata(WQPquery,
                                                  ignore_attributes = TRUE,
                                                  service = "Project")
        
        # Join results, sites, projects
        TADAprofile <- TADA_JoinWQPProfiles(
          FullPhysChem = results.DR,
          Sites = sites.DR,
          Projects = projects.DR
        )
        
        # need to specify this or throws error when trying to bind rows.
        # Temporary fix for larger issue where data structure for all columns
        # should be specified.
        TADAprofile <- TADAprofile %>% dplyr::mutate(
          across(everything(), as.character)
        )
        
        # run TADA_AutoClean function
        if (applyautoclean == TRUE) {
          print("Data successfully downloaded. Running TADA_AutoClean function.")
          
          TADAprofile.clean <- TADA_AutoClean(TADAprofile)
        } else {
          TADAprofile.clean <- TADAprofile
        }
      }
      
      return(TADAprofile.clean)
      
    }
    
    # If no sf object provided:
  } else {
    
    if (!is.null(statecode)) {
      load(system.file("extdata", "statecodes_df.Rdata", package = "EPATADA"))
      statecode <- as.character(statecode)
      statecodes_sub <- statecodes_df %>% dplyr::filter(STUSAB %in% statecode)
      statecd <- paste0("US:", statecodes_sub$STATE)
      if (nrow(statecodes_sub) == 0) {
        stop("State code is not valid. Check FIPS state/territory abbreviations.")
      }
      if (length(statecode) >= 1) {
        WQPquery <- c(WQPquery, statecode = list(statecd))
      }
    }
    
    if (length(huc) > 1) {
      WQPquery <- c(WQPquery, huc = list(huc))
    } else if (!is.null(huc)) {
      WQPquery <- c(WQPquery, huc = huc)
    }
    
    if (length(startDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = list(startDate))
    } else if (!is.null(startDate)) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate, orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = startDate)
    }
    
    if (length(countrycode) > 1) {
      WQPquery <- c(WQPquery, countrycode = list(countrycode))
    } else if (!is.null(countrycode)) {
      WQPquery <- c(WQPquery, countrycode = countrycode)
    }
    
    if (length(countycode) > 1) {
      WQPquery <- c(WQPquery, countycode = list(countycode))
    } else if (!is.null(countycode)) {
      WQPquery <- c(WQPquery, countycode = countycode)
    }
    
    if (length(siteid) > 1) {
      WQPquery <- c(WQPquery, siteid = list(siteid))
    } else if (!is.null(siteid)) {
      WQPquery <- c(WQPquery, siteid = siteid)
    }
    
    if (length(siteType) > 1) {
      WQPquery <- c(WQPquery, siteType = list(siteType))
    } else if (!is.null(siteType)) {
      WQPquery <- c(WQPquery, siteType = siteType)
    }
    
    if (length(characteristicName) > 1) {
      WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
    } else if (!is.null(characteristicName)) {
      WQPquery <- c(WQPquery, characteristicName = characteristicName)
    }
    
    if (length(characteristicType) > 1) {
      WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
    } else if (!is.null(characteristicType)) {
      WQPquery <- c(WQPquery, characteristicType = characteristicType)
    }
    
    if (length(sampleMedia) > 1) {
      WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia))
    } else if (!is.null(sampleMedia)) {
      WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
    }
    
    if (length(project) > 1) {
      WQPquery <- c(WQPquery, project = list(project))
    } else if (!is.null(project)) {
      WQPquery <- c(WQPquery, project = project)
    }
    
    if (length(providers) > 1) {
      WQPquery <- c(WQPquery, providers = list(providers))
    } else if (!is.null(providers)) {
      WQPquery <- c(WQPquery, providers = providers)
    }
    
    if (length(organization) > 1) {
      WQPquery <- c(WQPquery, organization = list(organization))
    } else if (!is.null(organization)) {
      WQPquery <- c(WQPquery, organization = organization)
    }
    
    if (length(endDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = list(endDate))
    } else if (!is.null(endDate)) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate, orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = endDate)
    }
    
    # Retrieve all 3 profiles
    print("Downloading WQP query results. This may take some time depending upon the query size.")
    print(WQPquery)
    results.DR <- dataRetrieval::readWQPdata(WQPquery,
                                             dataProfile = "resultPhysChem",
                                             ignore_attributes = TRUE
    )
    # check if any results are available
    if ((nrow(results.DR) > 0) == FALSE) {
      print(
        paste0(
          "Returning empty results dataframe: ",
          "Your WQP query returned no results (no data available). ",
          "Try a different query. ",
          "Removing some of your query filters OR broadening your search area may help."
        )
      )
      # Get results
      TADAprofile.clean <- results.DR
    } else {
      
      # Get site metadata
      sites.DR <- dataRetrieval::whatWQPsites(WQPquery)
      
      # Get project metadata
      projects.DR <- dataRetrieval::readWQPdata(WQPquery,
                                                ignore_attributes = TRUE,
                                                service = "Project"
      )
      
      # Join results, sites, projects
      TADAprofile <- TADA_JoinWQPProfiles(
        FullPhysChem = results.DR,
        Sites = sites.DR,
        Projects = projects.DR
      )
      
      # need to specify this or throws error when trying to bind rows. Temporary fix for larger
      # issue where data structure for all columns should be specified.
      TADAprofile <- TADAprofile %>% dplyr::mutate(
        across(everything(), as.character)
      )
      
      # run TADA_AutoClean function
      if (applyautoclean == TRUE) {
        print("Data successfully downloaded. Running TADA_AutoClean function.")
        
        TADAprofile.clean <- TADA_AutoClean(TADAprofile)
      } else {
        TADAprofile.clean <- TADAprofile
      }
    }
    
    return(TADAprofile.clean)
    
  }
  
  
}
