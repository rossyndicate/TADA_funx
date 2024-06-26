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
#' @param countycode FIPS county name. Note that a state code must also be supplied (e.g. statecode = "AL", countycode = "Chilton").
#' @param huc A numeric code denoting a hydrologic unit. Example: "04030202". Different size hucs can be entered.
#' @param siteid Unique monitoring location identifier
#' @param sf A polygon to subset the WQP data by
#' @param siteType Type of waterbody
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
#'                                           sf = sf_example)
#' 
#' 
#' 
#' 
#'
TADA_DataRetrieval_new <- function(startDate = NULL,
                                   endDate = NULL,
                                   countycode = NULL,
                                   huc = NULL,
                                   sf = NULL,
                                   siteid = NULL,
                                   siteType = NULL,
                                   characteristicName = NULL,
                                   characteristicType = NULL,
                                   sampleMedia = NULL,
                                   statecode = NULL,
                                   organization = NULL,
                                   project = NULL,
                                   providers = NULL,
                                   applyautoclean = TRUE) {
  
  # Ensure sf feature aligns with state/county/huc inputs, if any of these
  # are provided by user
  if (!is.null(sf) & inherits(sf, "sf")) {
    
    suppressWarnings(suppressMessages({
      sf::sf_use_s2(FALSE)
    }))
    
    # State inputs
    if(!is.null(statecode)){
      # Get state bounds
      suppressWarnings(suppressMessages({
        sf_states <- sf::st_transform(tigris::states(), sf::st_crs(sf)) %>%
          .[sf,]
      }))
      
      if (!is.null(statecode) & statecode %in% sf_states$STUSPS != TRUE) {
        stop("Your shapefile does not overlap your state(s) of interest.")
      }
      
      rm(sf_states)
      gc()
      
    }
    
    # County inputs
    if(!is.null(countycode)){
      # Get county bounds
      suppressWarnings(suppressMessages({
        sf_counties <- sf::st_transform(tigris::counties(), sf::st_crs(sf)) %>%
          .[sf,]
      }))
      
      if (!is.null(countycode) & countycode %in% sf_counties$NAME != TRUE) {
        stop("Your shapefile does not overlap your county (or counties) of interest.")
      }
      
      rm(sf_counties)
      gc()
      
    }
    
    # HUC inputs
    if(!is.null(huc)){
      # Get HUCs
      suppressWarnings(suppressMessages({
        sf_hucs <- sf %>%
          dplyr::summarize() %>%
          nhdplusTools::get_huc(AOI = ., type = "huc12") %>%
          dplyr::mutate(huc10 = substr(huc12, 1, 10),
                        huc8 = substr(huc12, 1, 8),
                        huc6 = substr(huc12, 1, 6),
                        huc4 = substr(huc12, 1, 4),
                        huc2 = substr(huc12, 1, 2))
      }))
      
      hucs <- c(sf_hucs$huc2, sf_hucs$huc4, sf_hucs$huc6, sf_hucs$huc8,
                sf_hucs$huc10, sf_hucs$huc12)
      
      if (!is.null(huc) & huc %in% hucs != TRUE) {
        stop("Your shapefile does not overlap your HUC(s) of interest.")
      }
      
      rm(hucs, sf_hucs)
      gc()
      
    }
    
    
    
    
    
    
    # fill in HUC argument to speed up pull if no HUC argument selected by user:
    if (is.null(huc)) {
      huc <- (sf_hucs$huc10)
    }
  }
  
  # Set query parameters
  WQPquery <- list()
  if (length(statecode) > 1) {
    WQPquery <- c(WQPquery, statecode = list(statecode))
  } else if (!is.null(statecode)) {
    WQPquery <- c(WQPquery, statecode = statecode)
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
  results.DR <- dataRetrieval::readWQPdata(WQPquery,
                                           dataProfile = "resultPhysChem",
                                           ignore_attributes = TRUE
  )
  # check if any results are available
  if ((nrow(results.DR) > 0) == FALSE) {
    print("Returning empty results dataframe: Your WQP query returned no results (no data available). Try a different query. Removing some of your query filters OR broadening your search area may help.")
    TADAprofile.clean <- results.DR
  } else {
    sites.DR <- dataRetrieval::whatWQPsites(WQPquery)
    
    projects.DR <- dataRetrieval::readWQPdata(WQPquery,
                                              ignore_attributes = TRUE,
                                              service = "Project"
    )
    
    TADAprofile <- TADA_JoinWQPProfiles(
      FullPhysChem = results.DR,
      Sites = sites.DR,
      Projects = projects.DR
    )
    
    # need to specify this or throws error when trying to bind rows. Temporary fix for larger
    # issue where data structure for all columns should be specified.
    cols <- names(TADAprofile)
    
    TADAprofile <- TADAprofile %>% dplyr::mutate_at(cols, as.character)
    
    # run TADA_AutoClean function
    if (applyautoclean == TRUE) {
      print("Data successfully downloaded. Running TADA_AutoClean function.")
      
      TADAprofile.clean <- TADA_AutoClean(TADAprofile)
    } else {
      TADAprofile.clean <- TADAprofile
    }
  }
  
  # subset original dataframe to only those WQP observations within the user-supplied sf:
  if (!is.null(sf) & inherits(sf, "sf")) {
    try(TADAprofile.clean <- TADAprofile.clean %>%
          # transform df into spatial object using a new TADA_MakeSpatial() function
          TADA_MakeSpatial(data = ., crs = 4326) %>%
          # transform to the CRS of the sf object
          sf::st_transform(sf::st_crs(sf)) %>%
          # select only WQP observations within the sf object
          .[sf, ] %>%
          sf::st_drop_geometry(),
        silent = TRUE)
  }
  
  return(TADAprofile.clean)
}