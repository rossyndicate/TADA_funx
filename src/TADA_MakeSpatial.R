#' Transform a Water Quality Portal dataframe into a geospatial {sf} object.
#' 
#' @param data A dataframe created by `TADA_DataRetrieval()`.
#' @param crs The coordinate reference system (CRS) you would like the returned point features to be in. The default is CRS 4326 (WGS84).
#' 
#' @return The original TADA Water Quality Portal dataframe but as geospatial {sf} point objects. 
#' 
#' @seealso [TADA_DataRetrieval()]
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' tada_not_spatial <- TADA_DataRetrieval(startDate = "1995-01-01",
#'                                        endDate = "1995-12-31",
#'                                        characteristicName = "pH",
#'                                        statecode = "SC", 
#'                                        countycode = "Abbeville",
#'                                        applyautoclean = TRUE)
#' 
#' # make `tada_not_spatial` an {sf} object, projected in crs = 4269 (NAD83)                                 
#' tada_spatial <- TADA_MakeSpatial(data = tada_not_spatial, crs = 4269)
#' }


TADA_MakeSpatial <- function(data, crs = 4326){
  
  # must have all columns needed to make spatial
  if (!"LongitudeMeasure" %in% colnames(data) & !"LatitudeMeasure" %in% colnames(data)) {
    stop("The dataframe does not contain WQP-style latitude and longitude data (column names `HorizontalCoordinateReferenceSystemDatumName`, `LatitudeMeasure` and LongitudeMeasure` or `TADA.LongitudeMeasure` and `TADA.LatitudeMeasures`.")
  } else if (!is.null(data) & inherits(data, "sf")) { stop("Your data is already a spatial object.")
  }
  
  # Make a reference table for CRS and EPSG codes
  epsg_codes <- tribble(
    ~ HorizontalCoordinateReferenceSystemDatumName, ~ epsg,
    "NAD83", 4269,
    "WGS84", 4326,
    "NAD27", 4267,
    # For now assume these are WGS84. USGS has done this, too. 
    "UNKWN", 4326,
    "OTHER", 4326,
    "OLDHI", 4135
  )
  
  suppressMessages(suppressWarnings({
    # join our CRS reference table to our original WQP dataframe:
    sf <- data %>%
      mutate(lat = as.numeric(LatitudeMeasure),
             lon = as.numeric(LongitudeMeasure)) %>%
      # Add EPSG codes
      dplyr::left_join(x = .,
                       y = epsg_codes,
                       by = "HorizontalCoordinateReferenceSystemDatumName") %>%
      # Group by CRS:
      split(f = .$HorizontalCoordinateReferenceSystemDatumName) %>%
      # Transform and re-stack:
      purrr::map_df(.x = .,
                    .f = ~ .x %>%
                      sf::st_as_sf(coords = c("lon", "lat"),
                                   crs = unique(.x$epsg)) %>%
                      # transform to the selected CRS:
                      sf::st_transform(sf::st_crs(as.numeric(crs)))) %>%
      dplyr::select(-epsg)
  }))
  
  return(sf)
  
}

