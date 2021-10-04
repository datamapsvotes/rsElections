#' Create an rs_boundary_set
#'
#' @param sfc Simple feature collection
#' @param names Name of the column containing electorate/district names
#' @param to_crs A CRS to convert the sfc to for the purposes of st_union()
#'
#' @return A simple feature collection with two columns, district and geometry
#' @export
rs_boundary_set <- function(sfc, names, to_crs=NULL){
  if (!inherits(sfc,"sf")){
    stop("'sfc' must be a Simple Feature. These can be generated using the 'sf' package.")
  }
  if (!is.character(sfc[[names]])){
    stop("'names' must be a column of 'sfc' of type 'character'")
  }
  if (!is.null(to_crs)){
    if (!is.numeric(to_crs)){
      stop("'to_crs' must be of type 'numeric'")
    }
    sfc <- sf::st_transform(sfc,to_crs)
  }
  sfc <- dplyr::rename(sfc,"district" = !!as.name(names)) %>%
    dplyr::group_by(!!as.name("district")) %>%
    dplyr::summarise(.groups = "drop") %>%
    sf::st_zm()
  if (sf::st_is_longlat(sfc)){
    warning("Ensure that 'sfc' uses a projected CRS. If you're receiving warnings, use 'to_crs' to convert the shapefile into an appropriate projected CRS for the data.")
  }
  class(sfc) <- c("rs_boundary_set",class(sfc))
  sfc
}
