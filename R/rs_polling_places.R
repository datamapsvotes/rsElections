#' Creates an rs_polling_places object
#'
#' @param df A data frame of polling places
#' @param pp_id The column name of the polling place identifier
#' @param dist_names The column name of the district names
#' @param longitude The column name of the longitude
#' @param latitude The column name of the latitude
#' @param crs The CRS of the coordinates (Defaults to 4326)
#'
#' @return An rs_polling_places object
#' @export
rs_polling_places <- function(df,pp_id,dist_names,longitude = "longitude",latitude = "latitude",crs=4326){
  protected_column_names <- c("pp_id","district","longitude","latitude","is_coord")
  if (!inherits(df,"data.frame")){
    stop("'df' must be a Data Frame")
  }
  protected_column_names_used <- any(names(dplyr::select(df,!c(pp_id,dist_names,longitude,latitude))) %in% protected_column_names)
  if(protected_column_names_used){
    stop("Additional column names must not be any of \"", paste(protected_column_names, collapse = "\" \""),"\"")
  }
  df <- dplyr::rename(df,"pp_id" = !!as.name(pp_id),"district" = !!as.name(dist_names),"longitude" = !!as.name(longitude),"latitude" = !!as.name(latitude)) %>%
    dplyr::mutate(is_coord = !is.na(longitude)&!is.na(latitude)) %>%
    dplyr::relocate(pp_id,as.name("district"),longitude,latitude,as.name("is_coord")) %>%
    dplyr::as_tibble()

  locations_count_df <- dplyr::select(df, "district", "longitude", "latitude") %>%
    dplyr::filter(!is.na(!!as.name("longitude")) & !is.na(!!as.name("latitude"))) %>%
    dplyr::group_by_all() %>%
    dplyr::summarise(n = n())
  if (max(locations_count_df$n)>1){
    stop("A location can only be entered once per district. That means that there is at least one district with the same polling booth entered twice.\n",
         "Try removing the longitude and latitude for special booths such as ones used for early voting.")
  }

  attr(df, "crs") <- crs
  class(df) <- c("rs_polling_places",class(df))
  df
}
