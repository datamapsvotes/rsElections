#' Creates an rs_redistribution object
#'
#' @param election An rs_election object
#' @param new_boundary_set An rs_boundary_set object - representing the new boundaries
#' @param slivers_rm If true, any rs_boundary_set element containing less than 2\% of another polling place will have that polling place removed from the calculations for the redistribution.
#'
#' @return An rs_redistribution object
#' @export
rs_redistribution <- function(election, new_boundary_set, slivers_rm = TRUE){
  new_boundary_set <- dplyr::rename(new_boundary_set, "to_district" = !!as.name("district"))
  voronoi <-  dplyr::rename(election$voronoi, "from_district" = !!as.name("district"))
  voronoi$initial_area <- sf::st_area(voronoi)
  intersection <- sf::st_intersection(voronoi,new_boundary_set)
  intersection$new_area <- sf::st_area(intersection)
  intersection <-  dplyr::mutate(intersection,new_area_pct = as.double(!!as.name("new_area")/!!as.name("initial_area")))
  if (slivers_rm == TRUE){
    intersection <-  dplyr::filter(intersection,!!as.name("new_area_pct") > 0.02)
  }
  intersection <-  dplyr::group_by(intersection,!!as.name("pp_id")) %>%
    dplyr::mutate(adj_new_area_pct = !!as.name("new_area_pct")/(sum(!!as.name("new_area_pct")))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("initial_area","new_area","new_area_pct"))

  rs_redistribution <- list(voronoi = intersection,adj_results = election$adj_results)
  class(rs_redistribution) <- c("rs_redistribution",class(rs_redistribution))
  rs_redistribution
}

#' Generic that summarizes an rs_redistribution
#'
#' @param object An rs_redistribution object
#' @param ... Additional arguments
#'
#' @return A list of summarized results for the new boundaries
#' @export
summary.rs_redistribution <- function(object, ...){
  transfer_proportions <- dplyr::select(sf::st_drop_geometry(object$voronoi),"pp_id","to_district","adj_new_area_pct")
  lapply(object$adj_results, function (x){
    x$results <- dplyr::left_join(x$results,transfer_proportions, by = "pp_id") %>%
      dplyr::group_by(!!as.name("group_code"),!!as.name("to_district"))  %>%
      dplyr::summarise(votes = sum(!!as.name("adj_votes")*!!as.name("adj_new_area_pct")), .groups = "drop")
    x
  })
}
