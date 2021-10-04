#' Create an rs_election object with voronoi polygons for polling booths
#'
#' @param name The name of the election
#' @param boundaries An rs_boundary_set with the electorate boundaries
#' @param polling_places An rs_polling_places object with each polling place used
#' @param election_result_list An list that contains one or more rs_election_results
#' @param crs The CRS that the voronoi polygons will be made in. Typically a projected CRS for the relevant area.
#'
#' @return An rs_election object
#' @export
rs_election <- function(name,boundaries,polling_places,election_result_list,crs){
  if (!is.character(name)){
    stop("'name' must be of type 'character'")
  }
  if (!inherits(boundaries,"rs_boundary_set")){
    stop("'boundaries' must be of type 'rs_boundary_set'")
  }
  if (!inherits(polling_places,"rs_polling_places")){
    stop("'polling_places' must be of type 'rs_polling_places'")
  }
  if (!inherits(election_result_list,"list")){
    stop("'election_result_list' must be of type 'list'")
  }
  else if (!all(sapply(election_result_list,function(x){"rs_election_results" %in% class(x)},simplify = TRUE))) {
    stop("'election_result_list' must only contain objects of type 'rs_election_results'")
  }
  else if (any(duplicated(sapply(election_result_list,function(x){x$name})))){
    stop("Every 'rs_election_results' in 'election_result_list' must have a unique name")
  }
  boundary_dist_names <- unique(boundaries$district)
  polling_place_dist_names <- unique(polling_places$district)
  if (length(setdiff(boundary_dist_names,polling_place_dist_names)) > 0){
    stop(paste0("Some district names appear in one set but not the other\n",
                   "District names in boundary set but not in polling place list:",
                   paste(setdiff(boundary_dist_names,polling_place_dist_names), collapse = ", "),"\n",
                   "District names in polling place list but not in boundary set:",
                   paste(setdiff(polling_place_dist_names,boundary_dist_names), collapse = ", "),"\n",
                "Each district name in the boundary set must have at least 1 polling place attached."))
  }
  if (length(setdiff(polling_place_dist_names,boundary_dist_names)) > 0){
    warning(paste0("Some district names appear in one set but not the other\n",
                   "District names in boundary set but not in polling place list:",
                   paste(setdiff(boundary_dist_names,polling_place_dist_names), collapse = ", "),"\n",
                   "District names in polling place list but not in boundary set:",
                   paste(setdiff(polling_place_dist_names,boundary_dist_names), collapse = ", ")))
  }

  adj_results <- lapply(election_result_list,rs_adjustment, rs_polling_places = polling_places)

  sf_polling_places <- sapply(boundary_dist_names, function(x,sfc,polling_places){
    district_polling_places <- dplyr::filter(polling_places,!!as.name("is_coord") == TRUE,!!as.name("district") == x) %>% dplyr::select(-c("district"))
    boundary <- dplyr::filter(sfc,!!as.name("district") == x)
    return(list(boundary = boundary,polling_places = district_polling_places))
  }, sfc = boundaries, polling_places = polling_places, simplify = FALSE, USE.NAMES = TRUE)

  voronoi <- suppressWarnings(dplyr::bind_rows(lapply(sf_polling_places,rs_voronoi,crs = crs)))

  missing_pp_id <- setdiff(unique(dplyr::filter(polling_places,!!as.name("is_coord") == TRUE)$pp_id),unique(voronoi$pp_id))

  if (length(missing_pp_id) != 0){
    warning("Polling places with id numbers: ",paste(missing_pp_id, collapse = ", ")," have been clipped from voronoi. Consider merging the results from these booths with nearby ones.\nUse ..$missing_pp_id_report for more information")
  }

  missing_pp_id_report <- left_join(data.frame(pp_id = missing_pp_id),polling_places, by = "pp_id")

  election <- list(name = name,boundaries = boundaries,polling_places = polling_places, voronoi = voronoi, adj_results = adj_results, missing_pp_id_report = missing_pp_id_report)
  class(election) <- c("rs_election",class(election))
  election
}

#' Generic to print text summary of the rs_election object
#'
#' @param x rs_election
#' @param ... Additional arguments
#' @method print rs_election
#' @return A text summary of the rs_election object - Returns rs_election invisibly
#' @export
print.rs_election <- function(x,...){
  cat("Election Name:", x$name,"\n\n")
  cat("Attached district map contains",nrow(x$boundaries),"districts\n")
  cat("    Names:",paste(utils::head(x$boundaries$district,n=10),collapse = ", "),"\n\n")
  cat("Attached list of", nrow(x$polling_places), "polling places - With",sum(x$polling_places$is_coord),"polling places with coordinates\n")
  cat("    Voronoi diagram contains",nrow(x$voronoi),"polling places\n\n")
  cat(length(x$adj_results),"results attached:",paste(sapply(x$adj_results,function(y){y$name}), collapse = ", "),"\n")
  invisible(x)
}

#' Creates voronoi for polling booths by electorate and cuts each of them by the electorate
#'
#' @param x A list of 2 Spatial Polygons Data Frame(s). The first contains the electorate outline, the second contains the locations of the polling places.
#' @param crs The CRS that the voronoi polygons will be made in. Typically a projected CRS for the relevant area.
#'
#' @return A list with a voronoi diagram for each electorate
rs_voronoi <- function(x,crs){
  if (is.null(attr(x$polling_places,"crs"))){
    stop("The crs attribute of the rs_polling_places object is missing. Please report this bug.")
  }
  polling_place_sfc <- sf::st_as_sf(x$polling_places, coords = c("longitude","latitude"), crs = attr(x$polling_places,"crs"))
  polling_place_sfc <- sf::st_transform(polling_place_sfc,crs)
  transformed_coordinates <- sf::st_coordinates(polling_place_sfc)
  polling_place_sfc <- sf::st_drop_geometry(polling_place_sfc) %>%
    cbind(transformed_coordinates)
  boundary_sp <- x$boundary %>% sf::as_Spatial()
  sf::st_as_sf(ggvoronoi::voronoi_polygon(polling_place_sfc,x = "X", y = "Y", outline = boundary_sp))
}

#' Adjust for votes not cast at an ordinary polling place
#'
#' @param rs_election_results An rs_election_results object
#' @param rs_polling_places An rs_polling_places object
#'
#' @return An rs_adjustment object
rs_adjustment <- function(rs_election_results, rs_polling_places){
  # Adjusts for votes that cannot be tied to a polling place with coordinates
  if (!inherits(rs_election_results,"rs_election_results")){
    stop("'rs_election_results' must be of type 'rs_election_results'")
  }
  if (!inherits(rs_polling_places,"rs_polling_places")){
    stop("'rs_polling_places' must be of type 'rs_polling_places'")
  }
  df <- dplyr::left_join(rs_election_results$results, dplyr::select(rs_polling_places, "pp_id", "district", "is_coord"), by="pp_id") %>%
    dplyr::group_by(!!as.name("group_code"),!!as.name("district"),!!as.name("is_coord")) %>%
    dplyr::mutate(district_pty_type_votes = sum(!!as.name("votes"))) %>%
    dplyr::ungroup(!!as.name("is_coord")) %>%
    dplyr::mutate(district_pty_votes = sum(!!as.name("votes"))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(adj_ratio = !!as.name("district_pty_votes")/!!as.name("district_pty_type_votes"), adj_votes = !!as.name("votes")*!!as.name("adj_ratio")) %>%
    dplyr::filter(!!as.name("is_coord") == TRUE) %>%
    dplyr::select("pp_id","group_code","votes","adj_votes") %>%
    dplyr::group_by(!!as.name("pp_id")) %>%
    dplyr::mutate(vote_pct = !!as.name("votes")/sum(!!as.name("votes")), adj_vote_pct = !!as.name("adj_votes")/sum(!!as.name("adj_votes"))) %>%
    dplyr::ungroup()
  rs_adjustment <- list(name = rs_election_results$name, results = df)
  class(rs_adjustment) <- c("rs_adjustment",class(rs_adjustment))
  rs_adjustment
}

#' Summaries of each of the results in an rs_election object
#'
#' @param object An rs_election object
#' @param ... Additional arguments
#'
#' @return A list of the summarized results, with each list item referring to a different result. The results themselves are summarized by district.
#' @export
summary.rs_election <- function(object, ...){
  transfer_proportions <- dplyr::select(object$polling_places,"pp_id","district")
  lapply(object$adj_results, function (x){
    x$results <- dplyr::left_join(x$results,transfer_proportions, by = "pp_id") %>%
      dplyr::group_by(!!as.name("group_code"),!!as.name("district"))  %>%
      dplyr::summarise(votes = sum(!!as.name("adj_votes")), .groups = "drop")
    x
  })
}

