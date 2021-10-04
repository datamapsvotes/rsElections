#' Adds colors based on the imported color palette
#'
#' @param election An rs_election or rs_redistribution object
#' @param result_name The name of the re_election_results object in the election parameter
#' @param polcolpal Political color palette created using rs_polcolpal_create
#' @param adjustments Whether to calculate the color based on the on-the-day vote or after adjustments are made
#' @param mode Either "plurality" or "single color". "Plurality" gives the largest vote at the booth, "single color" plots the vote share of the group specified in single_group_code
#' @param single_group_code If mode == "single color", the group_code to color with
#'
#' @return An rs_election_as_sf object with a col_code column
#' @export
rs_election_as_sf <- function(election, result_name, polcolpal, adjustments=TRUE, mode = "plurality", single_group_code = NA){
  if (!(mode %in% c("plurality","single group"))){
    stop("'mode' must be one of \"plurality\" or \"single group\"")
  }
  result_column <- ifelse(adjustments,"adj_vote_pct","vote_pct")
  result <- election$adj_results[match(result_name,sapply(election$adj_results,function(x){x$name}))][[1]]$results %>%
    dplyr::group_by(!!as.name("pp_id"))
  if (mode == "plurality"){
    if (!is.na(single_group_code)){
      stop("'single_group_code' only works if 'mode' = \"single group\"\nTry setting 'mode' = \"single group\" or removing 'single_group_code' as an argument")
    }
    result_to_join <- dplyr::slice_max(result, order_by = !!as.name(result_column), n=1,with_ties = FALSE)
  }
  if (mode == "single group"){
    result_to_join <- dplyr::filter(result, !!as.name("group_code") == single_group_code) %>%
      dplyr::slice_max(order_by = !!as.name(result_column), n=1,with_ties = FALSE)
  }
  if (nrow(result_to_join) == 0){
    stop("There are no results to add to the map.\nOne possible cause is if 'single_group_code' doesn't refer to a group code in the result.")
  }
  if (!all(unique(result_to_join$group_code) %in% unique(polcolpal$values$pty))){
    stop("'polcolpal' must have a colour for each group code in the result")
  }
  rs_as_sf <- dplyr::left_join(election$voronoi,result_to_join, by = "pp_id") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(col_code = polcolpal$choose_color(!!as.name("group_code"), vote_pct = !!as.name(result_column))) %>%
    dplyr::ungroup()
  class(rs_as_sf) <- c("rs_election_as_sf",class(rs_as_sf))
  attr(rs_as_sf, "palette") <- polcolpal
  attr(rs_as_sf, "result_name") <- result_name
  rs_as_sf
}
