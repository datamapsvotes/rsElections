#' Creates an rs_election_results object
#'
#' @param name The name of the result (Primary Vote, Two Party Preferred, Two Candidate Preferred etc.)
#' @param df The data frame that lists the results by polling booth
#' @param pp_id The column containing the polling place identifier
#' @param group_code The column containing the variable to split the results by (Typically, this refers to political parties or party codes)
#' @param votes The column containing the votes that each group recieved at each polling place
#'
#' @return An rs_election_results object
#' @export
#'
rs_election_results <- function(name, df, pp_id, group_code, votes){
  if (!inherits(df,"data.frame")){
    stop("'df' must be a Data Frame")
  }
  if (!all(sapply(list(name,pp_id,group_code,votes), inherits, what = "character"))){
    stop("'name', 'pp_id', 'group_code', 'votes' must all be of type character")
  }
  if (!is.numeric(df[[votes]])){
    stop("'votes' column must be of type numeric\nCheck to see if commas are used as a thousands separator in the 'votes' column")
  }
  df <- dplyr::select(df,c(pp_id,group_code,votes)) %>%
    dplyr::rename("pp_id" = pp_id, "group_code" = group_code, "votes" = votes)
  rs_election_results <- list(name = name, results = df)
  class(rs_election_results) <- c("rs_election_results",class(rs_election_results))
  rs_election_results
}
