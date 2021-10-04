#' Creates a color palette for each group code
#'
#' @param df A data frame with two columns. The first lists each group code, the second lists the base color for each group code.
#' @param breaks A vector of 10 numbers in ascending order that specifies when the strength of the color should change on the map. Anything below the lowest break will be colored white.
#'
#' @return An rs_polcolpal object
#' @export
rs_polcolpal_create <- function(df, breaks){
  input_length <- nrow(df)
  if (length(breaks) != 10){
    stop("'breaks' must be of length 10")
  }
  party_palette <- function(color){
    # Code with modification from https://hihayk.github.io/scale/
    # Made using Javascript, with two installed npm packages - Browserify and Color
    engine <- V8::v8(global = "window")
    engine$source(system.file("javascript", "out.js", package = "rsElections"))
    engine$eval("function getColorsList(colorsAmount, colorsShiftAmount, mixColor, rotate, saturation, mainColor)  {
    const colorsList = []
     let step
     for (step = 0; step < colorsAmount; step++) {
         colorsList
          .push(color(mainColor).rotate((step + 1) / colorsAmount * -rotate)
          .saturate((step + 1) / colorsAmount * (saturation / 100))
          .mix(color(mixColor), (colorsShiftAmount / 100) * (step + 1) / colorsAmount)
          .hex())
      }
     return colorsList
    }")
    color_JS_String <- paste0("'",color,"'")
    darker_Col <- {engine$eval(paste0("getColorsList(4, 64, 'black', 0, 64, ",color_JS_String,")")) %>%
      stringr::str_split(",")}[[1]]
    lighter_Col <- {engine$eval(paste0("getColorsList(5, 80, 'white', 0, 80, ",color_JS_String,")")) %>%
      stringr::str_split(",")}[[1]] %>%
      rev()
    return(c(lighter_Col,color,darker_Col))
  }

  df2 <- data.frame(colour_code = -2:(input_length*10-1),
                    pty=c(NA,NA,rep(df[[1]],each=10)),
                    strength_code = c(NA,NA,rep(0:9,input_length)),
                    colour = c("#FFFFFF","#FFFFFF",unlist(lapply(df[[2]],party_palette))))

  choose_color <- function(pty_code,vote_pct,type = "colour_code"){
    if (any(is.na(c(pty_code,vote_pct)))|is.nan(vote_pct)){
      row_to_pick <- dplyr::filter(df2, !!as.name("colour_code") == -2)
    }
    else {
      strength <- findInterval(vote_pct, breaks, left.open = TRUE) - 1
      if (strength == -1){
        row_to_pick <- dplyr::filter(df2, !!as.name("colour_code") == -1)
      }
      else {
        row_to_pick <- dplyr::filter(df2, !!as.name("pty") == pty_code, !!as.name("strength_code") == strength)
      }
    }
    row_to_pick[[type]]
  }
  ggplot_scale <- as.list(df2$colour)
  names(ggplot_scale) <- df2$colour_code
  legend <- function(square_size = 20, pty_codes){
    filtered_df <- dplyr::filter(df2, !!as.name("colour_code") >= 0, !!as.name("pty") %in% pty_codes)
    plot <- ggplot2::ggplot(filtered_df) +
      ggplot2::geom_point(ggplot2::aes(fill=as.factor(!!as.name("colour_code")),x=!!as.name("strength_code"),y=!!as.name("pty")),shape=22,size=square_size,color="white")+
      ggplot2::scale_fill_manual(values=filtered_df$colour)+
      ggplot2::scale_x_continuous(breaks = seq(0.5,8.5,1), labels = breaks[-1]*100) +
      ggplot2::coord_fixed(ratio = 1) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            legend.position = "none",
            axis.text.x = ggplot2::element_text(vjust = 3),
            plot.background = ggplot2::element_rect(fill = "transparent", colour = "transparent"))
    return(plot)
  }
  polcolpal <- list(values = df2,choose_color = choose_color,ggplot_scale = ggplot_scale, legend = legend)
  class(polcolpal) <- c("rs_polcolpal", class(polcolpal))
  polcolpal
}
