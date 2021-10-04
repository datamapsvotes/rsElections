#' Plots an rs_election or rs_redistribution object with a legend
#'
#' @param x An rs_election_as_sf object
#' @param pdf_file_name Name of the pdf file. If NA, then plots to viewer.
#'
#' @return Plots the object or returns a pdf file
#' @export
rs_plot <- function(x, pdf_file_name = NA){
  plot <- ggplot2::ggplot(x) +
    ggplot2::geom_sf(ggplot2::aes(fill = as.factor(!!as.name("col_code"))), color = "white") +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_fill_manual(values = attr(x,"palette")$ggplot_scale) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          legend.position = "none") +
    attr(x,"palette")$legend(8,pty_codes = unique(x$group_code)) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank()) +
    patchwork::plot_layout(ncol = 1, widths = 1, heights = c(1.2,sqrt(2)-1.2)) &
    ggplot2::theme(plot.margin = ggplot2::unit(c(0,0,0,0),"pt"))
  if (is.na(pdf_file_name)){
    plot
  }
  else {
    ggplot2::ggsave(filename = pdf_file_name, plot = plot)
  }
}
