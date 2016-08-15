#' Quickly plot Suma data using ggplot2
#'
#' Quickly plot Suma data with a pre-defined bar chart
#' @param df Data frame containing Suma Data
#' @param xVar What variable you'd like to use for the X axis of your plot(s)
#' @param op What operation you want to use to get the data
#' @param groupBy Optional variable defining how to group data
#' @param filterBy Optional variable defining how to filter data
#' @param facetBy Optional variable for faceting your plot
#' @param fillBy Optional variable to define the fill of the bars
#' @export
suma_plot <- function(df, xVar, op = "max", groupBy = NULL, filterBy = NULL, facetBy = NULL, fillBy = NULL) {
  func <- match.fun(paste0("suma_", op, "_count"))
  tmp <- func(df, filterBy, groupBy)
  print(tmp)
  p <- ggplot2::ggplot(tmp, ggplot2::aes_string(x = quote(xVar), y = "value"))
  if(!is.null(fillBy)){
    p <- p + ggplot2::geom_bar(ggplot2::aes_string(fill = fillBy), stat = "identity")
  }
  if(!is.null(facetBy)){
    p <- p + ggplot2::facet_wrap(facetBy)
  }
  return(p)
}
