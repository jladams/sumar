#' Quickly plot Suma data using ggplot2
#'
#' Quickly plot Suma data with a pre-defined bar chart
#' @param df Data frame containing Suma Data
#' @param x What variable you'd like to use for the X axis of your plot(s)
#' @param op What operation you want to use to get the data
#' @param groupBy Optional variable defining how to group data
#' @param filterBy Optional variable defining how to filter data
#' @param facetBy Optional variable for faceting your plot
#' @param fillBy Optional variable to define the fill of the bars
#' @export
suma_plot <- function(df, x, op = "max", groupBy = NULL, filterBy = NULL, facetBy = NULL, fillBy = NULL) {
  func <- match.fun(paste0("suma_", op, "_count"))
  print(func)
  df <- func(df, filterBy, groupBy)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = value))
  if(!is.null(fillBy)){
    p <- p + ggplot2::geom_bar(aes(fill = fillBy), stat = "identity")
  }
  if(!is.null(facetBy)){
    p <- p + ggplot2::facet_wrap(facetBy)
  }
  return(p)
}
