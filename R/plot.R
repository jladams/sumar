#' Quickly plot Suma data using ggplot2
#'
#' Quickly plot Suma data with a pre-defined bar chart
#' @param df Data frame containing Suma Data
#' @param xVar What variable you'd like to use for the X axis of your plot(s)
#' @param op What operation you want to use to get the data
#' @param groupBy Optional argument defining how to group data
#' @param filterBy Optional argument defining how to filter data
#' @param facetBy Optional argument to define faceting your plot
#' @param fillBy Optional argument to define the fill of the bars
#' @param yVar Optional argument to specify variable you'd like to use fo the Y axis of your plot(s), defaults to "value"
#' @param legend Optional argument to decide whether to display legend (default TRUE)
#' @param position Optional argument to define position of bars, takes standard ggplot2 geom_bar arguments
#' @export
suma_plot <- function(df, xVar, op = "max", groupBy = NULL, filterBy = NULL, facetBy = NULL, fillBy = NULL,  yVar = "value", legend = TRUE, position = "stack") {

  # Decide which data function to use from op argument
  func <- match.fun(paste0("suma_", op, "_count"))

  # Get the temporary data set
  tmp <- func(df, filterBy, groupBy)

  # Create plot using xVar and yVar (yVar defaults to "value")
  p <- ggplot2::ggplot(tmp, ggplot2::aes_string(x = xVar, y = yVar))

  # Add bars
  p <- p + ggplot2::geom_bar(ggplot2::aes_string(fill = fillBy), position = position, stat = "identity")

  # Remove legend if specified
  if(legend == FALSE) {
    p <- p + ggplot2::guides(fill = FALSE)
  }

  # Add facets if there is a facetBy
  if(!is.null(facetBy)){
    p <- p + ggplot2::facet_wrap(facetBy)
  }

  return(p)
}
