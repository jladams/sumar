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
suma_plot <- function(df, xVar, op = "max", groupBy = NULL, filterBy = NULL, facetBy = NULL, fillBy = NULL,  yVar = "value", legend = TRUE) {

  # Decide which data function to use from op argument
  func <- match.fun(paste0("suma_", op, "_count"))

  # Get the temporary data set
  tmp <- func(df, filterBy, groupBy)

  # Create plot using xVar and yVar (yVar defaults to "value")
  p <- ggplot2::ggplot(tmp, ggplot2::aes_string(x = xVar, y = yVar))

  # Decide fill with fillBy
  p <- p + ggplot2::geom_bar(ggplot2::aes_string(fill = fillBy), stat = "identity")

  # Remove legend if specified
  if(legend == FALSE) {
    p <- p + ggplot2::guides(fill = FALSE)
  }

  # Add facets if there is a facetBy
  if(!is.null(facetBy)){
    p <- p + ggplot2::facet_wrap(facetBy)
  }

  print(p)
}
