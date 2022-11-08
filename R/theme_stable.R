#' A ggplot2 theme
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' ggplot(iris) +
#'    geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'    theme_stable()

theme_stable <- function(...) {
          theme(rect = element_rect(fill = "black"),
                   panel.background = element_rect(fill = "grey30",
                                                   color = "black",
                                                   size = 0),
                   panel.grid.major = element_line("grey40"), 
                   panel.grid.minor = element_line("grey40"),
                   axis.text = element_text(color = "grey100",
                                                     size = 13),
                   axis.title = element_text(color = "grey100",
                                                      size = 14),
                   legend.background = element_rect(fill = "black"),
                   legend.text = element_text(color = "grey100",
                                                       size = 11),
                   legend.title = element_text(color = "grey100"),
                   legend.position = "bottom")
}
