#' "Grotta" bars for comparing modified Rankin Scale
#'
#' @description
#' Charts comparing modified Rankin Scale of two groups using a stacked bar for each
#' group, which a diagonal linking equivalent levels are known as "Grotta" bars.
#'
#' @param data data.frame with columns for `mrs` and `group`
#' @param mrs character string indicating variable in `data` for mRS. mRS data
#' should be numeric or integers values in (0,1,2,3,4,5,6)
#' @param bar_group character string indicating variable in `data` to be used for bars
#' @param bar_width widths of bars as a proportion, numeric between 0 and 1
#'
#' @keywords grotta mRS "grotta bars"
#' @importFrom ggplot2 ggplot aes geom_bar geom_line
#' @importFrom rlang sym !! :=
#' @importFrom stats reformulate xtabs
#' @export
#' @examples
#'
#' # simple example
#'pl <- grotta_bars(data = mRS,
#'                  mrs = "mRS",
#'                  bar_group = "intv")
#'
#'# ggplot object can be further adjusted as usual
#'# theme: note that some themes will move the legend
#'library(ggplot2)
#'pl +
#'  theme_bw()
#'
#'# title, fill/colour palette, facets
#'pl +
#'  labs(title = "mRS pre- vs post-intervention") +
#'  scale_fill_brewer(palette = "Blues") +
#'  facet_grid(rows = vars(group))
#'
#'# more complicated example, for a very simple chart
#'#change data, adding the variable bar_n
#'\dontrun{
#'pl2 <- pl
#'
#'pl2$data <- pl2$data %>%
#'  dplyr::group_by(intv) %>%
#'  dplyr::mutate(bar_n = dplyr::n()) %>%
#'  dplyr::ungroup()
#'
#'# access bar count data with after_stat(count). In this case we weight by 1/bar_n
#'# (ie divide by total count) and multiply by 100 to get within bar percentages.
#'# label uses ifelse() and sprintf to prevent printing in narrow bars and to set a
#'# sensible number of decimal places
#'
#'pl2 +
#'  # percentage labels
#'  geom_text(aes(label = ifelse(after_stat(count) < 0.05,
#'                               "",
#'                               sprintf("%0.0f%%", 100 * after_stat(count))),
#'                x = after_stat(count),
#'                weight = 1/bar_n),
#'            stat = "count",
#'            position = position_fill(vjust = 0.5)) +
#'  # similar idea for mRS labels
#'  geom_text(aes(label = mRS,
#'                x = after_stat(count),
#'                y = stage(start = intv,
#'                          after_scale = y + (y - 1.5)*0.6),
#'                weight = 1/bar_n),
#'            size = 4,
#'            stat = "count",
#'            position = position_fill(vjust = 0.5)) +
#'  # change colour scale
#'  scale_fill_brewer(palette = "Blues") +
#'  # change theme elements
#'  theme_void() +
#'  theme(axis.text.y = element_text()) +
#'  #remove legend
#'  guides(fill = "none") +
#'  # label bars ie y axis
#'  scale_y_discrete(labels = c(pre = "Pre-intervention", post = "Post-intervention"))}
#'
#'
#'

grotta_bars <- function(data, mrs, bar_group, bar_width = 0.5){
  # to avoide NSE CRAN check error
  count <- y <- NULL

  pl_dat <- data |>
    dplyr::mutate(!!sym(mrs) := factor(!!sym(mrs), levels = c(6,5,4,3,2,1,0)))

  cat("chart data:\n")
  print(xtabs(reformulate(c(mrs, bar_group)), pl_dat, addNA = T))

  pl <- ggplot(pl_dat, aes(fill = !!sym(mrs),
                           y = !!sym(bar_group))) +
    geom_bar(
      position = ggplot2::position_fill(),
      colour = "black",
      alpha = 0.75,
      width = bar_width) +
    geom_line(aes(x = ggplot2::after_stat(count),
                  #y = !!sym(bar_group),
                  y = ggplot2::stage(start = !!sym(bar_group),
                                     after_scale = (y - 1.5)*bar_width),
                  group = !!sym(mrs)),
    linetype = 1,
    alpha = 0.75,
    stat = "count",
    position = ggplot2::position_fill(vjust = 1))  +
    ggplot2::scale_x_continuous(name = "",
                                breaks = (0:10)/10,
                                labels = \(x) scales::percent(x, suffix = "%")) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, reverse = T, label.position = "top"))
  pl
  return(pl)
}
