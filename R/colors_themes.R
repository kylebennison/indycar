indycar_col_list <- c(
  lightest_blue = "#5c6272",
  lighter_blue = "#4c5872",
  light_blue = "#394871",
  medium_blue = "#22345a",
  dark_blue = "#041e42",
  orange = "#de703b",
  sign = "#1e1e1e",
  white = "#FFFFFF"
)

indycar_palette <- c("#5c6272", "#ffffff", "#de703b")


#' Create a color ramp
#'
#' @description This can be used for gt() tables if you want
#' a gradient from one color to another representing low-to-high values.
#'
#' @param x a numeric value that you want to convert to a color along a scale from
#' min being the first color in the indycar palette to max being the last.
indycar_ramp <- function(x) rgb(colorRamp(c(indycar_palette))(x), maxColorValue = 255)

#' Get a list of indycar colors and hexes
#'
#' @param ... either NULL or a character vector of colors in the palette you wish
#' to return hex values for.
#'
#' @description Simply call indycar_colors() to get a list of our
#' theme colors and their hex values, or provide a color name and it will
#' return the hex.
indycar_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (indycar_col_list)

  indycar_col_list[cols]
}

#' Theme for prettifying ggplots
#'
#' @description
#' Theme. Simply use + indycar_theme to add this theme to any custom ggplot2 object
#'
#' @export
indycar_theme <-
  ggplot2::theme(
    plot.caption = ggplot2::element_text(
      size = 12,
      hjust = 1,
      color = indycar_colors("orange")
    ),
    plot.title = ggplot2::element_text(
      color = indycar_colors("dark_blue"),
      size = 30,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(color = indycar_colors("light_blue"), size = 20),
    axis.text = ggplot2::element_text(color = indycar_colors("lightest_blue"), size = 14),
    axis.title = ggplot2::element_text(
      color = indycar_colors("lighter_blue"),
      size = 16,
      face = "bold"
    ),
    legend.title = ggplot2::element_text(
      color = indycar_colors("lighter_blue"),
      size = 16,
      face = "bold"
    ),
    legend.text = ggplot2::element_text(color = indycar_colors("lightest_blue"), size = 14),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    # no gridlines. If you want them, use ggplot2::element_line()
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(color = "#d6d6d6"
    )
  )
