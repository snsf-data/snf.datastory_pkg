#' @title Get the ggplot2 theme for data story plots
#'
#' @description The data plot theme is specially tailored for the clean look of
#' the data stories. By default, no axis titles and no grid lines are included,
#' they can be added when needed.
#'
#' @param legend_position Choose the position of the legend in regards to your
#' plot.
#' @param legend_margin ggplot \code{legend.margin} theme element.
#' @param gridline_axis TODO
#' @param title_axis Which axis titles should be shown, default X and Y:
#' \code{c("x", "y")}
#' @param text_axis Which axis text should be shown, default X and Y:
#' \code{c("x", "y")}
#' @param tick_axis On which axis the axis ticks should be displayed.
#' @param remove_plot_margin Whether all margin around the plot should be
#' removed. Default \code{FALSE}.
#' @param legend_key_size ggplot legend.key.width and legend.key.height theme
#' item: vector with first element width (mm) and second element height (mm).
#' @param family the name of the font to use to display text. Theinhardt is
#' used as default.
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#'  # TODO
get_datastory_theme <- function(legend_position = "top",
                                legend_margin = ggplot2::margin(0, 0, 0, 0),
                                gridline_axis = c(),
                                title_axis = c(),
                                text_axis = c("x", "y"),
                                tick_axis = c(),
                                remove_plot_margin = FALSE,
                                legend_key_size = c(),
                                family = "Theinhardt") {
  ds_theme <- ggplot2::theme(
    text = ggplot2::element_text(color = "#22211d", family = family),
    legend.title = ggplot2::element_blank(),
    legend.text =
      ggplot2::element_text(
        size = 8,
        margin = ggplot2::margin(l = 2, r = 2, unit = "mm")
      ),
    legend.position = legend_position,
    legend.justification = "left",
    legend.margin = legend_margin,
    legend.spacing.x = ggplot2::unit(0, "mm"),
    legend.key = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", size = 10),
    plot.subtitle = ggplot2::element_text(size = 8),
    plot.caption = ggplot2::element_text(size = 6, hjust =  0.5, face = "bold"),
    panel.background = ggplot2::element_blank(),
    axis.title.x =
      ggplot2::element_text(
        size = 9,
        margin =
          ggplot2::margin(
            t = 10,
            r = 0,
            b = 5,
            l = 0
          )
      ),
    axis.title.y =
      ggplot2::element_text(
        size = 9,
        margin =
          ggplot2::margin(
            t = 0,
            r = 10,
            b = 0,
            l = 5
          )
      ),
    strip.text = ggplot2::element_text(size = 8, face = "bold"),
    strip.background = ggplot2::element_rect(fill = NA, color = NA)
  )

  if (length(legend_key_size) > 0) {
    if (length(legend_key_size) == 2) {
      ds_theme <- ds_theme +
        ggplot2::theme(
          legend.key.width = ggplot2::unit(legend_key_size[1], "mm"),
          legend.key.height = ggplot2::unit(legend_key_size[2], "mm")
        )
    } else {
      warning("legend_key_size vector has not length 2 (width, height in mm)")
    }
  }

  if (remove_plot_margin) {
    ds_theme <- ds_theme +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "mm"))
  }

  # Add gridlines according to parameters
  if ("x" %in% gridline_axis) {
    ds_theme <- ds_theme +
      ggplot2::theme(
        panel.grid.major.x =
          ggplot2::element_line(
            color = "#AFAFAF",
            size = 0.2,
            linetype = "longdash"
          )
      )
  } else {
    theme <- ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }
  if ("y" %in% gridline_axis) {
    ds_theme <- ds_theme +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(
        color = "#AFAFAF",
        size = 0.2,
        linetype = "longdash"
      ))
  } else {
    ds_theme <-
      ds_theme +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }

  # Remove axis titles according to parameters
  if (!("x" %in% title_axis)) {
    ds_theme <-
      ds_theme +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if (!("y" %in% title_axis)) {
    ds_theme <-
      ds_theme +
      theme(axis.title.y = ggplot2::element_blank())
  }

  # Add axis text according to parameters
  if ("x" %in% text_axis) {
    ds_theme <- ds_theme +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 8, color = "#4F4F4F")
      )
  } else {
    ds_theme <-
      ds_theme +
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
  }
  if ("y" %in% text_axis) {
    ds_theme <- ds_theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8, color = "#4F4F4F")
      )
  } else {
    ds_theme <-
      ds_theme +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

  # Add ticks according to parameters
  if ("x" %in% tick_axis) {
    ds_theme <- ds_theme +
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_line(color = "#AFAFAF", size = 0.3)
      )
  } else {
    ds_theme <-
      ds_theme + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
  }
  if ("y" %in% tick_axis) {
    ds_theme <-
      ds_theme +
      ggplot2::theme(
        axis.ticks.y = ggplot2::element_line(color = "#AFAFAF", size = 0.3)
      )
  } else {
    ds_theme <-
      ds_theme +
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  }
  ds_theme
}

#' @title Make text bold for plotly with HTML
#'
#' @description \code{plotly::ggplotly()} is convenient but does not support
#' every aspect of a ggplot plot when converting it into a plotly plot. One of
#' these aspects is the \code{fontface} parameter of \code{geom_text}. The font
#' face can still be controlled for plotly output when wrapping HTML tags around
#' the actual text in \code{geom_text}, this function does this when the wished
#' output string should be formatted for plotly.
#'
#' @param x Character string to be formatted
#' @param is_plotly Whether to apply the formatting or not
#' @param make_bold Bold or not?
#' @param make_italic Italic or not?
#'
#' @export
#'
#' @examples
#'  format_when_plotly("hallo!", is_plotly = TRUE, make_bold = TRUE)
#'
format_when_plotly <- function(x, is_plotly = TRUE,
                               make_bold = TRUE,
                               make_italic = FALSE) {
  if (is_plotly) {
    if (make_italic)
      x <- paste0("<i>", x, "</i>")
    if (make_bold)
      x <- paste0("<b>", x, "</b>")
    x
  } else {
    x
  }
}

#' @title Get ggiraph tooltip CSS
#'
#' @description Get uniform tooltip CSS style for ggiraph graphs.
#'
#' @param family the name of the font to use to display text. Theinhardt is
#' used as default.
#'
#' @export

get_ggiraph_tooltip_css <- function(family = "Theinhardt") {
  paste0("font-family:'", family, "';background-color:black;",
         "font-size: 0.8em;padding: 0.5em;color:white;")
}
