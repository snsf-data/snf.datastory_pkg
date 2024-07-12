### Specific SNSF colors with different brightness levels (where available)

## Primary colors (100 percent, 70 percent, 30 percent, 10 percent)

#' @title SNSF dark blue primary color (100\%)
#'
#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_black)

snsf_black <- "#04293C"

#' @title SNSF dark blue colors (100\%, 70\%, 30\%, 10\%)
#'
#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_dark_blues)

snsf_dark_blues <- c("#5298BD", "#86B7D1", "#CBE0EB", "#EDF4F8")

#' @title SNSF blue colors (100\%, 70\%, 30\%, 10\%)
#'
#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_blues)

snsf_blues <- c("#83D0F5", "#A8DEF8", "#DAF1FC", "#F2FAFE")

#' @title SNSF dark red colors (100\%, 70\%, 30\%, 10\%)
#'
#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_dark_reds)

snsf_dark_reds <- c("#C95B40", "#D98C79", "#EFCDC5", "#F9EEEC")

#' @title SNSF red colors (100\%, 70\%, 30\%, 10\%)

#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_reds)

snsf_reds <- c("#F08262", "#F4A791", "#FAD9D0", "#FDF2EF")

## Secondary colors (100 percent, 50 percent)

#' @title SNSF yellow colors (100\%, 50\%)
#'
#' @description Secondary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_yellow)

snsf_yellow <- c("#FBBE5E", "#FDDEAE")

#' @title SNSF green colors (100\%, 50\%)
#'
#' @description Secondary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_green)

snsf_green <- c("#71B294", "#B8D8C9")

#' @title SNSF violett colors (100\%, 50\%)
#'
#' @description Secondary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_violet)

snsf_violet <- c("#9D90B9", "#CEC7DC")

#' @title SNSF gray colors (100\%, 50\%)
#'
#' @description Secondary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_grays)

snsf_grays <- c("#B2B1A7", "#D8D8D3")

#' @title The default datastory SNSF scheme (qualitative)
#'
#' @description Colors from the new Corporate Design of 2021.
#'
#' @export
#'
#' @examples
#'  # TODO

datastory_scheme_qualitative <- c(
  "#5298BD", # SNF Dunkelblau 100 percent
  "#FBBE5E", # SNF Gelb 100 percent
  "#71B294", # SNF Grün 100 percent
  "#9D90B9", # SNF Violett 100 percent
  "#F08262", # SNF Rot 100 percent
  # Lighter colors:
  "#86B7D1", # SNF Dunkelblau 70 percent
  "#FDDEAE", # SNF Gelb 50 percent
  "#B8D8C9", # SNF Grün 50 percent
  "#CEC7DC", # SNF Violett 50 percent
  "#F4A791" # SNF Rot 70 percent
)

#' @title The default datastory SNSF scheme without violet (qualitative)
#'
#' @description Colors from the new Corporate Design of 2021.
#'
#' @export
#'
#' @examples
#'  # TODO

datastory_scheme_qualitative_nv <-
  datastory_scheme_qualitative[
    !(datastory_scheme_qualitative %in% c("#9D90B9", "#CEC7DC"))
  ]

#' @title SNSF blues (CD 2021)
#'
#' @description Colors from the new Corporate Design of 2021.
#'
#' @export
#'
#' @examples
#'  # TODO

datastory_scheme_blue_seq <-
  c("#5298BD", # SNF Dunkelblau 100 percent
    "#86B7D1", # SNF Dunkelblau 70 percent
    "#83D0F5", # SNF Blau 100 percent
    "#A8DEF8") # SNF Blau 70 percent

#' @title Get your datastory SNSF scheme
#'
#' @description palettes : default (qualitative), qualitative, blue_seq,
#' green_seq, yellow_seq, gray_seq.
#'
#' @param palette Color palette to choose. Available: "default" (qualitative),
#' "qualitative", "blue_seq", "green_seq", "yellow_seq", "gray_seq"
#' @param n_col The number of colors to return. If it exceeds the number of
#' colors in the chosen palette, new colors are interpolated. Do not set this
#' argument if you want to get the original number of colors in the chosen
#' palette.
#'
#' @export
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#'  # TODO

get_datastory_scheme <- function(palette = "default", n_col = NULL) {
  # Choose the right color palette
  colors <- switch(palette,
                   default = datastory_scheme_qualitative,
                   default_nv = datastory_scheme_qualitative_nv,
                   qualitative = datastory_scheme_qualitative,
                   blue_seq = datastory_scheme_blue_seq,
                   green_seq = snsf_green,
                   yellow_seq = snsf_yellow,
                   gray_seq = snsf_grays)

  # Interpolate colors if not enough are available
  if (!is.null(n_col)) {
    if (n_col > length(colors))
      colors <- grDevices::colorRampPalette(colors)(n_col)
    return(colors[1:n_col])
  } else
    return(colors)
}

# SNSF data story scale functions ####

# The following functions are in majority inspired by the series of
# scale_*_viridis functions from the 'viridis' package. See the following page:
# https://github.com/sjmgarnier/viridis/blob/master/R/scales.R

#' @title SNSF data story color palettes generator
#'
#' @description Wrapper function around \code{get_datastory_scheme} to transform it
#' into a palette generator function compatible with
#' \code{\link[ggplot2]{discrete_scale}} or
#' \code{\link[ggplot2]{scale_fill_gradient}}/
#' \code{\link[ggplot2]{scale_fill_gradient2}}/.
#'
#' @details For more information, see \code{\link[snf.datastory]{get_datastory_scheme}}
#' for more information on the SNSF data story color palettes available.
#'
#' @param palette Character string indicating the SNSF data story color palette
#' to use. Only those available in \code{get_datastory_scheme()} can be used.
#'
#' @param repeat_col Logical indicating whether the color should be repeated
#' sequentially (TRUE) when the palette does not return enough values (the
#' default). If set to FALSE, number of colors will be extended using
#' interpolation.
#'
#' @param reverse Logical indicating whether the scale should be reversed or
#' not (the default). Note that when more colors need to be generated, the
#' the colors are first reversed before before repeated/interpolated (this
#' allows to get, for a given palette size, consistent colors whatever it is
#' reversed or not.)
#'
#' @export

datastory_apl <- function(palette = "default") {
  function(n) {
    get_datastory_scheme(n, palette = palette)
  }
}

#' @title SNSF data story color scales for ggplot2
#'
#' @description Fill and colour/color SNSF data story scale functions for
#'  \code{\link[ggplot2]{ggplot2}}.
#'
#' @param ... Parameters passed to external functions used to generate the
#' palette: \code{\link[ggplot2]{discrete_scale}},
#' \code{\link[ggplot2]{scale_fill_gradient}},
#' \code{\link[ggplot2]{scale_fill_gradient2}},
#' \code{\link[ggplot2]{scale_color_gradient}} or
#' \code{\link[ggplot2]{scale_color_gradient2}}.
#'
#' @param palette Character string indicating the SNSF data story color palette
#' to use. Only those available in \code{get_datastory_scheme()} can be used.
#'
#' @param type Character string indicating the type of scale used to color
#' the data. Four options are available: "qual" (default) for
#' qualitative/discrete data, "seq" for sequential qualitative/discrete data,
#' "cont" for continuous data, and "div" for diverging continuous data.
#'
#' @param grad_col Character string indicating the color of the continuous
#' (continuous scale only). You can choose between 'blue' (the default) and
#' 'red'. The gradient goes from blue/red 30% to dark blue/red 100%.
#'
#' @param aesthetics Character string or vector of character strings listing the
#' name(s) of the aesthetic(s) that this scale works with.
#'
#' @rdname scale_snsf
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(forcats)
#'
#' # Qualitative scale
#' mpg %>%
#'   ggplot() +
#'   aes(x = class, fill = class) +
#'   geom_bar(aes(fill = class)) +
#'   scale_fill_datastory(type = "qual") +
#'   coord_flip() +
#'   get_snsf_theme(
#'     flipped = TRUE,
#'     legend_position = "right"
#'   )
#'
#' # Sequential scale
#' mpg %>%
#'   summarise(mean = mean(cty), .by = cyl) %>%
#'   mutate(cyl = fct_reorder(as.character(cyl), mean, .desc = TRUE)) %>%
#'   ggplot() +
#'   aes(x = cyl, y = mean, fill = cyl) +
#'   geom_col() +
#'   scale_fill_datastory(type = "seq") +
#'   get_snsf_theme()
#'
#' # Continuous scale
#' mpg %>%
#'   ggplot() +
#'   aes(x = cty, y = displ, color = hwy) +
#'   geom_point() +
#'   coord_flip() +
#'   scale_color_datastory(type = "cont") +
#'   get_snsf_theme()
#'
#' # Diverging scale
#' mpg %>%
#'   mutate(hwy = (hwy - mean(hwy)) / sd(hwy)) %>%
#'   ggplot() +
#'   aes(x = cty, y = displ, color = hwy) +
#'   geom_point() +
#'   scale_color_datastory(type = "div") +
#'   coord_flip() +
#'   get_snsf_theme(flipped = TRUE)

scale_fill_datastory <- function(...,
                                 palette = "default",
                                 type = c("qual", "seq", "cont", "div"),
                                 grad_col = c("blue", "red", "yellow", "green"),
                                 div_col = c("br", "rb", "by", "yb", "bg", "gb"),
                                 aesthetics = "fill") {

  c_args <- rlang::call_args_names(rlang::current_call())

  type <- rlang::arg_match(type)
  grad_col <- rlang::arg_match(grad_col)
  div_col <- rlang::arg_match(div_col)

  # Inform the user that 'grad_col' is ignored when 'type = "seq"'
  if ("grad_col" %in% c_args && type %in% c("qual", "div")) {
    rlang::inform(
      c(
        "i" = paste0(
          "The argument `grad_col` is not used when the scale is ",
          "qualitative or diverging. Value passed to that argument was thus ",
          "ignored."
        )
      )
    )
  }

  # Inform the user that 'palette' is ignored when 'type' is not set to "seq".
  if ("palette" %in% c_args && type != "qual") {
    rlang::inform(
      c(
        "i" = paste0(
          "The argument `palette` is not used when the ",
          "scale is not qualitative. Any value passed to that arguments was ",
          "thus ignored."
        )
      )
    )
  }

  # Inform the user that 'div_col' is ignored when 'type' is not set to "div".
  if ("palette" %in% c_args && type != "qual") {
    rlang::inform(
      c(
        "i" = paste0(
          "The argument `palette` is not used when the ",
          "scale is not qualitative. Any value passed to that arguments was ",
          "thus ignored."
        )
      )
    )
  }

  # Qualitative scale using an SNSF theme
  if (type == "qual") {
    ggplot2::discrete_scale(
      aesthetics,
      "snsf_scheme_qual",
      datastory_apl(palette),
      ...
    )
  } else if (type == "cont") {
    high <- switch(
      grad_col,
      blue = snsf_dark_blues[1],
      red = snsf_reds[1],
      yellow = snsf_yellow[1],
      green = snsf_green[1]
    )

    ggplot2::scale_fill_gradient(
      low = snsf_grays[2],
      high = high,
      aesthetics = aesthetics,
      ...
    )
  } else if (type == "div") {
    ggplot2::scale_fill_gradient2(
      low =
        switch(
          div_col,
          br = snsf_dark_blues[1],
          by = snsf_dark_blues[1],
          bg = snsf_dark_blues[1],
          rb = snsf_reds[1],
          yb = snsf_yellow[1],
          gb = snsf_green[1]
        ),
      high =
        switch(
          div_col,
          br = snsf_reds[1],
          by = snsf_yellow[1],
          bg = snsf_green[1],
          rb = snsf_dark_blues[1],
          yb = snsf_dark_blues[1],
          gb = snsf_dark_blues[1]
        ),
      mid = "#e4e4e4",
      aesthetics = aesthetics,
      ...
    )
  } else if (type == "seq") {
    seq_cols <-
      c(
        snsf_grays[2],
        switch(grad_col,
               blue = snsf_dark_blues[1],
               red = snsf_reds[1],
               yellow = snsf_yellow[1],
               green = snsf_green[1]
        )
      )
    # if (reverse) seq_cols <- rev(seq_cols)
    ggplot2::discrete_scale(
      aesthetics,
      "snsf_scheme_seq",
      grDevices::colorRampPalette(seq_cols),
      ...
    )
  }
}

#' @rdname scale_snsf
#'
#' @export

scale_color_datastory <- function(...,
                                  palette = "default",
                                  type = c("qual", "seq", "cont", "div"),
                                  grad_col = c("blue", "red", "yellow", "green"),
                                  div_col = c("br", "rb", "by", "yb", "bg", "gb"),
                                  aesthetics = "color") {

  c_args <- rlang::call_args_names(rlang::current_call())

  type <- rlang::arg_match(type)
  grad_col <- rlang::arg_match(grad_col)
  div_col <- rlang::arg_match(div_col)

  # Inform the user that 'grad_col' is ignored when 'type = "seq"'
  if ("grad_col" %in% c_args && type %in% c("qual", "div")) {
    rlang::inform(
      c(
        "i" = paste0(
          "The argument `grad_col` is not used when the scale is ",
          "qualitative or diverging. Value passed to that argument was thus ",
          "ignored."
        )
      )
    )
  }

  # Inform the user that 'palette' is ignored when 'type' is not set to "seq".
  if ("palette" %in% c_args && type != "qual") {
    rlang::inform(
      c(
        "i" = paste0(
          "The argument `palette` is not used when the ",
          "scale is not qualitative. Any value passed to that arguments was ",
          "thus ignored."
        )
      )
    )
  }

  # Inform the user that 'div_col' is ignored when 'type' is not set to "div".
  if ("palette" %in% c_args && type != "qual") {
    rlang::inform(
      c(
        "i" = paste0(
          "The argument `palette` is not used when the ",
          "scale is not qualitative. Any value passed to that arguments was ",
          "thus ignored."
        )
      )
    )
  }

  # Qualitative scale using an SNSF theme
  if (type == "qual") {
    ggplot2::discrete_scale(
      aesthetics,
      "snsf_scheme_qual",
      snsf_pal(palette),
      ...
    )
  } else if (type == "cont") {
    high <- switch(
      grad_col,
      blue = snsf_dark_blues[1],
      red = snsf_reds[1],
      yellow = snsf_yellow[1],
      green = snsf_green[1]
    )
    ggplot2::scale_fill_gradient(
      low = snsf_grays[2],
      high = high,
      aesthetics = aesthetics,
      ...
    )
  } else if (type == "div") {
    ggplot2::scale_fill_gradient2(
      low =
        switch(
          div_col,
          br = snsf_dark_blues[1],
          by = snsf_dark_blues[1],
          bg = snsf_dark_blues[1],
          rb = snsf_reds[1],
          yb = snsf_yellow[1],
          gb = snsf_green[1]
        ),
      high =
        switch(
          div_col,
          br = snsf_reds[1],
          by = snsf_yellow[1],
          bg = snsf_green[1],
          rb = snsf_dark_blues[1],
          yb = snsf_dark_blues[1],
          gb = snsf_dark_blues[1]
        ),
      mid = "#e4e4e4",
      aesthetics = aesthetics,
      ...
    )
  } else if (type == "seq") {
    seq_cols <-
      c(
        snsf_grays[2],
        switch(grad_col,
               blue = snsf_dark_blues[1],
               red = snsf_dark_reds[1]
        )
      )
    # if (reverse) seq_cols <- rev(seq_cols)
    ggplot2::discrete_scale(
      aesthetics,
      "snsf_scheme_seq",
      grDevices::colorRampPalette(seq_cols),
      ...
    )
  }
}

#' @rdname scale_snsf
#'
#' @aliases scale_color_datastory
#'
#' @export

scale_colour_datastory <- scale_color_datastory
