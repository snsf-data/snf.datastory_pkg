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
