# Helpers to check the format of meta data when rendering the data stories

#' @title Check data story name format
#'
#' @description Take the name and the language of the data story and checks the
#' format is correct.
#'
#' @param name A string with the data story name.
#' @param lang A string wit the language of the name (can be ignored if the
#' generic name is tested).
#'
#' @export

chk_ds_name <- function(name, lang = NULL) {

  ds_name_type <-
    ifelse(
      is.null(lang),
      "Data story name",
      "Language-sepcific data story name"
    )

  chk::chk_not_missing(name, x_name = ds_name_type)
  chk::chk_string(name, x_name = ds_name_type)
  if (chk::vld_subset(name, ""))
    cli::cli_abort(
      "{ds_name_type} must not be an empty string."
    )
  if (chk::vld_subset(lang, "") && !is.null(lang))
    cli::cli_abort(
      "`lang` must name not be an empty string."
    )
  chk::chk_subset(lang, c(NULL, "en", "de", "fr"), x_name = "`lang`")
  if (!is.null(lang)) chk::chk_string(lang, x_name = "`lang`")

  if (str_starts(name, "datastory") || str_ends(name, "datastory"))
    cli::cli_abort(
      paste0("{ds_name_type} must not start or end with \"datastory\".")
    )

  if (is.null(lang)) {

    if (str_detect(str_remove_all(name, "_"), "[^[:lower:]]"))
      cli::cli_abort(
        paste0(
          "{ds_name_type} must contain only lower characters separated with ",
          "underscore."
        )
      )

  } else {

    if (str_detect(str_remove_all(name, "-"), "[^[:lower:]]"))
      cli::cli_abort(
        paste0(
          "{ds_name_type} must contain only lower ",
          "characters separated with dash."
        )
      )

  }
}

#' @title Check data story title/lead format
#'
#' @description Take the title/lead and the language of the data story and
#' checks the format is correct.
#'
#' @param name A string with the data story title/lead.
#' @param which A string indicating whether it is the title or the lead.
#' @param lang A string wit the language of the name.
#'
#' @export

chk_ds_title_lead <- function(text, which = NULL, lang = NULL) {

  chk::chk_not_missing(text, x_name = "Data story title/lead")
  chk::chk_string(text, x_name = "Data story title/lead")
  if (chk::vld_subset(text, ""))
    cli::cli_abort(
      "Data story title/lead must not be an empty string."
    )
  chk::chk_string(which, x_name = "`which`")
  chk::chk_subset(which, c("title", "lead"), x_name = "`which`")
  chk::chk_string(lang, x_name = "`lang`")
  chk::chk_subset(lang, c("en", "de", "fr"), x_name = "`lang`")

  limit <- switch(which, title = 90, lead = 230)

  if (nchar(text) > limit)
    cli::cli_warn(
      paste0(
        "The data story {which}_{lang} is longer than the {limit} characters ",
        "limit. It is recommended not to have {which} longer that this ",
        "limit."
      )
    )

}

#' @title Check data story authors format
#'
#' @description Take the authors list of the data story and checks the format is
#' correct.
#'
#' @param authors A named list of characters (names must be "en", "de", and
#' "fr"). Each element of the list should be a vector of length equal to the
#' number of authors. As an example, a data story with 2 authors corresponds to
#' a 3-element list where each element is of length 2.
#'
#' @export

chk_ds_authors <- function(authors) {

  chk::chk_not_missing(authors, x_name = "Data story author(s)")
  chk::chk_not_null(authors, x_name = "Data story author(s)")
  chk::chk_not_any_na(authors, x_name = "Data story author(s)")
  if (chk::vld_subset(authors, ""))
    cli::cli_abort(
      "author(s) must not be an empty string."
    )

  chk::chk_list(authors, x_name = "Data story author(s)")
  chk::chk_length(authors, 3L, x_name = "Data story author(s)")
  chk::check_names(
    authors,
    names = c("en", "de", "fr"),
    x_name = "`authors`"
  )

  if (length(authors$en) != length(authors$de) || length(authors$en) != length(authors$fr))
    cli::cli_abort("The number of authors is not the same in all languages.")

}

#' @title Check data story category format
#'
#' @description Take the category of the data story and checks the format is
#' correct.
#'
#' @param cat A string where the element is any of: standard", "briefing",
#' "techreport", "policybrief", "flagship", or "figure".
#'
#' @export

chk_ds_category <- function(cat) {

  chk::chk_not_missing(cat, x_name = "Data story category")
  chk::chk_string(cat, x_name = "Data story category")
  if (chk::vld_subset(cat, ""))
    cli::cli_abort(
      "Data story category must not be an empty string."
    )
  chk::chk_subset(
    cat,
    c("standard", "briefing", "techreport", "policybrief", "flagship", "figure"),
    x_name = "Data story category"
  )

  invisible()

}

#' @title Check data story publication date format
#'
#' @description Take the publication  date of the data story and checks the
#' format is correct.
#'
#' @param date A string where the element must comply with the
#' YYYY-MM-DD HH:MM:SS format.
#'
#' @export

chk_ds_pub_date <- function(date) {

  chk::chk_not_missing(date, x_name = "Data story publication date")
  chk::chk_string(date, x_name = "Data story publication date")
  if (chk::vld_subset(publication_date, ""))
    cli::cli_abort(
      "Data story publication date must not be an empty string."
    )
  if (chk::vld_subset(date, ""))
    cli::cli_abort(
      "Data story publication date must not be an empty string."
    )
  if (nchar(str_remove(date, "\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2}")) > 0)
    cli::cli_abort(
      paste0(
        "Data story publication date seems to have the wrong fromat. The ",
        "format must be \"YYYY-MM-DD HH:MM:SS\""
      )
    )

  ymd <- as.numeric(str_extract(date, "(\\d{4})-(\\d{2})-(\\d{2})", group = 1:3))
  time <- as.numeric(str_extract(date, "(\\d{2}):(\\d{2}):(\\d{2})", group = 1:3))

  if (ymd[2] < 1 || ymd[2] > 12)
    cli::cli_abort(
      paste0(
        "\"MM\" in data story publication date (\"YYYY-MM-DD HH:MM:SS\") ",
        "must be between 01 and 12."
      )
    )
  if (ymd[3] < 1 || ymd[3] > 31)
    cli::cli_abort(
      paste0(
        "\"DD\" in data story publication date (\"YYYY-MM-DD HH:MM:SS\") ",
        "must be between 01 and 31."
      )
    )
  if (time[1] < 0 || time[1] > 23)
    cli::cli_abort(
      paste0(
        "\"HH\" in data story publication date (\"YYYY-MM-DD HH:MM:SS\") ",
        "must be between 00 and 23."
      )
    )
  if (time[2] < 0 || time[2] > 59)
    cli::cli_abort(
      paste0(
        "\"MM\" in data story publication date (\"YYYY-MM-DD HH:MM:SS\") ",
        "must be between 00 and 59."
      )
    )
  if (time[3] < 0 || time[3] > 59)
    cli::cli_abort(
      paste0(
        "\"SS\" in data story publication date (\"YYYY-MM-DD HH:MM:SS\") ",
        "must be between 00 and 59."
      )
    )

  invisible()

}

#' @title Check data story languages format
#'
#' @description Take the languages of the data story and checks the
#' format are correct.
#'
#' @param lang A character vector indicating the data story languages. The
#' vector must be of length 3 and contains the following elements: "en", "de",
#' and "fr".
#'
#' @export

chk_ds_lang <- function(lang) {

  chk::chk_not_missing(lang, x_name = "Data story languages")
  chk::chk_character(lang, x_name = "Data story languages")
  chk::chk_length(lang, 3, x_name = "Data story languages")
  if (mean(c("en", "de", "fr") %in% lang) != 1)
    cli::cli_abort(
      paste0(
        "Data story languages must be a length-3 character vector with ",
        "\"en\", \"de\", and \"fr\" (any in order) as input."
      )
    )

  invisible()

}

#' @title Check data story tags format
#'
#' @description Take the tags of the data story and checks the
#' format are correct.
#'
#' @param lang A integer vector mapping to the tag used for the data story.
#' There is no length limit, but the tags must be integers and valid. One can
#' check tags available using [get_ds_tags_df].
#'
#' @export

chk_ds_tags <- function(tags) {

  chk::chk_not_missing(tags, x_name = "Data story tags")
  chk::chk_integer(tags, x_name = "Data story tags")
  chk::chk_not_empty(tags, x_name = "Data story tags")

  if (mean(tags %in% get_ds_tags_df()$TagId) != 1)
    cli::cli_abort(
      paste0(
        "Not all data story tags can be found in the tag list. Use ",
        "{.fn snf.datastory::get_ds_tags_df} to see all valid tags."
      )
    )

  invisible()

}

#' @title Data story tags
#'
#' @description Tibble with the data story tags in 3 languages.
#'
#' @export

get_ds_tags_df <- function() {

  tibble::tribble(
    ~TagId,                      ~English,                         ~German,                         ~French,
    10L,                    "open access",                   "Open Access",     "Libre accès (open access)",
    20L,             "open research data",            "Open Research Data",            "Open Research Data",
    30L,           "data management plan",          "Data Management Plan",          "Data Management Plan",
    40L,                   "open science",                  "Open Science",               "Science ouverte",
    50L,                       "overhead",                      "Overhead",                      "Overhead",
    60L,                "gender equality",                "Gleichstellung",                       "Égalité",
    70L, "promotion of young researchers",            "Nachwuchsförderung",    "Encouragement de la relève",
    80L,                "project funding",              "Projektförderung",      "Encouragement de projets",
    90L,                       "Sinergia",                      "Sinergia",                      "Sinergia",
    100L,                        "Bridge",                        "Bridge",                        "Bridge",
    105L,          "Marcel Benoist Prize",          "Marcel Benoist Preis",           "Prix Marcel Benoist",
    110L,                    "Innosuisse",                    "Innosuisse",                    "Innosuisse",
    120L,                   "Horizon2020",                   "Horizon2020",                   "Horizon2020",
    130L,                           "ERC",                           "ERC",                           "ERC",
    140L,                            "EU",                            "EU",                            "UE",
    150L,     "international cooperation", "Internationale Zusammenarbeit",    "Coopération internationale",
    160L,             "value of research",          "Nutzen der Forschung",        "Impact de la recherche",
    170L,                      "covid-19",                      "Covid-19",                      "Covid-19",
    180L,                 "bibliometrics",                  "Bibliometrie",                  "Bibliométrie",
    190L,                "sustainability",                "Nachhaltigkeit",                    "Durabilité",
    200L,               "energy research",              "Energieforschung",         "Recherche énergétique",
    210L,          "agriculture research",      "Landwirtschaftsforschung",   "Recherche sur l'agriculture",
    220L,                    "monitoring",                    "Monitoring",                    "Monitoring",
    230L,  "funding decisions statistics",   "Zahlen zu Förderentscheiden",    "Chiffres sur des décisions",
    240L,                 "success rates",                 "Erfolgsquoten",                "Taux de succès",
    250L,                   "peer review",                   "peer review",      "Évaluation par les pairs",
    260L,      "research and development",     "Forschung und Entwicklung",    "Recherche et développement",
    270L,      "gender monitoring series",       "Serie Gender-Monitoring",     "Série monitoring du genre",
    280L,    "evaluation bodies (panels)",   "Evaluationsgremien (Panels)", "Comités d‘évaluation (panels)",
    290L,                "career funding",             "Karriereförderung",    "Encouragement de carrières",
    300L,                   "fellowships",                    "Stipendien",                       "Bourses",
    310L,              "open data series",            "Serie Offene Daten",        "Série données ouvertes",
    320L,           "preliminary figures",             "Vorläufige Zahlen",        "Chiffres préliminaires"
  )

}

#' @title Check the format of data story meta data
#'
#' @description Take all the meta data used to generated the data stories in the
#' three required language and test them. This is a wrapper integrating all the
#' meta data checker functions.
#'
#' @param datastory_name
#' @param datastory_name_en
#' @param datastory_name_de
#' @param datastory_name_fr
#' @param title_en
#' @param title_de
#' @param title_fr
#' @param lead_en
#' @param lead_de
#' @param lead_fr
#' @param datastory_category
#' @param publication_date
#' @param languages
#' @param tags_ids
#'
#' @export

chk_meta <- function(datastory_name,
                     datastory_name_en, datastory_name_de, datastory_name_fr,
                     title_en, title_de, title_fr,
                     lead_en, lead_de, lead_fr,
                     datastory_category, publication_date, languages,
                     tags_ids) {

  chk_ds_name(datastory_name)
  chk_ds_name(datastory_name_en, lang = "en")
  chk_ds_name(datastory_name_de, lang = "de")
  chk_ds_name(datastory_name_fr, lang = "fr")
  chk_ds_title_lead(title_en, "title", "en")
  chk_ds_title_lead(lead_en, "lead", "en")
  chk_ds_title_lead(title_de, "title", "de")
  chk_ds_title_lead(lead_de, "lead", "de")
  chk_ds_title_lead(title_fr, "title", "fr")
  chk_ds_title_lead(lead_fr, "lead", "fr")
  chk_ds_category(datastory_category)
  chk_ds_pub_date(publication_date)
  chk_ds_lang(languages)
  chk_ds_tags(tags_ids)

  cli::cli_alert_success(
    "No formatting problem encountered when checking the data story meta data."
  )

}
