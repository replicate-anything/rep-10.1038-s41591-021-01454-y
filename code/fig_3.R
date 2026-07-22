# Fig. 3 — Most trusted sources for COVID-19 vaccine decisions
# Study: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y

library(dplyr)
library(ggplot2)
library(forcats)
library(estimatr)
library(broom)
library(plyr)
library(stringr)

source("./helpers/analysis.R")
source("./helpers/labels.R")

make_fig_3 <- function(data) {
  trust_vacc <- list(
    All = lapply(
      trust_outcome_names, reasons_together,
      df = data, num = c("Yes", "No", "DK")
    ) |> dplyr::bind_rows(),
    Yes = lapply(
      trust_outcome_names, reasons_together,
      df = data, num = "Yes"
    ) |> dplyr::bind_rows(),
    No = lapply(
      trust_outcome_names, reasons_together,
      df = data, num = c("No", "DK")
    ) |> dplyr::bind_rows()
  ) |>
    dplyr::bind_rows(.id = "sub") |>
    dplyr::filter(!is.nan(statistic)) |>
    dplyr::mutate(
      dplyr::across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1)),
      n_sub = round(n * estimate, 0),
      n_sub = ifelse(n_sub == 0, NA_integer_, n_sub),
      group = factor(group, levels = trust_studies_levels)
    ) |>
    dplyr::left_join(outcome_dictionary, by = "outcome") |>
    dplyr::mutate(
      tag = as.factor(tag),
      tag = forcats::fct_relevel(tag, trust_tag_levels),
      sub = forcats::fct_relevel(as.factor(sub), "No", "Yes", "All"),
      sub = plyr::mapvalues(
        sub,
        from = c("No", "Yes", "All"),
        to = c("No, Don't know", "Yes", "Any"),
        warn_missing = FALSE
      )
    )

  dplyr::filter(trust_vacc, sub == "Any") |>
    dplyr::mutate(group = plyr::mapvalues(group, "All", "All LMICs", warn_missing = FALSE))
}

format_fig_3 <- function(object) {
  ggplot2::ggplot(object, ggplot2::aes(estimate, tag)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", fill = "#DDCC77") +
    ggplot2::facet_wrap(~group, ncol = 2, strip.position = "left") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_discrete(
      labels = function(x) stringr::str_wrap(x, width = 16),
      guide = ggplot2::guide_axis(angle = 90)
    ) +
    ggplot2::labs(
      title = "Which of the following people would you trust MOST to help you decide whether you would get a COVID-19 vaccine?",
      y = ""
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      axis.text.y = ggplot2::element_text(hjust = 0)
    )
}
