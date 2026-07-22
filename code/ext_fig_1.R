# Extended Data Fig. 1 — Trusted sources by gender
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

make_ext_fig_1 <- function(data) {
  trust_vacc_gender <- list(
    All = lapply(
      trust_outcome_names, reasons_together_subgroup,
      df = data, num = c("Yes", "No", "DK"),
      dem_group = "gender", dem_subgroup = c("Female", "Male")
    ) |> dplyr::bind_rows(),
    Male = lapply(
      trust_outcome_names, reasons_together_subgroup,
      df = data, num = c("Yes", "No", "DK"),
      dem_group = "gender", dem_subgroup = "Male"
    ) |> dplyr::bind_rows(),
    Female = lapply(
      trust_outcome_names, reasons_together_subgroup,
      df = data, num = c("Yes", "No", "DK"),
      dem_group = "gender", dem_subgroup = "Female"
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
      sub = forcats::fct_relevel(as.factor(sub), "Female", "Male", "All")
    )

  dplyr::mutate(
    trust_vacc_gender,
    group = plyr::mapvalues(group, "All", "All LMICs", warn_missing = FALSE)
  )
}

format_ext_fig_1 <- function(object) {
  ggplot2::ggplot(object, ggplot2::aes(estimate, tag, fill = sub)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~group, ncol = 2, strip.position = "left") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      name = "Answer",
      values = safe_colorblind_palette[c(1, 3, 2)]
    ) +
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
