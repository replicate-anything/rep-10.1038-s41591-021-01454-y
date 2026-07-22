# Fig. 2 — Reasons not to take the vaccine
# Study: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y

library(dplyr)
library(ggplot2)
library(forcats)
library(estimatr)
library(broom)
library(plyr)

source("./helpers/analysis.R")
source("./helpers/labels.R")

make_fig_2 <- function(data) {
  df2 <- data
  no_vars <- names(dplyr::select(df2, dplyr::starts_with("no_vaccine_")))

  no_vacc <- lapply(no_vars, reasons_together, df = df2, num = c("No", "DK")) |>
    dplyr::bind_rows() |>
    dplyr::arrange(outcome) |>
    dplyr::mutate(
      dplyr::across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1)),
      n_sub = round(n * estimate, 0),
      n_sub = ifelse(n_sub == 0, NA_integer_, n_sub),
      group = factor(
        group,
        levels = rev(c(
          "Burkina Faso", "Colombia", "Mozambique", "Nepal", "Nigeria",
          "Pakistan 1", "Rwanda", "Sierra Leone 1", "Sierra Leone 2",
          "Uganda 1", "Uganda 2", "All", "Russia", "USA"
        ))
      )
    ) |>
    dplyr::left_join(outcome_dictionary, by = "outcome") |>
    dplyr::mutate(
      name = ifelse(group != "All", paste0(group, " (n=", n, ")"), "All"),
      name = gsub(pattern = " \\(", "\n(", name),
      tag = as.factor(tag),
      tag = forcats::fct_relevel(tag, no_vaccine_tag_levels)
    )

  special_cases <- sort(
    unique(no_vacc$name)[grep(unique(no_vacc$name), pattern = "All|Russia|USA")]
  )
  no_vacc |>
    dplyr::mutate(
      name = factor(
        x = name,
        ordered = TRUE,
        levels = rev(c(
          sort(unique(name)[!(unique(name) %in% special_cases)]),
          special_cases
        ))
      )
    ) |>
    dplyr::filter(!is.na(n_sub)) |>
    dplyr::mutate(name = plyr::mapvalues(name, "All", "All LMICs", warn_missing = FALSE))
}

format_fig_2 <- function(object) {
  ggplot2::ggplot(object, ggplot2::aes(name, estimate, color = tag)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      linewidth = 0.5,
      width = 0.2,
      position = ggplot2::position_dodge(0.6)
    ) +
    ggplot2::geom_point(shape = 16, position = ggplot2::position_dodge(0.6)) +
    ggplot2::facet_grid(. ~ tag, space = "free", labeller = ggplot2::label_wrap_gen(width = 15)) +
    ggplot2::geom_vline(xintercept = 3.5, color = "darkgrey") +
    ggplot2::geom_vline(xintercept = 2.5, color = "darkgrey") +
    ggplot2::guides(color = "none") +
    ggplot2::scale_colour_manual(values = safe_colorblind_palette) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Why would you not take the COVID-19 vaccine?",
      x = ""
    ) +
    ggplot2::ylim(c(-2, 100)) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text.y = ggplot2::element_text(hjust = 0)
    )
}
