# Reason not to take the vaccine ? COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Study repo: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_9.R

library(dplyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(tidyr)
library(broom)
library(estimatr)

make_tab_9 <- function(data) {
  study_weighting <- function(data) {
    data |>
      dplyr::group_by(country) |>
      dplyr::mutate(weight = weight / sum(weight)) |>
      dplyr::ungroup()
  }

  lm_helper <- function(data, ...) {
    data <- study_weighting(data)
    fit <- estimatr::lm_robust(data = data, ...)
    dplyr::bind_cols(broom::tidy(fit), n = nobs(fit))
  }

  reasons_together <- function(data, reason, num = c("No", "DK")) {
    data |>
      dplyr::filter(
        take_vaccine %in% num,
        if_all(c(all_of(reason), cluster, weight), ~ !is.na(.))
      ) |>
      dplyr::nest_by(group) |>
      dplyr::summarize(
        lm_helper(
          data = data,
          formula = as.formula(paste0(reason, "~ 1")),
          cluster = cluster,
          weight = weight,
          se_type = "stata"
        ),
        .groups = "drop"
      )
  }

  df <- data |>
    dplyr::group_by(study) |>
    dplyr::mutate(
      cluster = ifelse(is.na(cluster), paste(1:dplyr::n()), cluster),
      cluster = paste0(gsub(" ", "_", tolower(country)), "_", cluster)
    )

  df <- df |>
    dplyr::group_by(study) |>
    dplyr::mutate(
      weight_replace = mean(weight, rm.na = TRUE),
      weight = if_else(
        is.na(weight),
        if_else(is.na(weight_replace), 1, weight_replace),
        weight
      ),
      weight = weight / sum(weight)
    ) |>
    dplyr::ungroup()

  df2 <- dplyr::bind_rows(
    dplyr::mutate(df, group = country),
    dplyr::mutate(
      dplyr::filter(df, country != "USA" & country != "Russia"),
      group = "All"
    )
  ) |>
    dplyr::mutate(
      cluster = if_else(
        group == "All",
        gsub(pattern = " ", replacement = "_", x = tolower(country)),
        cluster
      )
    )

  no_vars <- df2 |>
    dplyr::select(dplyr::starts_with("no_vaccine_")) |>
    (\(x) names(x))()

  no_vacc <- lapply(no_vars, function(v) {
    reasons_together(data = df2, reason = v, num = c("No", "DK"))
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1)))

  no_vacc2 <- no_vacc |>
    dplyr::mutate(
      estimate = format(estimate, nsmall = 1),
      conf_int = paste0("(", conf.low, ", ", conf.high, ")")
    ) |>
    dplyr::select(group, estimate, conf_int, outcome, n) |>
    tidyr::pivot_wider(
      names_from = outcome,
      values_from = c(estimate, conf_int, n),
      names_sep = "__"
    ) |>
    tidyr::pivot_longer(
      cols = c(dplyr::starts_with("estimate__"), dplyr::starts_with("conf_int__")),
      names_to = c("type", ".value"),
      names_pattern = "(.*)__(.*)"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      n = ifelse(group == "All", NA, unique(na.omit(dplyr::c_across(dplyr::starts_with("n__")))))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      group = forcats::fct_relevel(as.factor(group), "All", "Russia", "USA", after = Inf)
    ) |>
    dplyr::arrange(group) |>
    dplyr::mutate(
      across(c(group, n), ~ ifelse(type == "conf_int", "", as.character(.))),
      group = ifelse(group == "All", "All LMICs", group)
    ) |>
    dplyr::select(group, n, type, dplyr::starts_with("no_vaccine_"), -dplyr::starts_with("n_no_vaccine"), -type)

  cnames <- c(
    "Study", "N",
    "Concerned about side effects",
    "Concerned about getting coronavirus from the vaccine",
    "Not concerned about getting seriously ill",
    "Doesn't think vaccines are effective",
    "Doesn't think Coronavirus outbreak is as serious as people say",
    "Doesn't like needles",
    "Allergic to vaccines",
    "Won't have time to get vaccinated",
    "Mentions a conspiracy theory",
    "Other reasons"
  )

  tab <- no_vacc2 |>
    knitr::kable(
      col.names = cnames,
      caption = "\\label{no}Reasons not to take the vaccine",
      booktabs = TRUE,
      linesep = "",
      align = c("l", rep("c", 11)),
      format = "html",
      format.args = list(big.mark = ",", scientific = FALSE)
    ) |>
    kableExtra::kable_styling(full_width = FALSE) |>
    kableExtra::row_spec(0, bold = TRUE) |>
    kableExtra::column_spec(1:12, width = "7em") |>
    kableExtra::footnote(
      general_title = "",
      general = "Table S5 shows percentage of respondents mentioning reasons why they would not take the Covid-19 vaccine. The number of observations and percentage corresponds only to people who would not take the vaccine or are unsure. Respondents in all countries could give more than one reason. A 95% confidence interval is shown between parentheses.",
      threeparttable = TRUE
    )

  as.character(tab)
}

make_tab_9(utils::read.csv("../data/combined.csv", stringsAsFactors = FALSE))
