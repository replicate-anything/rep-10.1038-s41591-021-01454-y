# Extended Data Fig. 2 — Leave-one/two-out acceptance distributions
# Study: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y

library(plyr)
library(dplyr)
library(ggplot2)
library(estimatr)
library(broom)
library(scales)
library(tidyr)

source("./helpers/analysis.R")

make_ext_fig_2 <- function(data) {
  df <- if (is.list(data) && !is.data.frame(data)) data$df else data[[1]]
  df2 <- if (is.list(data) && !is.data.frame(data)) data$df2 else data[[2]]

  loo_estimates <- plyr::ldply(
    .data = list(1, 2),
    .fun = function(x) {
      d <- df |>
        dplyr::filter(dplyr::if_all(c(take_vaccine_num, cluster, weight), ~ !is.na(.))) |>
        dplyr::filter(!(country %in% c("USA", "Russia")))
      loo_helper(d, "country", loo_n = x) |>
        dplyr::mutate(m = paste0("Leaving ", x, " out"), tag = "All", var = "All")
    }
  )

  loo_sub <- plyr::mdply(
    .data = tidyr::expand_grid(
      x = c(1, 2),
      var = c("gender", "educ_binary", "age_groups_three")
    ),
    .fun = function(x, var) {
      d <- df2 |>
        dplyr::filter(dplyr::if_all(c(take_vaccine_num, cluster, weight), ~ !is.na(.))) |>
        dplyr::filter(group == "All")
      loo_helper(
        d, "country",
        loo_n = x,
        loo_fun = function(dat) grp_analysis(dat, y = "take_vaccine_num", x = var)
      ) |>
        dplyr::mutate(m = paste0("Leaving ", x, " out"), var = var)
    }
  ) |>
    dplyr::mutate(
      tag = dplyr::coalesce(gender, educ_binary, age_groups_three),
      var = plyr::mapvalues(
        var,
        from = c("gender", "educ_binary", "age_groups_three"),
        to = c("By gender", "By education", "By age"),
        warn_missing = FALSE
      )
    )
  loo_estimates <- dplyr::bind_rows(loo_estimates, loo_sub)

  hist_data <- loo_estimates |>
    dplyr::mutate(
      var = factor(var, levels = c("All", "By gender", "By education", "By age")),
      tag = factor(
        tag,
        ordered = TRUE,
        levels = rev(c(
          "Female", "Male", "Up to Secondary", "> Secondary",
          "<25", "25-54", "55+", "All"
        )),
        labels = rev(c(
          "Female", "Male", "Up to Secondary", "More than Secondary",
          "<25 yr", "25-54 yr", "55+ yr", "All"
        ))
      )
    ) |>
    dplyr::select(var, m, tag, estimate) |>
    dplyr::filter(!is.na(m), !is.na(var), !is.na(tag))

  ref_groups <- dplyr::filter(df2, group %in% c("All", "USA", "Russia"))

  ref_data <- dplyr::bind_rows(
    ref_groups |>
      dplyr::filter(dplyr::if_all(c(take_vaccine_num, cluster, weight), ~ !is.na(.))) |>
      dplyr::nest_by(group) |>
      dplyr::summarize(
        lm_helper(
          data = data,
          formula = take_vaccine_num ~ 1,
          cluster = cluster,
          weight = weight,
          se_type = "stata"
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(cat = "All", var = "All"),
    grp_analysis(ref_groups, y = "take_vaccine_num", x = "gender") |>
      dplyr::rename(cat = gender) |>
      dplyr::mutate(var = "By gender"),
    grp_analysis(ref_groups, y = "take_vaccine_num", x = "educ_binary") |>
      dplyr::rename(cat = educ_binary) |>
      dplyr::mutate(var = "By education"),
    grp_analysis(ref_groups, y = "take_vaccine_num", x = "age_groups_three") |>
      dplyr::filter(statistic != Inf, conf.low > 0) |>
      dplyr::rename(cat = age_groups_three) |>
      dplyr::mutate(var = "By age")
  ) |>
    dplyr::mutate(
      estimate = round(estimate * 100, digits = 1),
      group = plyr::mapvalues(group, from = "All", to = "All LMIC", warn_missing = FALSE),
      cat = plyr::mapvalues(
        cat,
        from = c("<25", "25-54", "55+", "> Secondary"),
        to = c("<25 yr", "25-54 yr", "55+ yr", "More than Secondary"),
        warn_missing = FALSE
      ),
      tag = cat,
      var = factor(var, levels = c("All", "By gender", "By education", "By age"))
    ) |>
    dplyr::select(var, tag, estimate, cat, group)

  ref_data <- dplyr::bind_rows(
    dplyr::mutate(ref_data, m = "Leaving 1 out"),
    dplyr::mutate(ref_data, m = "Leaving 2 out")
  ) |>
    dplyr::filter(!is.na(m), !is.na(var), !is.na(tag))

  list(hist = hist_data, ref = ref_data)
}

format_ext_fig_2 <- function(object) {
  hist_data <- object$hist
  ref_data <- object$ref
  hist_data$estimate <- hist_data$estimate * 100

  ggplot2::ggplot(hist_data, ggplot2::aes(estimate, color = tag, fill = tag)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 200,
      position = "dodge",
      alpha = 0.3,
      linewidth = 0.3
    ) +
    ggplot2::geom_vline(
      data = ref_data,
      ggplot2::aes(xintercept = estimate, color = cat, linetype = group),
      linewidth = 0.9
    ) +
    ggplot2::facet_grid(var + tag ~ m) +
    ggplot2::scale_color_manual(values = safe_colorblind_palette) +
    ggplot2::scale_fill_manual(values = safe_colorblind_palette) +
    ggplot2::scale_linetype_manual(values = c("solid", "11", "dashed")) +
    ggplot2::scale_x_continuous(n.breaks = 8) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1, suffix = "", accuracy = 0.1)
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}
