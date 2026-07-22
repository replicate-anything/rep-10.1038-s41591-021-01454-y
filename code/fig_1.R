# Fig. 1 — Acceptance rates, overall and by respondent characteristics
# Study: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y
# make_* = weighted clustered means + national-sample pooled row; format_* = ggplot

library(dplyr)
library(ggplot2)
library(estimatr)
library(broom)
library(plyr)

source("./helpers/analysis.R")
source("./helpers/labels.R")

make_fig_1 <- function(data) {
  df2 <- data

  main_results <- df2 |>
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
    )

  ans <- dplyr::bind_rows(
    main_results |> dplyr::mutate(cat = "All", var = "All"),
    grp_analysis(df2, y = "take_vaccine_num", x = "gender") |>
      dplyr::rename(cat = gender) |>
      dplyr::mutate(var = "By gender"),
    grp_analysis(df2, y = "take_vaccine_num", x = "educ_binary") |>
      dplyr::rename(cat = educ_binary) |>
      dplyr::mutate(var = "By education"),
    grp_analysis(df2, y = "take_vaccine_num", x = "age_groups_three") |>
      dplyr::filter(statistic != Inf, conf.low > 0) |>
      dplyr::rename(cat = age_groups_three) |>
      dplyr::mutate(var = "By age")
  ) |>
    dplyr::mutate(dplyr::across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1)))

  tags <- data.frame(
    group = names(geographic_scope),
    scope = unname(geographic_scope),
    stringsAsFactors = FALSE
  ) |>
    dplyr::left_join(dplyr::filter(ans, cat == "All"), by = "group") |>
    dplyr::mutate(tag = paste0(group, " (", scope, ", ", n, ")")) |>
    dplyr::select(group, tag)

  ans <- ans |>
    dplyr::left_join(tags, by = "group") |>
    dplyr::mutate(tag = ifelse(group == "All", "All LMICs", tag)) |>
    dplyr::mutate(
      var = factor(var, levels = c("All", "By gender", "By education", "By age")),
      cat = factor(
        cat,
        ordered = TRUE,
        levels = rev(c(
          "Female", "Male", "Up to Secondary", "> Secondary",
          "<25", "25-54", "55+", "All"
        )),
        labels = rev(c(
          "Female", "Male", "Up to Secondary", "More than Secondary",
          "$< 25$", "$25-54$", "$55 +$", "All"
        ))
      ),
      tag = gsub(pattern = " \\(", "\n(", tag)
    )

  special_cases <- sort(
    unique(ans$tag)[grep(unique(ans$tag), pattern = "All LMICs|Russia|USA")]
  )
  ans <- ans |>
    dplyr::mutate(
      tag = factor(
        x = tag,
        ordered = TRUE,
        levels = rev(c(
          sort(unique(tag)[!(unique(tag) %in% special_cases)]),
          special_cases
        ))
      )
    )

  nationals <- dplyr::filter(df2, country %in% national_sample_countries)

  ans_n <- dplyr::bind_rows(
    nationals |>
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
    grp_analysis(nationals, y = "take_vaccine_num", x = "gender") |>
      dplyr::rename(cat = gender) |>
      dplyr::mutate(var = "By gender"),
    grp_analysis(nationals, y = "take_vaccine_num", x = "educ_binary") |>
      dplyr::rename(cat = educ_binary) |>
      dplyr::mutate(var = "By education"),
    grp_analysis(nationals, y = "take_vaccine_num", x = "age_groups_three") |>
      dplyr::filter(statistic != Inf, conf.low > 0) |>
      dplyr::rename(cat = age_groups_three) |>
      dplyr::mutate(var = "By age")
  ) |>
    dplyr::mutate(dplyr::across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1))) |>
    dplyr::filter(group == "All") |>
    dplyr::mutate(
      group = "All LMICs (National samples)",
      tag = "All LMICs (National samples)",
      var = factor(var, levels = c("All", "By gender", "By education", "By age")),
      cat = factor(
        cat,
        ordered = TRUE,
        levels = rev(c(
          "Female", "Male", "Up to Secondary", "> Secondary",
          "<25", "25-54", "55+", "All"
        )),
        labels = rev(c(
          "Female", "Male", "Up to Secondary", "More than Secondary",
          "$< 25$", "$25-54$", "$55 +$", "All"
        ))
      ),
      tag = gsub(pattern = " \\(", "\n(", tag)
    ) |>
    dplyr::bind_rows(ans)

  special_cases_n <- sort(
    unique(ans_n$tag)[
      grep(
        unique(ans_n$tag),
        pattern = "All LMICs \\(National samples\\)|All LMICs|Russia|USA"
      )
    ]
  )
  ans_n |>
    dplyr::mutate(
      tag = factor(
        x = tag,
        ordered = TRUE,
        levels = rev(c(
          sort(unique(tag)[!(unique(tag) %in% special_cases_n)]),
          special_cases_n
        ))
      )
    )
}

format_fig_1 <- function(object) {
  ggplot2::ggplot(object, ggplot2::aes(x = tag, y = estimate, color = cat)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      linewidth = 0.5,
      width = 0.2,
      position = ggplot2::position_dodge(0.6)
    ) +
    ggplot2::geom_point(position = ggplot2::position_dodge(0.6)) +
    ggplot2::facet_grid(. ~ var, scales = "free_x", space = "free") +
    ggplot2::coord_flip() +
    ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE, nrow = 2)) +
    ggplot2::geom_vline(xintercept = 4.5, color = "darkgrey") +
    ggplot2::geom_vline(xintercept = 3.5, color = "darkgrey") +
    ggplot2::geom_vline(xintercept = 2.5, color = "darkgrey") +
    ggplot2::scale_colour_manual(values = safe_colorblind_palette) +
    ggplot2::labs(
      title = "If a COVID-19 vaccine becomes available in [country], would you take it?",
      color = "Subgroups",
      x = ""
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::ylim(0, 100) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text.y = ggplot2::element_text(hjust = 0)
    )
}
