# prep_micro — cluster IDs, survey weights, age/education bins, country + All rows
# Study: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y
# Writes outputs/df.rds (respondent-level) and outputs/df2.rds (with All-LMIC rows)

library(dplyr)

source("./helpers/analysis.R")

make_prep_micro <- function(data) {
  df <- data |>
    dplyr::group_by(study) |>
    dplyr::mutate(
      cluster = ifelse(is.na(cluster), paste(seq_len(dplyr::n())), cluster),
      cluster = paste0(gsub(" ", "_", tolower(country)), "_", cluster)
    )

  df <- df |>
    dplyr::group_by(study) |>
    dplyr::mutate(
      weight_replace = mean(weight, na.rm = TRUE),
      weight = dplyr::if_else(
        is.na(weight),
        dplyr::if_else(is.na(weight_replace), 1, weight_replace),
        weight
      ),
      weight = weight / sum(weight)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      age_groups = as.character(
        cut(x = age, breaks = c(-Inf, 18, 30, 45, 60, Inf), right = FALSE)
      ),
      age_groups_binary = ifelse(age >= 55, "55+", NA_character_),
      age_groups_binary = ifelse(age < 55, "<55", age_groups_binary),
      age_less24 = ifelse(age <= 24, 1, 0),
      age_25_54 = ifelse(age >= 25 & age <= 54, 1, 0),
      age_55_more = ifelse(age >= 55, 1, 0),
      age_groups_three = ifelse(age <= 24, "<25", NA_character_),
      age_groups_three = ifelse(age >= 25 & age <= 54, "25-54", age_groups_three),
      age_groups_three = ifelse(age >= 55, "55+", age_groups_three),
      educ_binary = dplyr::if_else(
        educ == "More than secondary",
        "> Secondary",
        "Up to Secondary"
      )
    )

  df2 <- dplyr::bind_rows(
    dplyr::mutate(df, group = country),
    dplyr::mutate(
      dplyr::filter(df, country != "USA" & country != "Russia"),
      group = "All"
    )
  ) |>
    dplyr::mutate(
      cluster = dplyr::if_else(
        group == "All",
        gsub(pattern = " ", replacement = "_", x = tolower(country)),
        cluster
      )
    )

  out_dir <- study_output_dir()
  saveRDS(df, file.path(out_dir, "df.rds"))
  saveRDS(df2, file.path(out_dir, "df2.rds"))
  invisible(df2)
}
