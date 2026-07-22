# prep_trust — collapse trust items into paper recodes for Fig. 3 / Ext. Fig. 1
# Study: https://github.com/replicate-anything/rep-10.1038-s41591-021-01454-y
# Reads outputs/df2.rds; writes outputs/df_trust.rds

library(dplyr)

source("./helpers/analysis.R")

make_prep_trust <- function(data) {
  df_trust <- data |>
    dplyr::mutate(
      trust_recode_1 = ifelse(trust_vaccine_1 == 1 | trust_vaccine_2 == 1, 1, 0),
      trust_recode_1 = ifelse(
        (country == "Nigeria" | country == "USA") & is.na(trust_recode_1),
        0, trust_recode_1
      ),
      trust_recode_2 = ifelse(trust_vaccine_8 == 1 | trust_vaccine_9 == 1, 1, 0),
      trust_recode_2 = ifelse(
        country == "Sierra Leone 2" & is.na(trust_recode_2),
        0, trust_recode_2
      ),
      trust_recode_3 = ifelse(
        trust_vaccine_3 == 1 | trust_vaccine_7 == 1 | trust_vaccine_4 == 1,
        1, 0
      ),
      trust_recode_3 = ifelse(
        (country == "Nigeria" | country == "USA" | country == "Russia") &
          is.na(trust_recode_3),
        0, trust_recode_3
      ),
      trust_recode_4 = ifelse(trust_vaccine_666 == 1 | trust_vaccine_other == 1, 1, 0),
      trust_recode_4 = ifelse(
        (country == "Burkina Faso" | country == "Sierra Leone 2" | country == "Russia") &
          is.na(trust_recode_4),
        0, trust_recode_4
      ),
      trust_recode_5 = ifelse(
        trust_vaccine_dk == 1 | trust_vaccine_refuse == 1 | trust_vaccine_nr == 1,
        1, 0
      ),
      trust_recode_5 = ifelse(
        (country == "Nigeria" | country == "Sierra Leone 2" | country == "USA") &
          is.na(trust_recode_5),
        0, trust_recode_5
      )
    )

  saveRDS(df_trust, file.path(study_output_dir(), "df_trust.rds"))
  invisible(df_trust)
}
