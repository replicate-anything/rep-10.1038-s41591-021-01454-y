# Baked-in labels from studies_info / dictionary (avoid shipping xlsx for six targets)

# Geographic scope strings used in Fig. 1 y-axis tags (studies_info sample sheet)
geographic_scope <- c(
  "Burkina Faso" = "National",
  "Colombia" = "National",
  "India" = "Subnational, Slums in 2 cities",
  "Mozambique" = "Subnational, 2 cities",
  "Nepal" = "Subnational, 2 districts",
  "Nigeria" = "Subnational, 1 state",
  "Pakistan 1" = "Subnational, 2 districts",
  "Pakistan 2" = "Subnational, 1 province",
  "Russia" = "Subnational, 61 regions",
  "Rwanda" = "National",
  "Sierra Leone 1" = "National",
  "Sierra Leone 2" = "National",
  "Uganda 1" = "Subnational, 13 districts",
  "Uganda 2" = "Subnational, 1 district",
  "USA" = "National"
)

national_sample_countries <- c(
  "Burkina Faso", "Colombia", "Rwanda", "Sierra Leone 1", "Sierra Leone 2"
)

# Outcome → display tag (dictionary.xlsx subset used by Fig. 2 / Fig. 3 / Ext. Fig. 1)
outcome_dictionary <- data.frame(
  outcome = c(
    "no_vaccine_1", "no_vaccine_2", "no_vaccine_3", "no_vaccine_4",
    "no_vaccine_5", "no_vaccine_6", "no_vaccine_7", "no_vaccine_8",
    "no_vaccine_9", "no_vaccine_666",
    "trust_vaccine_5", "trust_vaccine_6",
    "trust_recode_1", "trust_recode_2", "trust_recode_3",
    "trust_recode_4", "trust_recode_5"
  ),
  tag = c(
    "Concerned about side effects",
    "Concerned about getting coronavirus from the vaccine",
    "Not concerned about getting seriously ill",
    "Doesn't think vaccines are effective",
    "Doesn't think Coronavirus outbreak is as serious as people say",
    "Doesn't like needles",
    "Allergic to vaccines",
    "Won't have time to get vaccinated",
    "Mentions a conspiracy theory",
    "Other reasons",
    "Health workers",
    "Government or MoH",
    "Family or Friends",
    "Newspapers, radio or online groups",
    "Famous person, religious leader or traditional healers",
    "Other",
    "Don't know or Refuse"
  ),
  stringsAsFactors = FALSE
)

trust_outcome_names <- c(
  "trust_recode_1", "trust_recode_2", "trust_recode_3",
  "trust_recode_4", "trust_recode_5", "trust_vaccine_5", "trust_vaccine_6"
)

trust_studies_levels <- c(
  "Burkina Faso", "Colombia", "India", "Mozambique",
  "Nepal", "Nigeria", "Pakistan 1", "Rwanda",
  "Sierra Leone 1", "Sierra Leone 2", "Uganda 2",
  "All", "Russia", "USA"
)

trust_tag_levels <- c(
  "Health workers",
  "Government or MoH",
  "Family or Friends",
  "Famous person, religious leader or traditional healers",
  "Newspapers, radio or online groups",
  "Other",
  "Don't know or Refuse"
)

no_vaccine_tag_levels <- c(
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
