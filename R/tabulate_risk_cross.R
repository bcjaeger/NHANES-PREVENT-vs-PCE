
# tar_load(nhanes_design_overall)
# nhanes_design = nhanes_design_overall

tabulate_risk_cross <- function(nhanes_design) {

  variables_prevent <- c(
    "PREVENT base equation" = "cvd_prevent_base_bnry_10",
    "PREVENT full equation" = "ascvd_prevent_base_bnry_10",
    "PREVENT base equation" = "cvd_prevent_base_bnry_15"
  )

  variables_pce <- c(
    "Pooled cohort equations" = "ascvd_pce_bnry_10",
    "Pooled cohort equations" = "ascvd_pce_bnry_10",
    "Pooled cohort equations" = "ascvd_pce_bnry_10"
  )

  results_overall <- map2_dfr(
    variables_prevent,
    variables_pce,
    .f = ~ {

      f <- as.formula(
        paste("~", .x, "+", .y, "+ bp_cat_meds_included")
      )

      svytable(f, design = nhanes_design) %>%
        as_tibble() %>%
        rename(left_var = !!as.character(.y),
               right_var = !!as.character(.x)) %>%
        group_by(bp_cat_meds_included) %>%
        mutate(p = n / sum(n)) %>%
        transmute(left_var,
                  right_var,
                  bp_cat_meds_included,
                  value = table_glue("{p * 100}"),
                  outcome = str_extract(.x, pattern = "^ascvd|^cvd"))

    },
    .id = 'label') %>%
    mutate(
      outcome = recode(outcome,
                       "ascvd" = "ASCVD",
                       "cvd" = "Total CVD")
    ) %>%
    pivot_wider(names_from = bp_cat_meds_included,
                values_from = value) %>%
    mutate(label = paste(label, outcome, sep = ' - ')) %>%
    select(-outcome)

  nhanes_design$variables <- nhanes_design$variables %>%
    mutate(age_cat = if_else(demo_age_years < 60,
                             "Age < 60 years",
                             "Age ≥ 60 years"))

  results_by_age <- map2_dfr(
    variables_prevent,
    variables_pce,
    .f = ~ {

      f <- as.formula(
        paste("~", .x, "+", .y, "+ bp_cat_meds_included + age_cat")
      )

      svytable(f, design = nhanes_design) %>%
        as_tibble() %>%
        rename(left_var = !!as.character(.y),
               right_var = !!as.character(.x)) %>%
        group_by(bp_cat_meds_included, age_cat) %>%
        mutate(p = n / sum(n)) %>%
        transmute(left_var,
                  right_var,
                  bp_cat_meds_included,
                  age_cat,
                  value = table_glue("{p * 100}"),
                  outcome = str_extract(.x, pattern = "^ascvd|^cvd"))

    },
    .id = 'label') %>%
    mutate(
      outcome = recode(outcome,
                       "ascvd" = "ASCVD",
                       "cvd" = "Total CVD")
    ) %>%
    pivot_wider(names_from = bp_cat_meds_included,
                values_from = value) %>%
    mutate(label = paste(label, outcome, sep = ' - '),
           label = paste(label, age_cat, sep = "; ")) %>%
    ungroup() %>%
    select(-outcome, -age_cat)

  list(overall = results_overall,
       by_age = results_by_age)

}
