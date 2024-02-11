#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nhanes_design
tabulate_risk_cross <- function(nhanes_design) {

  variables_left <- c(
    "PREVENT base equation" = "cvd_prevent_base_bnry_10",
    "PREVENT full equation" = "ascvd_prevent_base_bnry_10",
    "PREVENT base equation" = "cvd_prevent_base_bnry_15"
  )

  variables_right <- c(
    "Pooled cohort equations" = "ascvd_pce_bnry_10",
    "Pooled cohort equations" = "ascvd_pce_bnry_10",
    "Pooled cohort equations" = "ascvd_pce_bnry_10"
  )

  results <- map2_dfr(
    variables_left,
    variables_right,
    .f = ~ {

      f <- as.formula(paste("~", .x, "+", .y, "+ bp_cat_meds_included"))

      svytable(f, design = nhanes_design) %>%
        as_tibble() %>%
        rename(left_var = !!as.character(.x),
               right_var = !!as.character(.y)) %>%
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


  results

}
