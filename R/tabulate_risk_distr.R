#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nhanes_design
tabulate_risk_distr <- function(nhanes_design) {

  variables <- c(
    "Pooled cohort equations" = "ascvd_pce_cat",
    "PREVENT base equation" = "ascvd_prevent_base_cat",
    "PREVENT full equation" = "ascvd_prevent_full_cat",
    "PREVENT base equation" = "cvd_prevent_base_cat",
    "PREVENT full equation" = "cvd_prevent_full_cat"
  )

  results <- map_dfr(
    variables,
    .f = ~ {

      f <- as.formula(paste("~", .x, "+ bp_cat_meds_included"))

      svytable(f, design = nhanes_design) %>%
        as_tibble() %>%
        rename(risk_cat = !!as.character(.x)) %>%
        group_by(bp_cat_meds_included) %>%
        mutate(p = n / sum(n)) %>%
        transmute(risk_cat,
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
