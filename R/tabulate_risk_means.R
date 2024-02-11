#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nhanes_design
tabulate_risk_means <- function(nhanes_design) {

  variables <- c(
    "Pooled cohort equations" = "ascvd_pce",
    "PREVENT base equation" = "ascvd_prevent_base",
    "PREVENT full equation" = "ascvd_prevent_full",
    "PREVENT base equation" = "cvd_prevent_base",
    "PREVENT full equation" = "cvd_prevent_full"
  )

  results <- map_dfr(
    variables,
    .f = ~ {

      f <- as.formula(paste("~", .x))

      svyby(f, by = ~bp_cat_meds_included,
            design = nhanes_design,
            svymean, na.rm = TRUE) %>%
        bind_confint() %>%
        rename_with(.fn = ~ "x", .cols = all_of(.x)) %>%
        mutate(across(c(x, ci_lower, ci_upper), ~.x * 100)) %>%
        transmute(bp_cat_meds_included,
                  value = table_glue("{x}\n({ci_lower}, {ci_upper})"),
                  outcome = str_extract(.x, pattern = "^ascvd|^cvd"))

    },
    .id = 'label') %>%
    mutate(
      outcome = recode(outcome,
                       "ascvd" = "ASCVD",
                       "cvd" = "Total CVD")
    ) %>%
    pivot_wider(names_from = bp_cat_meds_included,
                values_from = value)


  results

}
