# tar_load(nhanes_design_comorb)
# nhanes_design <- nhanes_design_comorb

tabulate_risk_distr <- function(nhanes_design) {

  variables <- c(
    "Pooled cohort equations" = "ascvd_pce_cat",
    "PREVENT base equation" = "ascvd_prevent_base_cat",
    "PREVENT full equation" = "ascvd_prevent_full_cat",
    "PREVENT base equation" = "cvd_prevent_base_cat",
    "PREVENT full equation" = "cvd_prevent_full_cat"
  )

  bpcats <- levels(nhanes_design$variables$bp_cat_meds_included)

  results <- map_dfr(
    variables,
    .f = ~ {

      lvls <- levels(nhanes_design$variables[[.x]])

      results <- list()

      for(i in lvls){

        nhanes_design$variables$tmp <-
          as.numeric(nhanes_design$variables[[.x]] == i)

        result <- list()

        for(j in bpcats){

          ..design <- subset(nhanes_design, bp_cat_meds_included == j)

          svy_ci_result <- svyciprop(~tmp, design = ..design)

          result[[j]] <- svy_ci_result %>%
            bind_confint() %>%
            mutate(bp_cat_meds_included = j,
                   x = as.numeric(x),
                   se = as.numeric(SE(svy_ci_result))) %>%
            rename(estimate = x) %>%
            mutate(
              suppress_status = suppress_catg(
                estimate = estimate,
                std_error = se,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                design = ..design
              )
            )

        }

        results[[i]] <- bind_rows(result)

      }

      bind_rows(results, .id = 'risk_cat') %>%
        mutate(outcome = str_extract(.x, pattern = "^ascvd|^cvd"))

      # svytable(f, design = nhanes_design) %>%
      #   as_tibble() %>%
      #   rename(risk_cat = !!as.character(.x)) %>%
      #   group_by(bp_cat_meds_included) %>%
      #   mutate(p = n / sum(n)) %>%
      #   transmute(risk_cat,
      #             bp_cat_meds_included,
      #             value = table_glue("{p * 100}"),
      #             outcome = str_extract(.x, pattern = "^ascvd|^cvd"))

    },
    .id = 'label') %>%
    mutate(
      outcome = recode(outcome,
                       "ascvd" = "ASCVD",
                       "cvd" = "Total CVD")
    )


  out <- results %>%
    mutate(value = table_glue("{estimate * 100}"),
           value = if_else(str_detect(suppress_status, "suppress"),
                           "--",
                           value)) %>%
    select(label, risk_cat, value, bp_cat_meds_included, outcome) %>%
    pivot_wider(names_from = bp_cat_meds_included,
                values_from = value) %>%
    mutate(label = paste(label, outcome, sep = ' - ')) %>%
    select(-outcome)

  out


}
