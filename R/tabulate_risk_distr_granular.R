
# tar_load(nhanes_design_overall)
# nhanes_design <- nhanes_design_overall

tabulate_risk_distr_granular <- function(nhanes_design) {

  variables <- c(
    "PREVENT base equation" = "ascvd_prevent_base_cat",
    "PREVENT base equation" = "cvd_prevent_base_cat"
  )

  bpcats <- levels(nhanes_design$variables$bp_cat_meds_included)

  .design <- subset(nhanes_design, ascvd_pce >= .10)


  .means_ascvd <- svyby(~ ascvd_prevent_base,
                        by = ~ bp_cat_meds_included,
                        design = subset(.design, ascvd_prevent_base < 0.10),
                        FUN = svymean) %>%
    bind_confint() %>%
    transmute(label = "PREVENT base equation - ASCVD",
              bp_cat_meds_included,
              tbl_value = table_glue("{ascvd_prevent_base * 100} \\
                                     ({ci_lower * 100}, \\
                                     {ci_upper * 100})"))

  .means_cvd <- svyby(~ cvd_prevent_base,
                      by = ~ bp_cat_meds_included,
                      design = subset(.design, cvd_prevent_base < 0.10),
                      FUN = svymean) %>%
    bind_confint() %>%
    transmute(label = "PREVENT base equation - CVD",
              bp_cat_meds_included,
              tbl_value = table_glue("{cvd_prevent_base * 100} \\
                                     ({ci_lower * 100}, \\
                                     {ci_upper * 100})"))

  .means <- bind_rows(.means_ascvd,
                      .means_cvd) %>%
    pivot_wider(names_from = bp_cat_meds_included,
                values_from = tbl_value)

  .design$variables <- .design$variables %>%
    mutate(
      ascvd_prevent_base_cat = case_when(
        ascvd_prevent_base < 0.05  ~ "< 5%",
        ascvd_prevent_base < 0.075 ~ "≥ 5% to <7.5%",
        ascvd_prevent_base < 0.10  ~ "≥ 7.5% to <10%",
        ascvd_prevent_base >= 0.10 ~ "≥ 10%",
        TRUE ~ NA_character_
      ),
      cvd_prevent_base_cat = case_when(
        cvd_prevent_base < 0.05  ~ "< 5%",
        cvd_prevent_base < 0.075 ~ "≥ 5% to <7.5%",
        cvd_prevent_base < 0.10  ~ "≥ 7.5% to <10%",
        cvd_prevent_base >= 0.10 ~ "≥ 10%",
        TRUE ~ NA_character_
      ),
      across(
        .cols = c(ascvd_prevent_base_cat, cvd_prevent_base_cat),
        .fns = ~ factor(.x, levels = c("< 5%",
                                       "≥ 5% to <7.5%",
                                       "≥ 7.5% to <10%",
                                       "≥ 10%"))
      )
    )

  # png(filename = 'tmp_hist.png', width = 12, height = 5, units = 'in', res = 300)
  # par(mfrow = c(1,2))
  # svyhist(~ascvd_prevent_base, .design)
  # svyhist(~cvd_prevent_base, .design)
  # dev.off()

  results <- map_dfr(
    variables,
    .f = ~ {

      lvls <- levels(.design$variables[[.x]])

      results <- list()

      for(i in lvls){

        .design$variables$tmp <-
          as.numeric(.design$variables[[.x]] == i)

        result <- list()

        for(j in bpcats){

          ..design <- subset(.design, bp_cat_meds_included == j)

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

      # svytable(f, design = .design) %>%
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

  out_nosupp <- results %>%
    mutate(value = table_glue("{estimate * 100}")) %>%
    select(label, risk_cat, value, bp_cat_meds_included, outcome) %>%
    pivot_wider(names_from = bp_cat_meds_included,
                values_from = value) %>%
    mutate(label = paste(label, outcome, sep = ' - ')) %>%
    select(-outcome)

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

  list(suppressed = out,
       no_suppress = out_nosupp,
       means = .means)


}
