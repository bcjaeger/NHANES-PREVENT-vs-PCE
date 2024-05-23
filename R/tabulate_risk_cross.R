
# tar_load(nhanes_design_overall)
# nhanes_design = nhanes_design_overall

tabulate_risk_cross <- function(nhanes_design) {

  variables_prevent <- c(
    "PREVENT base equation" = "ascvd_prevent_base_bnry_10",
    "PREVENT base equation" = "cvd_prevent_base_bnry_10",
    "PREVENT base equation" = "cvd_prevent_base_bnry_15",
    "PREVENT full equation" = "ascvd_prevent_full_bnry_10",
    "PREVENT full equation" = "cvd_prevent_full_bnry_10",
    "PREVENT full equation" = "cvd_prevent_full_bnry_15"
  )

  variables_pce <- c(
    "Pooled cohort equations" = "ascvd_pce_bnry_10",
    "Pooled cohort equations" = "ascvd_pce_bnry_10",
    "Pooled cohort equations" = "ascvd_pce_bnry_10",
    "Pooled cohort equations" = "ascvd_pce_bnry_10",
    "Pooled cohort equations" = "ascvd_pce_bnry_10",
    "Pooled cohort equations" = "ascvd_pce_bnry_10"
  )

  results_overall <- map2(
    .x = variables_prevent,
    .y = variables_pce,
    .f = ~ {

      .x_lvls <- levels(nhanes_design$variables[[.x]])
      .y_lvls <- levels(nhanes_design$variables[[.y]])

      result <- list()

      for(i in .x_lvls){

        for(j in .y_lvls){

          nhanes_design$variables$tmp <- as.numeric(
            nhanes_design$variables[[.x]] == i &
              nhanes_design$variables[[.y]] == j
          )

          .counts_unwtd <- nhanes_design$variables %>%
            count(tmp, bp_cat_meds_included, .drop = FALSE) %>%
            filter(tmp == 1) %>%
            select(-tmp) %>%
            rename(n_unwtd = n)

          .name <- paste(i, j, sep = '...')

          result[[.name]] <- svyby(formula = ~tmp,
                                   by = ~bp_cat_meds_included,
                                   FUN = svyciprop,
                                   drop.empty.groups = FALSE,
                                   design = nhanes_design) %>%
            bind_confint() %>%
            mutate(prevent_var = i,
                   pce_var = j) %>%
            rename(estimate = tmp,
                   se = `se.as.numeric(tmp)`) %>%
            left_join(.counts_unwtd)

        }

      }

      p <- bind_rows(result) %>%
        mutate(outcome = str_extract(.x, pattern = "^ascvd|^cvd")) %>%
        select(pce_var, prevent_var, bp_cat_meds_included,
               estimate, se, n_unwtd, ci_lower, ci_upper) %>%
        mutate(outcome = str_extract(.x, pattern = "^ascvd|^cvd"),
               suppress_status = suppress_catg(estimate = estimate,
                                               std_error = se,
                                               ci_lower = ci_lower,
                                               ci_upper = ci_upper,
                                               design = nhanes_design))

      #
      # p <- svytable(f, design = nhanes_design) %>%
      #   as_tibble() %>%
      #   rename(left_var = !!as.character(.y),
      #          right_var = !!as.character(.x)) %>%
      #   group_by(bp_cat_meds_included) %>%
      #   mutate(p = n / sum(n)) %>%
      #   transmute(left_var,
      #             right_var,
      #             bp_cat_meds_included,
      #             value = table_glue("{p * 100}"),
      #             outcome = str_extract(.x, pattern = "^ascvd|^cvd"))

      f <- as.formula(
        paste("~", .x, "+", .y, "+ bp_cat_meds_included")
      )

      m <- svyby(formula = ~ cvd_prevent_30_base + ascvd_prevent_30_base,
                 by = f,
                 design = nhanes_design,
                 FUN = svymean,
                 na.rm = TRUE)

      m_est <- as_tibble(m) %>%
        rename(pce_var = !!as.character(.y),
               prevent_var = !!as.character(.x)) %>%
        select(-starts_with("se")) %>%
        pivot_longer(cols = -c(pce_var, prevent_var, bp_cat_meds_included),
                     names_to = 'variable',
                     values_to = 'est') %>%
        mutate(outcome = str_extract(.x, pattern = "^ascvd|^cvd"))

      m_ci <- confint(m) %>%
        as_tibble(rownames = 'term') %>%
        separate(term, into = c("term", "variable"), sep = "\\:") %>%
        separate(term, into = c('prevent_var',
                                'pce_var',
                                'bp_cat_meds_included'),
                 sep = '\\.') %>%
        rename(ci_lwr = `2.5 %`,
               ci_upr = `97.5 %`)

      m_se <- SE(m) %>%
        as_tibble(rownames = 'term') %>%
        separate(term, into = c('prevent_var',
                                'pce_var',
                                'bp_cat_meds_included'),
                 sep = '\\.') %>%
        pivot_longer(cols = starts_with("se"),
                     names_prefix = 'se\\.',
                     names_to = 'variable',
                     values_to = 'se')

      m <- m_est %>%
        left_join(m_ci,
                  by = c("prevent_var",
                         "pce_var",
                         "bp_cat_meds_included",
                         "variable")) %>%
        left_join(m_se,
                  by = c("prevent_var",
                         "pce_var",
                         "bp_cat_meds_included",
                         "variable")) %>%
        mutate(suppress_status = suppress_ctns(estimate = est,
                                               std_error = se,
                                               design = nhanes_design))

      list(proportions = p, means = m)

    }) %>%
    enframe(name = 'label') %>%
    unnest_wider(value)

  proportion_tbl_overall <- results_overall %>%
    select(label, proportions) %>%
    unnest(proportions) %>%
    mutate(
      outcome = recode(outcome,
                       "ascvd" = "ASCVD",
                       "cvd" = "Total CVD"),
      value = case_when(
        str_detect(suppress_status, 'suppress') ~ "suppress",
        n_unwtd == 0 ~ "--",
        estimate < 0.001 ~ "< 0.1",
        TRUE ~ table_glue("{estimate * 100}")
      )
    ) %>%
    select(label,
           outcome,
           pce_var,
           prevent_var,
           bp_cat_meds_included,
           value) %>%
    pivot_wider(names_from = bp_cat_meds_included,
                values_from = value) %>%
    mutate(label = paste(label, outcome, sep = ' - ')) %>%
    select(-outcome) %>%
    # mutate(prevent_var = factor(prevent_var,
    #                             levels = c("< 10%",
    #                                        "≥ 10%",
    #                                        "< 15%",
    #                                        "≥ 15%"))) %>%
    mutate(label2 = if_else(str_detect(prevent_var, '15'),
                            '15', '10')) %>%
    arrange(label, label2, pce_var) %>%
    select(-label2)

  means_tbl_overall <- results_overall %>%
    select(label, means) %>%
    unnest(means) %>%
    mutate(
      m_risk = str_extract(variable, pattern = "^ascvd|^cvd"),
      m_risk = recode(m_risk,
                      "ascvd" = "30-year ASCVD risk",
                      "cvd" = "30-year total CVD risk"),
      value = if_else(
        str_detect(suppress_status, 'suppress'),
        true = '--',
        false = table_glue("{100*est}\n({100*ci_lwr}, {100*ci_upr})")
      ),
      outcome = recode(outcome,
                       "ascvd" = "ASCVD",
                       "cvd" = "Total CVD")
    ) %>%
    select(-est, -se, -ci_lwr, -ci_upr, -variable, -suppress_status) %>%
    pivot_wider(names_from = bp_cat_meds_included,
                values_from = value) %>%
    mutate(
      label = paste(label, outcome, sep = ' - '),
      # label = paste(label, m_risk, sep = '; ')
    ) %>%
    filter(m_risk == "30-year total CVD risk") %>%
    select(-outcome, -m_risk) %>%
    mutate(across(where(is.character), ~ {
      .x[is.na(.x)] <- '--'
      .x
    }))

  result <- list(overall = proportion_tbl_overall,
                 overall_30 = means_tbl_overall,
                 by_age = NULL)

  if(get_design_subgroup(nhanes_design) %in% c("age_lt60")){
    return( result )
  }

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

      counts_unwtd <- nhanes_design$variables %>%
        count(.data[[.x]],
              .data[[.y]],
              age_cat,
              bp_cat_meds_included,
              .drop = FALSE) %>%
        rename(n_unwtd = n)

      svytable(f, design = nhanes_design) %>%
        as_tibble() %>%
        left_join(counts_unwtd) %>%
        rename(left_var = !!as.character(.y),
               right_var = !!as.character(.x)) %>%
        group_by(bp_cat_meds_included, age_cat) %>%
        mutate(p = n / sum(n)) %>%
        transmute(left_var,
                  right_var,
                  bp_cat_meds_included,
                  age_cat,
                  value = case_when(
                    n_unwtd == 0 ~ "--",
                    p < 0.001 ~ "< 0.1",
                    TRUE ~ table_glue("{p * 100}")
                  ),
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
    select(-outcome, -age_cat) %>%
    rename(pce_var = left_var,
           prevent_var = right_var)

  result$by_age <- results_by_age

  result

}
