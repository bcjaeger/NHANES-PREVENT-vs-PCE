
# tar_load(nhanes_design_overall)
# nhanes_design <- nhanes_design_overall

tabulate_discrepant <- function(nhanes_design,
                                prop_confint = FALSE,
                                suppress = TRUE) {

  variables <- c(
    "Age, years" = "demo_age_years",
    "Men" = "demo_gender",
    "Race/ethnicity, %" = "demo_race",
    "Cigarette smoking, %" = "cc_smoke_current",
    # "Total cholesterol, mg/dL" = "chol_total",
    # "HDL cholesterol, mg/dL" = "chol_hdl",
    "Non-HDL cholesterol, mg/dL" = "chol_non_hdl",
    "Statin use, %" = "chol_med_statin",
    # "HbA1c, %" = "cc_hba1c",
    "Diabetes, %" = "cc_diabetes",
    # "eGFR, ml/min/1.73 m2" = "cc_egfr",
    # "eGFR < 60 ml/min/1.73 m2, %" = "cc_egfr_lt60",
    # "ACR, log mg/g" = "ln_acr",
    # "ACR \u2265 30 mg/g" = "cc_acr_gteq30",
    "Chronic kidney disease, %" = "cc_ckd",
    "Diabetes or CKD, %" = "cc_ckd_or_diab",
    # "Diabetes or CKD or Age>=65, %" = "cc_ckd_or_diab_or_age",
    "SBP, mm Hg" = "bp_sys_mean",
    "DBP, mm Hg" = "bp_dia_mean"
  )


  if(get_design_subgroup(nhanes_design) == "comorb"){

    drop <- str_which(variables, "diabetes$|ckd$|lt60$|gteq30$|diab$|or_age$")
    variables <- variables[-drop]

  }

  # if(get_design_subgroup(nhanes_design) == "age_lt60"){
  #
  #   drop <- str_which(variables, "lt60")
  #   variables <- variables[-drop]
  #
  # }

  result <- list()

  rspec_prs <- round_spec() %>%
    round_using_magnitude(digits = c(2, 1, 0), breaks = c(10, 100, Inf))

  for(bp_level in levels(nhanes_design$variables$bp_cat_meds_included)){

    nhanes_design_subpop <- nhanes_design %>%
      subset(bp_cat_meds_included == bp_level &
               discrep_cvd_base_15 %in% c("≥ 10%.< 15%", "≥ 10%.≥ 15%"))

    nhanes_design_subpop$variables <- nhanes_design_subpop$variables %>%
      mutate(
        ln_acr = log(cc_acr),
        # for table characteristics
        discrep_cvd_base_15 = factor(
          discrep_cvd_base_15,
          levels = c("≥ 10%.< 15%", "≥ 10%.≥ 15%"),
          labels = c("PCE ≥ 10% Prevent <15%", "PCE ≥ 10% Prevent ≥15%")
        ),
        # for table prevalence ratios
        discrep = as.numeric(discrep_cvd_base_15) - 1L
      )

    # characteristics
    tbl_chars_discrepancy <- variables %>%
      map(tb1_fun,
          design = nhanes_design_subpop,
          by = 'discrep_cvd_base_15',
          prop_confint = prop_confint) %>%
      bind_rows(.id = 'label') %>%
      bind_rows() %>%
      mutate(label = factor(label, levels = names(variables)),
             value = if_else(is_suppressed(suppress_status),
                             true = if(suppress) "--" else value,
                             false = value)) %>%
      arrange(label) %>%
      mutate(variable = recode(label, !!!variables)) %>%
      select(label, variable, level, value, ctns, discrep_cvd_base_15) %>%
      pivot_wider(names_from = discrep_cvd_base_15,
                  values_from = value)

    nhanes_design_subpop$variables <- nhanes_design_subpop$variables %>%
      mutate(demo_age_years = demo_age_years / 5,
             chol_total = chol_total / 40,
             chol_hdl = chol_hdl / 20,
             chol_non_hdl = chol_non_hdl / 20,
             cc_egfr = cc_egfr / 5,
             bp_sys_mean = bp_sys_mean / 10,
             bp_dia_mean = bp_dia_mean / 5)

    pr_adj <- variables %>%
      map_dfr(.f = ~ {

        rhs <- unique(c(.x, 'demo_age_years', 'demo_gender')) %>%
          paste(collapse = ' + ')

        f <- as.formula(glue("discrep ~ {rhs}"))

        out <- svyglm(f, family = quasipoisson(), design = nhanes_design_subpop) %>%
          tidy(conf.int = TRUE, exponentiate = TRUE) %>%
          filter(str_detect(term, .x)) %>%
          mutate(variable = .x,
                 level = .x)

        if(is.factor(nhanes_design_subpop$variables[[.x]]) ||
           is.character(nhanes_design_subpop$variables[[.x]])){
          out <- out %>%
            mutate(variable = str_extract(term, .x),
                   level = str_remove(term, .x))
        }

        out

      },
      .id = 'label') %>%
      transmute(label, variable, level,
                pr_adj = table_glue("{estimate}\n({conf.low}, {conf.high})",
                                    rspec = rspec_prs))

    pr_unadj <- variables %>%
      map_dfr(.f = ~ {

        f <- as.formula(glue("discrep ~ {.x}"))

        out <- svyglm(f, family = quasipoisson(), design = nhanes_design_subpop) %>%
          broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
          filter(str_detect(term, .x)) %>%
          mutate(variable = .x,
                 level = .x)

        if(is.factor(nhanes_design_subpop$variables[[.x]]) ||
           is.character(nhanes_design_subpop$variables[[.x]])){
          out <- out %>%
            mutate(variable = str_extract(term, .x),
                   level = str_remove(term, .x))
        }

        out

      },
      .id = 'label') %>%
      transmute(label, variable, level,
                pr_unadj = table_glue("{estimate}\n({conf.low} {conf.high})",
                                      rspec = rspec_prs))


    result[[bp_level]] <- tbl_chars_discrepancy %>%
      left_join(pr_unadj) %>%
      left_join(pr_adj) %>%
      ungroup() %>%
      split(.$label) %>%
      map_dfr(
        .f = ~ {

          if(nrow(.x) == 1) return(.x)
          if(nrow(.x) == 2) return(.x[2, ])

          .x$pr_unadj[1] <- "1 (ref)"
          .x$pr_adj[1] <- "1 (ref)"
          .x

        },
        .id = 'label'
      ) %>%
      mutate(label = factor(label, levels = names(variables))) %>%
      add_count(label) %>%
      arrange(label) %>%
      mutate(
        label = as.character(label),
        level = if_else(ctns | tolower(level) == 'yes', label, level),
        label = if_else(n > 2, label, NA_character_)
      ) %>%
      select(-ctns, -n, -variable)


  }

  bind_rows(result, .id = 'bp_cat_meds_included') %>%
    filter(level != "Other") %>%
    mutate(
      across(
        .cols = starts_with("pr_"),
        .fns = ~ if_else(
          `PCE ≥ 10% Prevent <15%` == "--" |
          `PCE ≥ 10% Prevent ≥15%` == "--",
          true = "--",
          false = .x
        )
      )
    )

}


