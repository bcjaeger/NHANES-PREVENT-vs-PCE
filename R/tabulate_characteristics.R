
# tar_load(nhanes_design_overall)
# nhanes_design <- nhanes_design_overall

tabulate_characteristics <- function(nhanes_design) {

  variables <- c(
    "Age, years" = "demo_age_years",
    "Male gender, %" = "demo_gender",
    "Race/ethnicity, %" = "demo_race",
    "Cigarette smoking, %" = "cc_smoke_current",
    "Total cholesterol, mg/dL" = "chol_total",
    "HDL cholesterol, mg/dL" = "chol_hdl",
    "Non-HDL cholesterol, mg/dL" = "chol_non_hdl",
    "Statin use, %" = "chol_med_statin",
    "HbA1c, %" = "cc_hba1c",
    "Diabetes, %" = "cc_diabetes",
    "eGFR, ml/min/1.73 m2" = "cc_egfr",
    # "eGFR < 60 ml/min/1.73 m2, %" = "cc_egfr_lt60",
    "Albumin-to-creatinine ratio, mg/g" = "cc_acr",
    # "Albumin-to-creatinine ratio > 30 mg/g" = "cc_acr_gteq30",
    "Chronic kidney disease, %" = "cc_ckd",
    "Systolic blood pressure, mm Hg" = "bp_sys_mean",
    "Diastolic blood pressure, mm Hg" = "bp_dia_mean"
  )

  if(sum(nhanes_design$variables$cc_diabetes == "Yes", na.rm = T) == 0){
    variables <- variables[-which(variables == 'cc_diabetes')]
  }

  if(sum(nhanes_design$variables$cc_ckd == "Yes", na.rm = T) == 0){
    variables <- variables[-which(variables == 'cc_ckd')]
  }

  tb1_bpcat <- variables %>%
    map(tb1_fun,
        by = 'bp_cat_meds_included',
        design = nhanes_design) %>%
    bind_rows(.id = 'label') %>%
    mutate(label = factor(label, levels = names(variables))) %>%
    arrange(label)

  # tb1_bpcat_discrep <- variables %>%
  #   map(tb1_fun,
  #       by = c('bp_cat_meds_included', 'discrep_cvd_base_15'),
  #       design = nhanes_design) %>%
  #   bind_rows(.id = 'label') %>%
  #   mutate(label = factor(label, levels = names(variables))) %>%
  #   arrange(label)

  tb1_counts_wtd <- svytable(~bp_cat_meds_included, nhanes_design) %>%
    enframe() %>%
    transmute(
      label = "Number of US Adults, millions",
      value = table_value(value / 1e6),
      bp_cat_meds_included = name,
      level = 'counts',
      ctns = TRUE
    )

  tb1_counts_raw <- nhanes_design %>%
    getElement('variables') %>%
    count(bp_cat_meds_included) %>%
    mutate(
      n = table_glue("{bp_cat_meds_included}\n(n = {n})")
    ) %>%
    deframe()

  bp_props <- list(
    svyciprop(~I(bp_cat_meds_included == "<120/80"), nhanes_design),
    svyciprop(~I(bp_cat_meds_included == "120-129/<80"), nhanes_design),
    svyciprop(~I(bp_cat_meds_included == "130-139/80-89"), nhanes_design),
    svyciprop(~I(bp_cat_meds_included == "≥140/90"), nhanes_design),
    svyciprop(~I(bp_cat_meds_included == "Taking antihypertensive medication"), nhanes_design)
  ) %>%
    set_names(levels(nhanes_design$variables$bp_cat_meds_included)) %>%
    map(bind_confint) %>%
    map(mutate, x = as.numeric(x)) %>%
    bind_rows(.id = 'bp_cat_meds_included') %>%
    mutate(across(c(x, ci_lower, ci_upper), ~.x*100)) %>%
    transmute(label = "% (95% CI) of US population",
              bp_cat_meds_included,
              level = "bp_prop",
              value = table_glue("{x}\n({ci_lower}, {ci_upper})"),
              ctns = TRUE)

  tb1_bpcat_out <- tb1_bpcat %>%
    mutate(bp_cat_meds_included = recode(bp_cat_meds_included,
                                         !!!tb1_counts_raw)) %>%
    mutate(value = if_else(is_suppressed(suppress_status),
                           true = "--",
                           false = value)) %>%
    select(label, bp_cat_meds_included, ctns, level, value) %>%
    pivot_wider(names_from = bp_cat_meds_included,
                values_from = value) %>%
    add_count(label) %>%
    split(.$label) %>%
    map_dfr(~if(nrow(.x)==2){.x[-1, ]} else {.x}) %>%
    mutate(label = factor(label,
                          levels = c("Number of US Adults, millions",
                                     "% (95% CI) of US population",
                                     names(variables)))) %>%
    arrange(label) %>%
    mutate(
      label = as.character(label),
      level = if_else(ctns | n == 2, label, level),
      label = if_else(n > 2, label, NA_character_)
    ) %>%
    select(-ctns, -n)

  # tb1_bpcat_discrep_out <- tb1_bpcat_discrep %>%
  #   filter(discrep_cvd_base_15 %in% c("≥ 10%.< 15%", "≥ 10%.≥ 15%")) %>%
  #   mutate(
  #     bp_cat_meds_included = recode(
  #       bp_cat_meds_included,
  #       "<120/80" = 'lt_120',
  #       "120-129/<80" = 'lt_130',
  #       "130-139/80-89" = 'lt_140',
  #       '≥140/90' = 'gteq_140',
  #       "Taking antihypertensive medication" = 'meds'
  #     ),
  #     discrep_cvd_base_15 = recode(
  #       discrep_cvd_base_15,
  #       "≥ 10%.< 15%" = 'hi_lo',
  #       "≥ 10%.≥ 15%" = 'hi_hi'
  #     )
  #   ) %>%
  #   mutate(value = if_else(str_detect(suppress_status, "suppress"),
  #                          "--",
  #                          value)) %>%
  #   select(-mn, -se, -ci_lower, -ci_upper, -suppress_status) %>%
  #   pivot_wider(names_from = c(discrep_cvd_base_15, bp_cat_meds_included),
  #               values_from = value) %>%
  #   add_count(label) %>%
  #   split(.$label) %>%
  #   map_dfr(~if(nrow(.x)==2){.x[-1, ]} else {.x}) %>%
  #   mutate(label = factor(label,
  #                         levels = c("% (95% CI) of US population",
  #                                    names(variables)))) %>%
  #   arrange(label) %>%
  #   mutate(
  #     label = as.character(label),
  #     level = if_else(ctns | tolower(level) == 'yes', label, level),
  #     label = if_else(n > 2, label, NA_character_)
  #   ) %>%
  #   select(-ctns, -n) %>%
  #   select(label, level,
  #          ends_with("lt_120"),
  #          ends_with("lt_130"),
  #          ends_with("lt_140"),
  #          ends_with("gteq_140"),
  #          ends_with("meds"))

  list(bpcat = tb1_bpcat_out)
  # bpcat_discrep = tb1_bpcat_discrep_out


}

bind_confint <- function(x){

  ci <- confint(x) %>%
    set_colnames(c("ci_lower", "ci_upper"))

  as_tibble(x) %>%
    bind_cols(ci)

}

tb1_fun <- function(.x, design, by = NULL, prop_confint = FALSE){

  do_by <- !is.null(by)

  by_collapse <- paste(by, collapse = ' + ')

  if(do_by){
    by_formula <- as.formula(glue("~ {by_collapse}"))
  } else {
    by_formula <- NULL
  }

  if(is.numeric(design$variables[[.x]])){

    formula <- as.formula(glue("~ {.x}"))

    if(.x == 'cc_acr'){

      init <- if(do_by){

        svyby(formula, by = by_formula,
              design = design,
              FUN = svyquantile,
              quantiles = 1/2,
              na.rm = TRUE) %>%
          bind_confint() %>%
          mutate(level = .x) %>%
          rename_at(.vars = .x, .funs = ~ 'estimate') %>%
          rename_at(.vars = paste("se", .x, sep = '.'), .funs = ~'std_error')

      } else {

        svyquantile(x = formula,
                    design = design,
                    quantiles = 1/2,
                    na.rm = TRUE) %>%
          getElement(.x) %>%
          as_tibble() %>%
          rename(estimate = quantile, ci_lower = ci.2.5, ci_upper = ci.97.5) %>%
          mutate(level = .x)

      }

    } else {

      init <- if(do_by){

        svyby(formula, by = by_formula,
              design = design,
              FUN = svymean,
              na.rm = TRUE) %>%
          bind_confint() %>%
          mutate(level = .x) %>%
          rename_at(.vars = .x, .funs = ~ 'estimate') %>%
          rename(std_error = se)

      } else {

        svymean(x = formula,
                design = design,
                na.rm = TRUE) %>%
          bind_confint() %>%
          rename_at(.vars = .x, .funs = ~ 'std_error') %>%
          mutate(level = .x) %>%
          rename(estimate = mean)

      }

    }


    if(as.character(.x) == 'bp_dia_mean'){

      init %>%
        mutate(
          value = table_glue("{estimate}\n({ci_lower}, {ci_upper})",
                             rspec = round_spec() %>%
                               round_using_decimal(digits = 0)),
          suppress_status = suppress_ctns(estimate = estimate,
                                          std_error = std_error,
                                          design = design),
          ctns = TRUE
        ) %>%
        ungroup() %>%
        mutate_if(is.factor, as.character)

    } else {

      init %>%
        mutate(
          value = table_glue("{estimate}\n({ci_lower}, {ci_upper})"),
          suppress_status = suppress_ctns(estimate = estimate,
                                          std_error = std_error,
                                          design = design),
          ctns = TRUE
        ) %>%
        ungroup() %>%
        mutate_if(is.factor, as.character)

    }



  } else {

    out <- svy_ciprop(variable = .x, by = by, design = design) %>%
      mutate(ctns = FALSE,
             value = table_glue("{100 * estimate}")) %>%
      rename(level = !!.x)

    if(prop_confint){
      out <- out %>%
        mutate(
          value = table_glue(
            "{100 * estimate}\n({100 * ci_lower}, {100 * ci_upper})"
          )
        )
    }

    out

  }

}
