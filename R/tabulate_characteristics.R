#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nhanes_design

tabulate_characteristics <- function(nhanes_design) {

  variables = c(
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
    "eGFR < 60 ml/min/1.73 m2, %" = "cc_egfr_lt60",
    "Albumin-to-creatinine ratio, mg/g" = "cc_acr",
    "Albumin-to-creatinine ratio > 30 mg/g" = "cc_acr_gteq30",
    "Systolic blood pressure, mm Hg" = "bp_sys_mean",
    "Diastolic blood pressure, mm Hg" = "bp_dia_mean"
  )

  # not used at the moment
  # tb1_ovrl <- variables %>%
  #   map(tb1_fun,  design = nhanes_design) %>%
  #   bind_rows(.id = 'label') %>%
  #   split(.$label) %>%
  #   map(~ {
  #     if(nrow(.x) == 2){
  #       return(.x[2,])
  #     } else {
  #       return(.x)
  #     }
  #   }) %>%
  #   .[names(variables)] %>%
  #   bind_rows()

  tb1_bpcat <- variables %>%
    map(tb1_fun,
        by = 'bp_cat_meds_included',
        design = nhanes_design) %>%
    bind_rows(.id = 'label') %>%
    bind_rows() %>%
    mutate(label = factor(label, levels = names(variables))) %>%
    arrange(label)

  tb1_bpcat_discrep <- variables %>%
    map(tb1_fun,
        by = c('bp_cat_meds_included', 'discrep_cvd_base_15'),
        design = nhanes_design) %>%
    bind_rows(.id = 'label') %>%
    bind_rows() %>%
    mutate(label = factor(label, levels = names(variables))) %>%
    arrange(label)

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

  tb1_bpcat_out <- bind_rows(bp_props, tb1_counts_wtd, tb1_bpcat) %>%
    mutate(bp_cat_meds_included = recode(bp_cat_meds_included,
                                         !!!tb1_counts_raw)) %>%
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

  tb1_bpcat_discrep_out <- tb1_bpcat_discrep %>%
    filter(discrep_cvd_base_15 %in% c("≥ 10%.< 15%", "≥ 10%.≥ 15%")) %>%
    mutate(
      bp_cat_meds_included = recode(
        bp_cat_meds_included,
        "<120/80" = 'lt_120',
        "120-129/<80" = 'lt_130',
        "130-139/80-89" = 'lt_140',
        '≥140/90' = 'gteq_140',
        "Taking antihypertensive medication" = 'meds'
      ),
      discrep_cvd_base_15 = recode(
        discrep_cvd_base_15,
        "≥ 10%.< 15%" = 'hi_lo',
        "≥ 10%.≥ 15%" = 'hi_hi'
      )
    ) %>%
    pivot_wider(names_from = c(discrep_cvd_base_15, bp_cat_meds_included),
                values_from = value) %>%
    add_count(label) %>%
    split(.$label) %>%
    map_dfr(~if(nrow(.x)==2){.x[-1, ]} else {.x}) %>%
    mutate(label = factor(label,
                          levels = c("% (95% CI) of US population",
                                     names(variables)))) %>%
    arrange(label) %>%
    mutate(
      label = as.character(label),
      level = if_else(ctns | tolower(level) == 'yes', label, level),
      label = if_else(n > 2, label, NA_character_)
    ) %>%
    select(-ctns, -n) %>%
    select(label, level,
           ends_with("lt_120"),
           ends_with("lt_130"),
           ends_with("lt_140"),
           ends_with("gteq_140"),
           ends_with("meds"))

  list(bpcat = tb1_bpcat_out,
       bpcat_discrep = tb1_bpcat_discrep_out)


}

bind_confint <- function(x){

  ci <- confint(x) %>%
    set_colnames(c("ci_lower", "ci_upper"))

  as_tibble(x) %>%
    bind_cols(ci)

}

tb1_fun <- function(.x, by = NULL, design){

  do_by <- !is.null(by)

  by_collapse <- paste(by, collapse = ' + ')

  if(do_by){
    by_formula <- as.formula(glue("~ {by_collapse}"))
  } else {
    by_formula <- NULL
  }

  if(is.numeric(design$variables[[.x]])){

    formula <- as.formula(glue("~ {.x}"))

    init <- if(do_by){

      svyby(formula, by = by_formula,
            design = design,
            FUN = svymean,
            na.rm = TRUE) %>%
        bind_confint() %>%
        mutate(level = .x) %>%
        rename_at(.vars = .x, .funs = ~ 'mn')

    } else {

      svymean(x = formula,
              design = design,
              na.rm = TRUE) %>%
        bind_confint() %>%
        rename_at(.vars = .x, .funs = ~ 'se') %>%
        mutate(level = .x) %>%
        rename(mn = mean)

    }

    init %>%
      mutate(
        value = table_glue("{mn}\n({ci_lower}, {ci_upper})"),
        ctns = TRUE
      ) %>%
      select(-mn, -se, -starts_with("ci")) %>%
      ungroup() %>%
      mutate_if(is.factor, as.character)

  } else {

    formula <- if(do_by){
      as.formula(glue('~ {.x} + {by_collapse}'))
    } else {
      as.formula(glue("~ {.x}"))
    }

    init <- svytable(formula = formula, design = design) %>%
      as_tibble()

    if(do_by) init <- init %>%
      group_by_at(.vars = by)

    init %>%
      mutate(
        value = n / sum(n),
        value = table_glue("{100 * value}"),
        ctns = FALSE
      ) %>%
      rename_at(.vars = .x, .funs = ~ 'level') %>%
      select(-n) %>%
      ungroup() %>%
      mutate_if(is.factor, as.character)

  }

}
