
svy_mean_diff <- function(design, nhanes_derived, outcome, exposure, control){

  # take exposure out of control if needed.
  control <- setdiff(control, exposure)

  if(exposure == 'demo_age_gteq_60' && 'demo_age_years' %in% control){
    control <- setdiff(control, 'demo_age_years')
  }

  .control <- paste(control, collapse = ' + ')

  mdl_formula <- as.formula(
    glue("{outcome} ~ {exposure} + {.control}")
  )

  mns_formula <- as.formula(
    paste("~", outcome)
  )

  by_formula <- as.formula(
    paste("~", exposure)
  )

  mns <- svyby(formula = mns_formula,
               by = by_formula,
               design = design,
               FUN = svymean) %>%
    bind_confint() %>%
    rename(level = {{exposure}},
           estimate = {{outcome}},
           std_error = se) %>%
    mutate(suppress_status = suppress_ctns(estimate = estimate,
                                           std_error = std_error,
                                           design = design))

  mdl_fit <- try(svyglm(mdl_formula, design), silent = TRUE)

  if(!inherits(mdl_fit, 'try-error')){

    ref_level <- levels(design$variables[[exposure]])[1]
    nonref_levels <- levels(design$variables[[exposure]])[-1]

    mdl_diff <- mdl_fit %>%
      emmeans(specs = exposure) %>%
      pairs(reverse = TRUE) %>%
      confint() %>%
      as_tibble() %>%
      filter(
        str_detect(contrast, paste0(" - \\(", ref_level, "\\)$")) |
          str_detect(contrast, paste0(ref_level, "$"))
      ) %>%
      mutate(level = str_remove(contrast, "- .*"),
             level = str_extract(level, nonref_levels)) %>%
      transmute(level,
                estimate_diff = estimate,
                ci_lower_diff = lower.CL,
                ci_upper_diff = upper.CL) %>%
      add_row(level = ref_level,
              estimate_diff = 0,
              ci_lower_diff = 0,
              ci_upper_diff = 0,
              .before = 1)

    return(left_join(mns, mdl_diff, by = 'level'))

  }

  mns

}
