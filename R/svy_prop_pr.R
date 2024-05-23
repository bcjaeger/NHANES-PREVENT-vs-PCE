
svy_prop_pr <- function(design, outcome, exposure, control){

  props <- svy_ciprop(variable = outcome,
                      by = exposure,
                      design = design) %>%
    rename(exposure_level = {{exposure}},
           outcome_level = {{outcome}}) %>%
    mutate(suppress_status = suppress_ctns(estimate = estimate,
                                           std_error = std_error,
                                           design = design))

  # take exposure out of control if needed.
  control <- setdiff(control, exposure)

  if(exposure == 'demo_age_gteq_60' && 'demo_age_years' %in% control){
    control <- setdiff(control, 'demo_age_years')
  }

  .control <- paste(control, collapse = ' + ')

  design$variables$tmp <- as.numeric(design$variables[[outcome]]) - 1L

  mdl_formula <- as.formula(
    glue("tmp ~ {exposure} + {.control}")
  )

  prs <- svyglm(mdl_formula,
         family = quasipoisson(),
         design = design) %>%
    tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    filter(str_detect(term, exposure)) %>%
    mutate(exposure = exposure,
           term = str_remove(term, exposure)) %>%
    transmute(exposure_level = term,
              pr_estimate = estimate,
              pr_std_error = std.error,
              pr_ci_lower = conf.low,
              pr_ci_upper = conf.high)

  left_join(props, prs)

}
