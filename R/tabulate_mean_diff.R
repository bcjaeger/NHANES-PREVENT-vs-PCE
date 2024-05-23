
# tar_load(nhanes_design_age_lt60)
# nhanes_design <- nhanes_design_age_lt60

tabulate_mean_diff <- function(nhanes_design,
                               nhanes_derived,
                               outcomes = c("ascvd_pce",
                                            "ascvd_prevent_base",
                                            "cvd_prevent_base",
                                            "cvd_prevent_30_base")) {

  variables <- c(
    "Age ≥ 60 years" = "demo_age_gteq_60",
    "Sex" = "demo_gender",
    "Race/ethnicity" = "demo_race",
    "Cigarette smoking" = "cc_smoke_current",
    "Non-HDL cholesterol ≥ 130 mg/dL" = "chol_non_hdl_gteq_130",
    "Statin use" = "chol_med_statin",
    "Diabetes" = "cc_diabetes",
    "Chronic kidney disease" = "cc_ckd"
  )


  if(get_design_subgroup(nhanes_design) == "comorb"){

    drop <- str_which(variables, "diabetes$|ckd$|lt60$|gteq30$|diab$|or_age$")
    variables <- variables[-drop]

  }

  # if(get_design_subgroup(nhanes_design) == "age_lt60"){
  #
  #   drop <- str_which(variables, "60$")
  #   variables <- variables[-drop]
  #
  # }

  control <- c("demo_age_years", "demo_gender")

  bp_lvls <- levels(nhanes_derived$bp_cat_meds_included)

  results <- vector(mode = 'list', length = length(bp_lvls))

  names(results) <- bp_lvls

  for(bp_group in bp_lvls){

    nhanes_design_subpop <-
      subset(nhanes_design, bp_cat_meds_included == bp_group)

    result <- expand_grid(
      outcome = outcomes,
      exposure = variables
    ) %>%
      mutate(
        mdiffs = map2(
          .x = exposure,
          .y = outcome,
          .f = ~ svy_mean_diff(design = nhanes_design_subpop,
                               nhanes_derived = nhanes_derived,
                               outcome = .y,
                               exposure = .x,
                               control = control)
        )
      )

    recoder <- names(variables) %>%
      set_names(variables)

    results[[bp_group]] <- result %>%
      mutate(label = recode(exposure, !!!recoder), .before = exposure) %>%
      rename(variable = exposure) %>%
      unnest(mdiffs)

  }

  results

}
