
# tar_load(nhanes_design_overall)
# nhanes_design <- nhanes_design_overall

tabulate_highrisk <- function(nhanes_design) {

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

  if(get_design_subgroup(nhanes_design) == "age_lt60"){

    drop <- str_which(variables, "60$")
    variables <- variables[-drop]

  }

  control <- c("demo_age_years", "demo_gender")

  nhanes_design_subpop <-
    subset(nhanes_design, bp_cat_meds_included == "130-139/80-89")

  rspec_prs <- round_spec() %>%
    round_using_magnitude(digits = c(2, 1, 0),
                          breaks = c(10, 100, Inf))


  results <- expand_grid(
    outcome = c("cvd_prevent_base_bnry_10",
                "cvd_prevent_30_base_bnry_30"),
    exposure = variables
  ) %>%
    mutate(
      proprs = map2(
        .x = exposure,
        .y = outcome,
        .f = ~ svy_prop_pr(design = nhanes_design_subpop,
                           outcome = .y,
                           exposure = .x,
                           control = control)
      )
    )

  recoder <- names(variables) %>%
    set_names(variables) %>%
    c(cvd_prevent_base_bnry_10 = "pv_10",
      cvd_prevent_30_base_bnry_30 = "pv_30")

  unnest(results, proprs) %>%
    filter(!str_detect(outcome_level, "<")) %>%
    select(-outcome_level) %>%
    filter(exposure_level != "Other") %>%
    mutate(exposure = recode(exposure, !!!recoder),
           outcome = recode(outcome, !!!recoder))

}


