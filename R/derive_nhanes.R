

derive_nhanes <- function(nhanes_excluded) {

  data <- nhanes_excluded$data

  exams <- unique(data$svy_year)

  race_levels <- list(white = "No", black = "Yes")
  sex_levels <- list(male = 'Men', female = 'Women')
  smoke_current_levels <- list(no = "No", yes = "Yes")
  diabetes_levels <- list(no = "No", yes = "Yes")
  bp_meds_levels <- list(no = "No", yes = "Yes")
  statin_meds_levels <- list(no = "No", yes = "Yes")

  data %>%
    mutate(
      chol_non_hdl = chol_total - chol_hdl,
      .after = chol_hdl
    ) %>%
    mutate(
      svy_weight_mec = svy_weight_mec / length(exams),
      bp_cat_meds_included = recode(
        bp_cat_meds_included,
        "SBP <120 and DBP <80 mm Hg" = "<120/80",
        "SBP of 120 to <130 and DBP < 80 mm Hg" = "120-129/<80",
        "SBP of 130 to <140 or DBP 80 to <90 mm Hg" = "130-139/80-89",
        "SBP of 140 to <160 or DBP 90 to <100 mm Hg" = "≥140/90",
        "SBP 160+ or DBP 100+ mm Hg" = "≥140/90",
        "taking antihypertensive medications" = "Taking antihypertensive medication"
      ),
      demo_gender = factor(demo_gender, levels = c("Women", "Men")),
      cc_smoke_current = factor(
        cc_smoke == "Current",
        levels = c(FALSE, TRUE),
        labels = c("No", "Yes")
      ),
      cc_sdi = NA,
      ascvd_pce = predict_10yr_ascvd_risk(
        age_years = demo_age_years,
        race = demo_race_black,
        sex = demo_gender,
        smoke_current = cc_smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = bp_sys_mean,
        bp_meds = bp_med_use,
        diabetes = cc_diabetes,
        equation_version = "Goff_2013",
        override_boundary_errors = TRUE,
        race_levels = race_levels,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),

      ascvd_pce_cat = cut(
        ascvd_pce,
        breaks = c(0, 0.05, 0.10, 0.15, 1),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("<5%", "5% to <10%", "10% to <15%", "≥ 15%")
      ),

      ascvd_pce_bnry_10 = factor(ascvd_pce >= 0.10,
                                 levels = c(FALSE, TRUE),
                                 labels = c("< 10%", "≥ 10%")),


      ascvd_prevent_base = predict_10yr_ascvd_risk(
        age_years = demo_age_years,
        sex = demo_gender,
        smoke_current = cc_smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = bp_sys_mean,
        bp_meds = bp_med_use,
        diabetes = cc_diabetes,
        statin_meds = chol_med_statin,
        egfr_mlminm2 = cc_egfr,
        bmi = cc_bmi,
        equation_version = "Khan_2023",
        prevent_type = 'base',
        override_boundary_errors = TRUE,
        race_levels = race_levels,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),

      ascvd_prevent_base_cat = cut(
        ascvd_prevent_base,
        breaks = c(0, 0.05, 0.10, 0.15, 1),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("<5%", "5% to <10%", "10% to <15%", "≥ 15%")
      ),

      ascvd_prevent_base_bnry_10 = factor(ascvd_prevent_base >= 0.10,
                                          levels = c(FALSE, TRUE),
                                          labels = c("< 10%", "≥ 10%")),

      ascvd_prevent_base_bnry_15 = factor(ascvd_prevent_base >= 0.15,
                                          levels = c(FALSE, TRUE),
                                          labels = c("< 15%", "≥ 15%")),

      ascvd_prevent_full = predict_10yr_ascvd_risk(
        age_years = demo_age_years,
        sex = demo_gender,
        smoke_current = cc_smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = bp_sys_mean,
        bp_meds = bp_med_use,
        diabetes = cc_diabetes,
        acr = cc_acr,
        hba1c = cc_hba1c,
        sdi = cc_sdi,
        statin_meds = chol_med_statin,
        egfr_mlminm2 = cc_egfr,
        bmi = cc_bmi,
        equation_version = "Khan_2023",
        prevent_type = 'full',
        override_boundary_errors = TRUE,
        race_levels = race_levels,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),

      ascvd_prevent_full_cat = cut(
        ascvd_prevent_full,
        breaks = c(0, 0.05, 0.10, 0.15, 1),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("<5%", "5% to <10%", "10% to <15%", "≥ 15%")
      ),

      ascvd_prevent_full_bnry_10 = factor(ascvd_prevent_full >= 0.10,
                                          levels = c(FALSE, TRUE),
                                          labels = c("< 10%", "≥ 10%")),

      ascvd_prevent_full_bnry_15 = factor(ascvd_prevent_full >= 0.15,
                                          levels = c(FALSE, TRUE),
                                          labels = c("< 15%", "≥ 15%")),

      cvd_prevent_base = predict_10yr_cvd_risk(
        age_years = demo_age_years,
        sex = demo_gender,
        smoke_current = cc_smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = bp_sys_mean,
        bp_meds = bp_med_use,
        diabetes = cc_diabetes,
        statin_meds = chol_med_statin,
        egfr_mlminm2 = cc_egfr,
        bmi = cc_bmi,
        equation_version = "Khan_2023",
        prevent_type = 'base',
        override_boundary_errors = TRUE,
        race_levels = race_levels,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),

      cvd_prevent_base_cat = cut(
        cvd_prevent_base,
        breaks = c(0, 0.05, 0.10, 0.15, 1),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("<5%", "5% to <10%", "10% to <15%", "≥ 15%")
      ),

      cvd_prevent_base_bnry_10 = factor(cvd_prevent_base >= 0.10,
                                        levels = c(FALSE, TRUE),
                                        labels = c("< 10%", "≥ 10%")),

      cvd_prevent_base_bnry_15 = factor(cvd_prevent_base >= 0.15,
                                        levels = c(FALSE, TRUE),
                                        labels = c("< 15%", "≥ 15%")),

      cvd_prevent_full = predict_10yr_cvd_risk(
        age_years = demo_age_years,
        race = demo_race_black,
        sex = demo_gender,
        smoke_current = cc_smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = bp_sys_mean,
        bp_meds = bp_med_use,
        diabetes = cc_diabetes,
        acr = cc_acr,
        hba1c = cc_hba1c,
        sdi = cc_sdi,
        statin_meds = chol_med_statin,
        egfr_mlminm2 = cc_egfr,
        bmi = cc_bmi,
        equation_version = "Khan_2023",
        prevent_type = 'full',
        override_boundary_errors = TRUE,
        race_levels = race_levels,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),

      cvd_prevent_full_cat = cut(
        cvd_prevent_full,
        breaks = c(0, 0.05, 0.10, 0.15, 1),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("<5%", "5% to <10%", "10% to <15%", "≥ 15%")
      ),

      cvd_prevent_full_bnry_10 = factor(cvd_prevent_full >= 0.10,
                                        levels = c(FALSE, TRUE),
                                        labels = c("< 10%", "≥ 10%")),

      cvd_prevent_full_bnry_15 = factor(cvd_prevent_full >= 0.15,
                                        levels = c(FALSE, TRUE),
                                        labels = c("< 15%", "≥ 15%")),

      ascvd_prevent_30_base = predict_30yr_ascvd_risk(
        age_years = demo_age_years,
        race = demo_race_black,
        sex = demo_gender,
        smoke_current = cc_smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = bp_sys_mean,
        bp_meds = bp_med_use,
        diabetes = cc_diabetes,
        acr = cc_acr,
        hba1c = cc_hba1c,
        sdi = cc_sdi,
        statin_meds = chol_med_statin,
        egfr_mlminm2 = cc_egfr,
        bmi = cc_bmi,
        equation_version = "Khan_2023",
        prevent_type = 'base',
        override_boundary_errors = TRUE,
        race_levels = race_levels,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),

      ascvd_prevent_30_base_bnry_30 = factor(ascvd_prevent_30_base >= 0.30,
                                             levels = c(FALSE, TRUE),
                                             labels = c("< 30%", "≥ 30%")),

      ascvd_prevent_30_base_bnry_45 = factor(ascvd_prevent_30_base >= 0.45,
                                             levels = c(FALSE, TRUE),
                                             labels = c("< 45%", "≥ 45%")),


      cvd_prevent_30_base = predict_30yr_cvd_risk(
        age_years = demo_age_years,
        race = demo_race_black,
        sex = demo_gender,
        smoke_current = cc_smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = bp_sys_mean,
        bp_meds = bp_med_use,
        diabetes = cc_diabetes,
        acr = cc_acr,
        hba1c = cc_hba1c,
        sdi = cc_sdi,
        statin_meds = chol_med_statin,
        egfr_mlminm2 = cc_egfr,
        bmi = cc_bmi,
        equation_version = "Khan_2023",
        prevent_type = 'base',
        override_boundary_errors = TRUE,
        race_levels = race_levels,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),

      cvd_prevent_30_base_bnry_30 = factor(cvd_prevent_30_base >= 0.30,
                                           levels = c(FALSE, TRUE),
                                           labels = c("< 30%", "≥ 30%")),

      cvd_prevent_30_base_bnry_45 = factor(cvd_prevent_30_base >= 0.45,
                                           levels = c(FALSE, TRUE),
                                           labels = c("< 45%", "≥ 45%")),


      ascvd_prevent_30_full = predict_30yr_ascvd_risk(
        age_years = demo_age_years,
        race = demo_race_black,
        sex = demo_gender,
        smoke_current = cc_smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = bp_sys_mean,
        bp_meds = bp_med_use,
        diabetes = cc_diabetes,
        acr = cc_acr,
        hba1c = cc_hba1c,
        sdi = cc_sdi,
        statin_meds = chol_med_statin,
        egfr_mlminm2 = cc_egfr,
        bmi = cc_bmi,
        equation_version = "Khan_2023",
        prevent_type = 'full',
        override_boundary_errors = TRUE,
        race_levels = race_levels,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),

      ascvd_prevent_30_full_bnry_30 = factor(ascvd_prevent_30_full >= 0.30,
                                             levels = c(FALSE, TRUE),
                                             labels = c("< 30%", "≥ 30%")),

      ascvd_prevent_30_full_bnry_45 = factor(ascvd_prevent_30_full >= 0.45,
                                             levels = c(FALSE, TRUE),
                                             labels = c("< 45%", "≥ 45%")),


      cvd_prevent_30_full = predict_30yr_cvd_risk(
        age_years = demo_age_years,
        race = demo_race_black,
        sex = demo_gender,
        smoke_current = cc_smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = bp_sys_mean,
        bp_meds = bp_med_use,
        diabetes = cc_diabetes,
        acr = cc_acr,
        hba1c = cc_hba1c,
        sdi = cc_sdi,
        statin_meds = chol_med_statin,
        egfr_mlminm2 = cc_egfr,
        bmi = cc_bmi,
        equation_version = "Khan_2023",
        prevent_type = 'full',
        override_boundary_errors = TRUE,
        race_levels = race_levels,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),

      cvd_prevent_30_full_bnry_30 = factor(cvd_prevent_30_full >= 0.30,
                                           levels = c(FALSE, TRUE),
                                           labels = c("< 30%", "≥ 30%")),

      cvd_prevent_30_full_bnry_45 = factor(cvd_prevent_30_full >= 0.45,
                                           levels = c(FALSE, TRUE),
                                           labels = c("< 45%", "≥ 45%")),



      # discrepancies:

      discrep_cvd_base_15 = interaction(ascvd_pce_bnry_10,
                                        cvd_prevent_base_bnry_15),

      # combine ckd and diabetes

      cc_ckd_or_diab = if_else(
        cc_ckd == "Yes" | cc_diabetes == "Yes",
        "Yes",
        "No"
      ),

      cc_ckd_or_diab_or_age = if_else(
        cc_ckd == "Yes" | cc_diabetes == "Yes" | demo_age_years >= 65,
        "Yes",
        "No"
      ),

      cc_ckd_or_diab = factor(cc_ckd_or_diab,
                              levels = c("No", "Yes")),

      cc_ckd_or_diab_or_age = factor(cc_ckd_or_diab_or_age,
                                     levels = c("No", "Yes")),

      demo_age_gteq_60 = factor(demo_age_years >= 60,
                                levels = c(FALSE, TRUE),
                                labels = c("No", "Yes")),

      chol_non_hdl_gteq_130 = factor(chol_non_hdl >= 130,
                                     levels = c(FALSE, TRUE),
                                     labels = c("No", "Yes"))



    )



}

