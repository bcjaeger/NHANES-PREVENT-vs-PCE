#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

load_nhanes <- function(cycles) {

  nhanes_data <- cardioStatsUSA::nhanes_data %>%
    rename(cc_bmi_cat = cc_bmi)

  nhanes_statin <- read_sas('data/statin9900.sas7bdat') %>%
    rename(svy_id = SEQN)

  nhanes_pm <- read_sas("data/final9920_02202023.sas7bdat")

  merge_in <- nhanes_pm %>%
    select(svy_id = SEQN,
           cc_bmi = BMXBMI,
           chol_hdl = lbdhdd,
           chol_total = LBXTC) %>%
    left_join(nhanes_statin) %>%
    mutate(statin = if_else(is.na(statin), 0, statin),
           statin = factor(statin,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
    rename(chol_med_statin = statin)


  nhanes_data %>%
    filter(svy_year %in% cycles) %>%
    select(starts_with("svy"),
           -contains("subpop"),
           starts_with("demo"),
           bp_sys_mean,
           bp_dia_mean,
           bp_cat_meds_included,
           bp_med_use,
           cc_cvd_any,
           cc_bmi_cat,
           cc_diabetes,
           cc_smoke,
           cc_acr,
           cc_egfr,
           cc_hba1c,
           cc_egfr_lt60,
           cc_acr_gteq30) %>%
    mutate(
      # use this version as it allows missing
      cc_ckd = if_else(
        cc_acr > 30 | cc_egfr < 60,
        "Yes",
        "No"
      )
    ) %>%
    left_join(merge_in, by = 'svy_id') %>%
    mutate(across(where(is.character), as.factor)) %>%
    relocate(cc_bmi, .before = cc_bmi_cat)

}
