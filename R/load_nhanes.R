#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

load_nhanes <- function(cycles) {

  nhanes_data <- cardioStatsUSA::nhanes_data %>%
    rename(cc_bmi_cat = cc_bmi)

  nhanes_pm <- read_sas("data/final9920_02202023.sas7bdat")

  merge_in <- nhanes_pm %>%
    select(svy_id = SEQN,
           cc_bmi = BMXBMI,
           chol_hdl = lbdhdd,
           chol_total = LBXTC)

  nhanes_data %>%
    filter(svy_year %in% cycles) %>%
    select(starts_with("svy"),
           -contains("subpop"),
           starts_with("demo"),
           bp_sys_mean,
           bp_dia_mean,
           bp_cat_meds_included,
           bp_med_use,
           chol_med_statin,
           cc_cvd_any,
           cc_bmi_cat,
           cc_diabetes,
           cc_smoke,
           cc_acr,
           cc_egfr,
           cc_hba1c,
           cc_egfr_lt60,
           cc_acr_gteq30) %>%
    left_join(merge_in, by = 'svy_id') %>%
    relocate(cc_bmi, .before = cc_bmi_cat)

}
