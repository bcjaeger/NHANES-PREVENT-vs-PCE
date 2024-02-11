#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nhanes_init
exclude_nhanes <- function(nhanes_init) {

  # nhanes_init has two pre-built exclusions:
  # 1. >= 18 years old
  # 2. completed interview and exam

  # the code here pulls down raw nhanes files in order to get
  # counts of participants for exclusions in the current analysis

  links <- c(
    "2013-2014" = "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT",
    "2015-2016" = "https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT",
    "2017-2020" = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.XPT"
  )

  nhanes_raw <- map_dfr(links, haven::read_xpt, .id = 'svy_year')

  # convenience
  tidy_counts <- function(data, exclusion_label){
    total <- nrow(data)
    by_cyc <- count(data, svy_year)
    pivot_wider(by_cyc, values_from = n, names_from = svy_year) %>%
      mutate(overall = total, .before = 1) %>%
      mutate(label = exclusion_label, .before = 1)
  }

  # Completed the interview and examination
  nhanes_exclude_1 <- filter(nhanes_raw, RIDSTATR == 2)

  # Age 30 to 79 years of age
  nhanes_exclude_2 <- nhanes_exclude_1 %>%
    filter(RIDAGEYR >= 30, RIDAGEYR <= 79)

  nhanes_cardiostat_exclude_2 <- nhanes_init %>%
    filter(demo_age_years >= 30, demo_age_years <= 79)

  # be assertive
  test_that(
    desc = paste("cardioStatsUSA data have the same counts as the raw data",
                 "downloaded from CDC/NHANES website"),
    code = {

      # same counts
      expect_equal(
        tidy_counts(nhanes_exclude_2, "Age 30 to 79 years of age"),
        tidy_counts(nhanes_cardiostat_exclude_2, "Age 30 to 79 years of age")
      )

      # same people
      expect_true(
        all(nhanes_cardiostat_exclude_2$svy_id %in% nhanes_exclude_2$SEQN) &&
          all(nhanes_exclude_2$SEQN %in% nhanes_cardiostat_exclude_2$svy_id)
      )

    }
  )

  # transfer from raw nhanes data to cardiostats data
  nhanes_exclude_2 <- nhanes_cardiostat_exclude_2

  # Without history of CVD:
  #  - heart disease
  #  - myocardial infarction
  #  - stroke
  #  - heart failure
  nhanes_exclude_3 <- nhanes_exclude_2 %>%
    filter(cc_cvd_any == "No")

  # Have information on BP and self-reported antihypertensive medication use
  nhanes_exclude_4 <- nhanes_exclude_3 %>%
    drop_na(bp_sys_mean, bp_dia_mean, bp_med_use)

  # Have data for the variables in the PCEs and PREVENT equations
  # - PREVENT variables (age, sex, total cholesterol, HDL-cholesterol,
  #   cigarette smoking, diabetes, estimated glomerular filtration rate,
  #   statin use)
  # - Pooled cohort risk equation variables (race/ethnicity)
  nhanes_exclude_5 <- nhanes_exclude_4 %>%
    drop_na(demo_age_years,
            demo_gender,
            chol_total,
            chol_hdl,
            chol_med_statin,
            cc_smoke,
            cc_diabetes,
            cc_egfr)

  counts <- bind_rows(
  tidy_counts(nhanes_raw, "NHANES participants from 2013-2020"),
  tidy_counts(nhanes_exclude_2, "Completed the interview and examination"),
  tidy_counts(nhanes_exclude_3, "No history of CVD"),
  tidy_counts(nhanes_exclude_4, paste("Have information on SBP, DBP",
                                      "and self-reported antihypertensive",
                                      "medication use")),
  tidy_counts(nhanes_exclude_5, paste("Have information on other variables",
                                      "in the PCEs and PREVENT equations"))
  )

  list(data = nhanes_exclude_5,
       counts = counts)

}
