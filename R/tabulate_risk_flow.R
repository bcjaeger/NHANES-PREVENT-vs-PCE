
# tar_load(nhanes_design_overall)
# nhanes_design <- nhanes_design_overall

tabulate_risk_flow <- function(nhanes_design) {


  # first_col <- svytable(~ ascvd_pce_bnry_10 +
  #                         bp_cat_meds_included,
  #                       nhanes_design) %>%
  #   as_tibble() %>%
  #   group_by(bp_cat_meds_included) %>%
  #   mutate(p_1 = n / sum(n))

  first_col <-
    svy_ciprop(variable = 'ascvd_pce_bnry_10',
               by = 'bp_cat_meds_included',
               design = nhanes_design) %>%
    mutate(p_1 = if_else(is_suppressed(suppress_status),
                         true = "--",
                         false = table_glue("{estimate * 100}"))) %>%
    select(ascvd_pce_bnry_10, bp_cat_meds_included, p_1)

  # second_col <- svytable(~ ascvd_pce_bnry_10 +
  #                          cvd_prevent_base_bnry_15 +
  #                          bp_cat_meds_included,
  #                        nhanes_design) %>%
  #   as_tibble() %>%
  #   group_by(bp_cat_meds_included) %>%
  #   mutate(p_2 = n / sum(n))

  second_col <-
    svy_ciprop(variable = 'cvd_prevent_base_bnry_15',
               by = c('bp_cat_meds_included', 'ascvd_pce_bnry_10'),
               design = nhanes_design) %>%
    mutate(p_2 = if_else(is_suppressed(suppress_status),
                         true = "--",
                         false = table_glue("{estimate * 100}"))) %>%
    select(ascvd_pce_bnry_10,
           bp_cat_meds_included,
           cvd_prevent_base_bnry_15,
           p_2)


  # third_col <-  svytable(~ ascvd_pce_bnry_10 +
  #                          cvd_prevent_base_bnry_15 +
  #                          cvd_prevent_30_base_bnry_30 +
  #                          bp_cat_meds_included,
  #                        design = nhanes_design) %>%
  #   as_tibble() %>%
  #   group_by(bp_cat_meds_included) %>%
  #   mutate(p_3 = n / sum(n))

  third_col <-
    svy_ciprop(variable = 'cvd_prevent_30_base_bnry_30',
               by = c("cvd_prevent_base_bnry_15",
                      "ascvd_pce_bnry_10",
                      "bp_cat_meds_included"),
               design = nhanes_design) %>%
    mutate(p_3 = if_else(is_suppressed(suppress_status),
                         true = "--",
                         false = table_glue("{estimate * 100}"))) %>%
    select(ascvd_pce_bnry_10,
           bp_cat_meds_included,
           cvd_prevent_base_bnry_15,
           cvd_prevent_30_base_bnry_30,
           p_3)

  tbl_col_pct <- list(first_col, second_col, third_col) %>%
    reduce(full_join) %>%
    mutate(p_1 = as.numeric(p_1),
           p_2 = as.numeric(p_2),
           p_3 = as.numeric(p_3),
           p_1_new = p_1/100,
           p_2_new = (p_2/100) * (p_1/100),
           p_3_new = (p_3/100) * (p_2/100) * (p_1/100)) %>%
    select(-c(p_1, p_2, p_3)) %>%
    rename(p_1 = p_1_new,
           p_2 = p_2_new,
           p_3 = p_3_new) %>%
    mutate(across(c(p_1, p_2, p_3), ~table_value(.x * 100)))


  tbl_col_pct

  # first_col <- svytable(~ ascvd_pce_bnry_10, nhanes_design) %>%
  #   as_tibble() %>%
  #   mutate(p_1 = n / sum(n))
  #
  # second_col <- svytable(~ ascvd_pce_bnry_10 + cvd_prevent_base_bnry_15,
  #                        nhanes_design) %>%
  #   as_tibble() %>%
  #   group_by(ascvd_pce_bnry_10) %>%
  #   mutate(p_2 = n / sum(n))
  #
  # third_col <-  svytable(~ ascvd_pce_bnry_10 +
  #                          cvd_prevent_base_bnry_15 +
  #                          cvd_prevent_30_base_bnry_30,
  #                        design = nhanes_design) %>%
  #   as_tibble() %>%
  #   group_by(ascvd_pce_bnry_10, cvd_prevent_base_bnry_15) %>%
  #   mutate(p_3 = n / sum(n))
  #
  # tbl_row_pct <- list(first_col, second_col, third_col) %>%
  #   map(select, -n) %>%
  #   reduce(full_join)

  # list(col_pct = tbl_col_pct,
  #      row_pct = tbl_row_pct)


}
