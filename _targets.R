## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  nhanes_init = load_nhanes(cycles = c("2013-2014",
                                       "2015-2016",
                                       "2017-2020")),

  nhanes_excluded = exclude_nhanes(nhanes_init),

  nhanes_derived = derive_nhanes(nhanes_excluded),

  nhanes_design = design_nhanes(nhanes_derived),

  tbl_characteristics = tabulate_characteristics(nhanes_design),
  tbl_risk_means = tabulate_risk_means(nhanes_design),
  tbl_risk_distr = tabulate_risk_distr(nhanes_design),
  tbl_risk_cross = tabulate_risk_cross(nhanes_design)



)
