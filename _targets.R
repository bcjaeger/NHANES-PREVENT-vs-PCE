## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()

nhanes_init_target <-
  tar_target(
    nhanes_init,
    load_nhanes(cycles = c("2013-2014",
                           "2015-2016",
                           "2017-2020"))
  )

nhanes_excluded_target  <- tar_target(
  nhanes_excluded,
  exclude_nhanes(nhanes_init)
)

nhanes_derived_target <-
  tar_target(
    nhanes_derived,
    derive_nhanes(nhanes_excluded)
  )

nhanes_analyses <-
  tar_map(
    values = list(
      subgroup = c(
        "overall",
        "comorb"
        # "men",
        # "women",
        # "white",
        # "black"
      )
    ),
    tar_target(
      nhanes_design,
      design_nhanes(nhanes_derived, subgroup = subgroup)
    ),
    tar_target(
      tbl_characteristics,
      tabulate_characteristics(nhanes_design)
    ),
    tar_target(
      tbl_risk_means,
      tabulate_risk_means(nhanes_design)
    ),
    tar_target(
      tbl_risk_dist,
      tabulate_risk_distr(nhanes_design)
    ),
    tar_target(
      tbl_risk_cross,
      tabulate_risk_cross(nhanes_design)
    ),
    tar_target(
      tbl_discrepant,
      tabulate_discrepant(nhanes_design)
    )

  )

.analysis_overall <-
  tar_combine(
    analysis_overall,
    nhanes_analyses$tbl_characteristics[[1]],
    nhanes_analyses$tbl_risk_means[[1]],
    nhanes_analyses$tbl_risk_dist[[1]],
    nhanes_analyses$tbl_risk_cross[[1]],
    nhanes_analyses$tbl_discrepant[[1]],
    command = list(!!!.x)
  )

.analysis_comorb <-
  tar_combine(
    analysis_comorb,
    nhanes_analyses$tbl_characteristics[[2]],
    nhanes_analyses$tbl_risk_means[[2]],
    nhanes_analyses$tbl_risk_dist[[2]],
    nhanes_analyses$tbl_risk_cross[[2]],
    nhanes_analyses$tbl_discrepant[[2]],
    command = list(!!!.x)
  )


list(
  nhanes_init_target,
  nhanes_excluded_target,
  nhanes_derived_target,
  nhanes_analyses,
  .analysis_overall,
  .analysis_comorb
)
