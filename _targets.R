## Load your packages, e.g. library(targets).

# TODO:
# Review risk cross-classification,
# - set cells with 0 sample size to --
# - set cells with estimate <0.1 to <0.1



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
        "comorb",
        "age_lt60"
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
      tbl_risk_dist_granular,
      tabulate_risk_distr_granular(nhanes_design)
    ),
    tar_target(
      tbl_risk_cross,
      tabulate_risk_cross(nhanes_design)
    ),
    tar_target(
      tbl_discrepant,
      tabulate_discrepant(nhanes_design)
    ),
    tar_target(
      tbl_mean_diff,
      tabulate_mean_diff(nhanes_design, nhanes_derived)
    ),
    tar_target(
      tbl_mean_diff_high_pce,
      tabulate_mean_diff(
        subset(nhanes_design, ascvd_pce_bnry_10 == "≥ 10%"),
        nhanes_derived,
        outcomes = c("ascvd_prevent_base",
                     "cvd_prevent_base",
                     "cvd_prevent_30_base")
      )
    ),
    tar_target(
      tbl_highrisk,
      tabulate_highrisk(nhanes_design)
    ),
    tar_target(
      tbl_risk_flow,
      tabulate_risk_flow(nhanes_design)
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
    nhanes_analyses$tbl_mean_diff[[1]],
    nhanes_analyses$tbl_mean_diff_high_pce[[1]],
    nhanes_analyses$tbl_highrisk[[1]],
    nhanes_analyses$tbl_risk_flow[[1]],
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
    nhanes_analyses$tbl_mean_diff[[2]],
    nhanes_analyses$tbl_mean_diff_high_pce[[2]],
    nhanes_analyses$tbl_highrisk[[2]],
    nhanes_analyses$tbl_risk_flow[[2]],
    command = list(!!!.x)
  )

.analysis_lt60 <-
  tar_combine(
    analysis_lt60,
    nhanes_analyses$tbl_characteristics[[3]],
    nhanes_analyses$tbl_risk_means[[3]],
    nhanes_analyses$tbl_risk_dist[[3]],
    nhanes_analyses$tbl_risk_cross[[3]],
    nhanes_analyses$tbl_discrepant[[3]],
    nhanes_analyses$tbl_mean_diff[[3]],
    nhanes_analyses$tbl_mean_diff_high_pce[[3]],
    nhanes_analyses$tbl_highrisk[[3]],
    nhanes_analyses$tbl_risk_flow[[3]],
    command = list(!!!.x)
  )


list(
  nhanes_init_target,
  nhanes_excluded_target,
  nhanes_derived_target,
  nhanes_analyses,
  .analysis_overall,
  .analysis_comorb,
  .analysis_lt60
) %>%
  tar_hook_before(
    hook = {

      # specification for table tables
      rspec <- round_spec() |>
        round_using_magnitude(digits = c(1, 0), breaks = c(100, Inf))

      # rspec <- round_spec() |>
      #   round_using_decimal(digits = 1)

      # save it to options:
      names(rspec) <- paste('table.glue', names(rspec), sep = '.')
      options(rspec)

    }
  )
