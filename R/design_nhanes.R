#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nhanes_derived
design_nhanes <- function(nhanes_derived, subgroup) {

  design_init <- svydesign(ids = ~ svy_psu,
                           strata = ~ svy_strata,
                           weights = ~ svy_weight_mec,
                           data = nhanes_derived,
                           nest = TRUE)

  design_out <- switch(

    subgroup,

    "overall" = design_init,

    "comorb" = design_init %>%
      subset(cc_diabetes == "No" &
               (cc_ckd == "No" | is.na(cc_ckd)) &
               demo_age_years < 65),

    "men" = design_init %>%
      subset(demo_gender == "Men"),

    "women" = design_init %>%
      subset(demo_gender == "Women"),

    age_lt60 = design_init %>%
      subset(demo_age_years < 60),

    "white" = design_init %>%
      subset(demo_race == "White"),

    "black" = design_init %>%
      subset(demo_race == "Black")

  )

  attr(design_out, "subgroup") <- subgroup

  design_out

}

get_design_subgroup <- function(x) attr(x, 'subgroup')

