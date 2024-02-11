#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nhanes_derived
design_nhanes <- function(nhanes_derived) {

  svydesign(ids = ~ svy_psu,
            strata = ~ svy_strata,
            weights = ~ svy_weight_mec,
            data = nhanes_derived,
            nest = TRUE)

}
