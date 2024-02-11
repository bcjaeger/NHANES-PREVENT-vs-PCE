## library() calls go here
library(conflicted)
library(dotenv)
library(targets)
library(tarchetypes)

library(cardioStatsUSA)
library(tidyverse)
library(haven)
library(PooledCohort)

library(testthat)
library(survey)
library(gtsummary)
library(glue)
library(table.glue)
library(magrittr)

conflicts_prefer(dplyr::filter)
conflicts_prefer(purrr::set_names)
