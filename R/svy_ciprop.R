


svy_ciprop <- function(variable, by = NULL, design){

  .design <- design

  if(!is.null(by)){

    by_lvls <- select(.design$variables, all_of(by)) %>%
      lapply(levels) %>%
      expand.grid() %>%
      unite(col = 'tmp', everything(), sep = '._x_.') %>%
      pull(tmp)

    by_vars <- select(.design$variables, all_of(by)) %>%
      unite(col = 'tmp', everything(), sep = '._x_.') %>%
      pull(tmp) %>%
      factor(levels = by_lvls)

    .design$variables$tmp_group <- by_vars

    results <- vector(mode = 'list', length = length(by_lvls))

    for(i in by_lvls){

      ..design <- subset(.design, tmp_group == i)

      results[[i]] <- svy_ciprop(variable = variable,
                                 by = NULL,
                                 design = ..design)

    }

    .id <- if(length(by) == 1) by else "split_me"

    results_bound <- bind_rows(results, .id = .id)

    if(.id == 'split_me'){
      results_bound %<>%
        separate(col = 'split_me', into = by, sep = "\\._x_\\.")
    }

    return(results_bound)

  }

  .levels <- levels(.design$variables[[variable]])

  results <- vector(mode = 'list', length = length(.levels))

  if(nrow(.design$variables) == 0){

    results <- map(results, ~ tibble(estimate = 0,
                                     std_error = NA_real_,
                                     ci_lower = 0,
                                     ci_upper = 1)) %>%
      set_names(.levels)

  } else {

    for(i in .levels){

      .design$variables %<>% mutate(tmp = as.numeric(.data[[variable]] == i))

      .svy_ci <- svyciprop(~tmp, design = .design)

      .ci <- as_tibble(confint(.svy_ci)) %>%
        rename(ci_lower = `2.5%`, ci_upper = `97.5%`)

      results[[i]] <- .svy_ci_tidy <- tibble(
        estimate = as.numeric(.svy_ci),
        std_error = as.numeric(SE(.svy_ci)),
      ) %>%
        bind_cols(.ci)

    }

  }

  results %>%
    bind_rows(.id = variable) %>%
    mutate(suppress_status = suppress_catg(estimate = estimate,
                                           std_error = std_error,
                                           ci_upper = ci_upper,
                                           ci_lower = ci_lower,
                                           design = .design))

}


