#' Direct Age Standardisation with Confidence Intervals
#'
#' Computes directly age-standardised rates with 95% confidence intervals
#' using the 2013 European Standard Population and Byar’s method.
#'
#' @param data Dataframe with observed values and population
#' @param var One or more grouping variables (quoted or unquoted)
#' @param ageband Column specifying age bands
#' @param observed Column with observed counts
#' @param population Column with denominator population
#' @param multiplier Scale for rates (default = 1,000)
#'
#' @return A tibble with directly standardised rates and 95% confidence intervals
#' @export
direct_age_standardise <- function(data, var, ageband, observed, population, multiplier = 1000) {
  # handle symbols flexibly (quoted or unquoted)
  var_syms <- rlang::syms(rlang::as_string(rlang::enexpr(var)) %||% var)
  if (is.character(var)) var_syms <- rlang::syms(var)
  ageband <- rlang::ensym(ageband)
  observed <- rlang::ensym(observed)
  population <- rlang::ensym(population)

  out <- data %>%
    dplyr::group_by(!!!var_syms, !!ageband) %>%
    dplyr::summarise(
      obs = sum(!!observed, na.rm = TRUE),
      pop = sum(!!population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(rate = obs / pop) %>%
    dplyr::left_join(
      ref_population,
      by = setNames("AgeBand", rlang::as_string(ageband))
    ) %>%
    dplyr::mutate(
      expected = rate * StdPopulation,
      var_component = (StdPopulation^2) * (obs / (pop^2))
    ) %>%
    dplyr::group_by(!!!var_syms) %>%
    dplyr::summarise(
      das_rate = sum(expected, na.rm = TRUE) / sum(StdPopulation),
      variance = sum(var_component, na.rm = TRUE) / (sum(StdPopulation, na.rm = TRUE)^2),
      total_obs = sum(obs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Byar’s CI approximation
    dplyr::mutate(
      lower = ifelse(
        total_obs == 0, 0,
        das_rate * ((1 - (1 / (9 * total_obs)) - (1.96 / (3 * sqrt(total_obs))))^3)
      ),
      upper = das_rate * ((1 - (1 / (9 * (total_obs + 1))) + (1.96 / (3 * sqrt(total_obs + 1))))^3),
      lower = ifelse(lower < 0, 0, lower),
      das_rate = multiplier * das_rate,
      lower = multiplier * lower,
      upper = multiplier * upper
    ) %>%
    dplyr::select(!!!var_syms, das_rate, lower, upper)

  return(out)
}
