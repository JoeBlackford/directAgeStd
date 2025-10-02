#' Direct Age Standardisation with Confidence Intervals
#'
#' Computes directly age-standardised rates with 95% confidence intervals
#' using the 2013 European Standard Population.
#'
#' @param data Dataframe with observed values and population
#' @param var Grouping variable (e.g. region, sex)
#' @param ageband Column specifying age bands
#' @param observed Column with observed counts
#' @param population Column with denominator population
#' @param multiplier Scale for rates (default = 1,000)
#'
#' @return A tibble with directly standardised rates and 95% confidence intervals
#' @export


direct_age_standardise <- function(data, var, ageband, observed, population, multiplier = 1000) {
  var <- rlang::ensym(var)
  ageband <- rlang::ensym(ageband)
  observed <- rlang::ensym(observed)
  population <- rlang::ensym(population)

  out <- data %>%
    dplyr::group_by(!!var, !!ageband) %>%
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
    dplyr::group_by(!!var) %>%
    dplyr::summarise(
      das_rate = sum(expected, na.rm = TRUE) / 100000,
      variance = sum(var_component, na.rm = TRUE) / (sum(StdPopulation, na.rm = TRUE)^2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = sqrt(variance),
      lower = das_rate - 1.96 * se,
      upper = das_rate + 1.96 * se,
      das_rate = round(multiplier * das_rate, 2),
      lower = round(multiplier * lower, 2),
      upper = round(multiplier * upper, 2)
    ) %>%
    dplyr::select(!!var, das_rate, lower, upper)

  return(out)
}

