#' Direct Age Standardisation with Robust Byar Confidence Intervals
#'
#' Computes directly age-standardised rates using Byar's method.
#' Lower bounds less than zero are reported as zero.
#' Handles cases where DAS rate is zero or very small.
#'
#' @param data Dataframe with observed values and population
#' @param var Grouping variable(s) (e.g. region, sex, deprivation, year)
#' @param ageband Column specifying age bands
#' @param observed Column with observed counts
#' @param population Column with denominator population
#' @param multiplier Scale for rates (default = 1,000)
#'
#' @return A tibble with directly standardised rates and 95% confidence intervals
#' @export

direct_age_standardise <- function(data, var, ageband, observed, population, multiplier = 1000) {

  var_syms <- rlang::syms(var)
  ageband_sym <- rlang::ensym(ageband)
  observed_sym <- rlang::ensym(observed)
  population_sym <- rlang::ensym(population)

  out <- data %>%
    dplyr::group_by(dplyr::across(!!!var_syms), !!ageband_sym) %>%
    dplyr::summarise(
      obs = sum(!!observed_sym, na.rm = TRUE),
      pop = sum(!!population_sym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(rate = obs / pop) %>%
    dplyr::left_join(
      ref_population,
      by = setNames("AgeBand", rlang::as_string(ageband_sym))
    ) %>%
    dplyr::mutate(
      expected = rate * StdPopulation,
      var_component = (StdPopulation^2) * (obs / (pop^2))
    ) %>%
    dplyr::group_by(dplyr::across(!!!var_syms)) %>%
    dplyr::summarise(
      das_rate = sum(expected, na.rm = TRUE) / 100000,
      variance = sum(var_component, na.rm = TRUE) / (sum(StdPopulation, na.rm = TRUE)^2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = sqrt(variance),
      # Robust Byar CI
      epsilon = 1e-10,  # small value to avoid division by zero
      lower = ifelse(das_rate > 0,
                     das_rate * (1 - 1.96 / sqrt(das_rate * sum(StdPopulation, na.rm = TRUE) + epsilon)),
                     0),
      upper = ifelse(das_rate > 0,
                     das_rate * (1 + 1.96 / sqrt(das_rate * sum(StdPopulation, na.rm = TRUE) + epsilon)),
                     0),
      lower = pmax(0, lower),
      das_rate = round(multiplier * das_rate, 2),
      lower = round(multiplier * lower, 2),
      upper = round(multiplier * upper, 2)
    ) %>%
    dplyr::select(dplyr::all_of(var), das_rate, lower, upper)

  return(out)
}
