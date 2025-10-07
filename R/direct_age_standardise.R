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
  # ------------------------------
  # Helper to convert quoted or unquoted names to symbols
  # ------------------------------
  sym_flex <- function(x) {
    if (is.character(x)) rlang::syms(x)       # quoted
    else rlang::syms(as.character(rlang::enexpr(x)))  # unquoted
  }

  var_syms <- sym_flex(var)
  ageband_sym <- rlang::ensym(ageband)
  observed_sym <- rlang::ensym(observed)
  population_sym <- rlang::ensym(population)

  # ------------------------------
  # Compute grouped counts
  # ------------------------------
  out <- data %>%
    dplyr::group_by(!!!var_syms, !!ageband_sym) %>%
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
    dplyr::group_by(!!!var_syms) %>%
    dplyr::summarise(
      das_rate = sum(expected, na.rm = TRUE) / sum(StdPopulation, na.rm = TRUE),
      variance = sum(var_component, na.rm = TRUE) / (sum(StdPopulation, na.rm = TRUE)^2),
      total_obs = sum(obs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Byar CI
      lower = ifelse(
        total_obs == 0, 0,
        das_rate * ((1 - (1 / (9 * total_obs)) - (1.96 / (3 * sqrt(total_obs))))^3)
      ),
      upper = das_rate * ((1 - (1 / (9 * (total_obs + 1))) + (1.96 / (3 * sqrt(total_obs + 1))))^3),
      lower = pmax(0, lower),
      # apply multiplier and round
      das_rate = round(multiplier * das_rate, 2),
      lower = round(multiplier * lower, 2),
      upper = round(multiplier * upper, 2)
    ) %>%
    dplyr::select(!!!var_syms, das_rate, lower, upper)

  return(out)
}
