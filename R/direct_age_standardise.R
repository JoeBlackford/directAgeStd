#' Direct Age Standardisation with Byar Confidence Intervals
#'
#' Computes directly age-standardised rates using Byar's method.
#' Lower bounds less than zero are reported as zero.
#' Handles multiple grouping variables.
#'
#' @param data Dataframe with observed values and population
#' @param var Vector of grouping variable names (strings), e.g. c("Region", "Sex")
#' @param ageband Column name (string) specifying age bands
#' @param observed Column name (string) with observed counts
#' @param population Column name (string) with denominator population
#' @param multiplier Scale for rates (default = 1000)
#'
#' @return A tibble with directly standardised rates and 95% confidence intervals
#' @export
direct_age_standardise <- function(data, var, ageband, observed, population, multiplier = 1000) {

  if (!is.character(var)) {
    stop("`var` must be a character vector of column names, e.g. c('PCN', 'FinancialYear').", call. = FALSE)
  }
  if (!is.character(ageband) || length(ageband) != 1) {
    stop("`ageband` must be a single column name as a string, e.g. 'AgeBand'.", call. = FALSE)
  }
  if (!is.character(observed) || length(observed) != 1) {
    stop("`observed` must be a single column name as a string, e.g. 'Value'.", call. = FALSE)
  }
  if (!is.character(population) || length(population) != 1) {
    stop("`population` must be a single column name as a string, e.g. 'Population'.", call. = FALSE)
  }

  var_syms <- rlang::syms(var)
  ageband_sym <- rlang::sym(ageband)
  observed_sym <- rlang::sym(observed)
  population_sym <- rlang::sym(population)

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
      das_rate = sum(expected, na.rm = TRUE) / sum(StdPopulation, na.rm = TRUE),
      variance = sum(var_component, na.rm = TRUE) / (sum(StdPopulation, na.rm = TRUE)^2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se = sqrt(variance),
      lower = ifelse(das_rate > 0,
                     das_rate * (1 - 1.96 / sqrt(das_rate * sum(ref_population$StdPopulation))),
                     0),
      upper = ifelse(das_rate > 0,
                     das_rate * (1 + 1.96 / sqrt(das_rate * sum(ref_population$StdPopulation))),
                     0),
      lower = pmax(0, lower),  # prevent negatives
      das_rate = round(multiplier * das_rate, 2),
      lower = round(multiplier * lower, 2),
      upper = round(multiplier * upper, 2)
    ) %>%
    dplyr::select(dplyr::all_of(var), das_rate, lower, upper)

  return(out)}
