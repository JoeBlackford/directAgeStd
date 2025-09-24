
library(dplyr)
library(magrittr)

#test population
#inputs = var, ageband, observed, population

test <- data.frame(
  var = c("Gloucester","Gloucester","Gloucester","Gloucester","Gloucester",
          "Gloucester","Gloucester","Gloucester","Gloucester","Gloucester",
          "Gloucester","Gloucester","Gloucester","Gloucester","Gloucester",
          "Gloucester","Gloucester","Gloucester","Gloucester"),
  ageband = c("0-4",   "5-9",   "10-14", "15-19", "20-24",
              "25-29", "30-34", "35-39", "40-44", "45-49",
              "50-54", "55-59", "60-64", "65-69", "70-74",
              "75-79", "80-84", "85-89", "90+"),
  observed = c(127, 25, 18, 21, 12,
               27, 48, 27, 29, 32,
               32, 34, 45, 65, 38,
               68, 74, 82, 91),
  population = c(636, 558, 609, 543, 384,
                 594, 669, 843, 660, 576,
                 606, 522, 426, 273, 300,
                 288, 153, 123, 207))

out <- direct_age_standardise(test, var, ageband, observed, population, multiplier = 1000)
devtools::install()
