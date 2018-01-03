library(PortfolioAttribution)
library(testthat)

context("Geometric multi-level Attribution Examples from Pratical Portfolio Performance Management and Attribution by Carl Bacon (2008)")
epsilon = 5e-5

cnames = c("Financial", "Software", "Autos", "Chemicals", "Govt", "Corp")
Wp = c(0.1, 0.09, 0.225, 0.17, 0.125, 0.29)
Wb = c(0.05, 0.1, 0.25, 0.15, 0.1, 0.35)

# Example using one-period time series data
Rp = xts(matrix(c(NA, NA, NA, NA, NA, NA,
                  0.0838, 0.02, 0.1605, 0.02, 0.02, 0.025),ncol=6,byrow=TRUE,
                dimnames=list(c("Rp", "Rp"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rb = xts(matrix(c(NA, NA, NA, NA, NA, NA,
                  0.096, 0.0645, 0.156, -0.0067, 0.01, 0.02),ncol=6,byrow=TRUE,
                dimnames=list(c("Rb", "Rb"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))

heirarchy = data.frame(
  primary_id = cnames, 
  level1 = c("Equities", "Equities", "Equities", "Equities", "Bonds", "Bonds"),
  level2 = c("US", "US", "Europe", "Europe", "Govt", "Corp"),
  level3 = c("Financial", "Software", "Autos", "Chemicals", NA, NA),
  stringsAsFactors = FALSE
)

test_that("Multi-level Geometric example using data in Tables 9.4" , {
  attribution_results = Attribution.levels(Rp, Wp, Rb, Wb, h = heirarchy, h_levels = c("level1", "level2", "level3"), geometric = TRUE, anchored = FALSE)
  
  expect_equal(attribution_results$`Multi-level attribution`[2, "Level 1 Allocation"], 0.002373644, tolerance = epsilon)
  expect_equal(attribution_results$`Multi-level attribution`[2, "Level 2 Allocation"], -0.000867662, tolerance = epsilon)
  expect_equal(attribution_results$`Multi-level attribution`[2, "Level 3 Allocation"], -0.002275164, tolerance = epsilon)
  
  expect_equal(attribution_results$`Multi-level attribution`[2, "Selection"], 0.00284263, tolerance = epsilon)
  
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Bonds"], 0.001305504, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Equities"], 0.00106814, tolerance = epsilon)
  
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Equities-US"], -0.0005472731, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Equities-Europe"], -2.594829e-05, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Bonds-Govt"], -0.000183997, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Bonds-Corp"], -0.0001245518, tolerance = epsilon)
  
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Equities-US-Financial"], 0.0009916887, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Equities-US-Software"], 9.916887e-05, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Equities-Europe-Autos"], -0.00144031, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Equities-Europe-Chemicals"], -0.001921043, tolerance = epsilon)

  expect_equal(attribution_results$`Security selection`[2, "Equities-US-Financial"], -0.001154866, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Equities-US-Software"], -0.003791178, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Equities-Europe-Autos"], 0.0009584438, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Equities-Europe-Chemicals"], 0.004296668, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Bonds-Govt-NA"], 0.001183264, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Bonds-Corp-NA"], 0.001372586, tolerance = epsilon)

  expect_equal(attribution_results$`Excess returns`[2,"Geometric"], 0.002080969, tolerance = epsilon)
}
)
