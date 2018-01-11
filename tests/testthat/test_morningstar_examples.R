library(PortfolioAttribution)
library(testthat)

context("Examples from Morningstar methodology documentation")
# Reference: https://corporate.morningstar.com/us/documents/MethodologyDocuments/MethodologyPapers/EquityPerformanceAttributionMeth.pdf

epsilon = 8e-5

cnames = c("Large Cap Asia Cyclical", "Small Cap Asia Cyclical", "Large Cap Asia Defensive", "Small Cap Asia Defensive", "Large Cap Europe Cyclical", "Small Cap Europe Cyclical", "Large Cap Europe Defensive", "Small Cap Europe Defensive")
Wp = c(0.15, 0.1, 0.05, 0.23, 0, 0.12, 0.18, 0.17)
Wb = c(0.05, 0.25, 0.1, 0.05, 0.1, 0.25, 0, 0.2)

# Example using one-period time series data
Rp = xts(matrix(c(NA, NA, NA, NA, NA, NA, NA, NA,
                  0.1, 0.2, 0.07, 0.133, 0.1476, 0.07, 0.05, -0.1),ncol=8,byrow=TRUE,
                dimnames=list(c("Rp", "Rp"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rb = xts(matrix(c(NA, NA, NA, NA, NA, NA, NA, NA,
                  -0.08, 0.08, 0.18, -0.01, 0.1476, 0.0586, 0.05, -0.05),ncol=8,byrow=TRUE,
                dimnames=list(c("Rb", "Rb"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))

heirarchy = data.frame(
  primary_id = cnames, 
  region = c("Asia", "Asia", "Asia", "Asia", "Europe", "Europe", "Europe", "Europe"),
  sector = c("Cyclical", "Cyclical", "Defensive", "Defensive", "Cyclical", "Cyclical", "Defensive", "Defensive"),
  market_cap = cnames,
  stringsAsFactors = FALSE
)

test_that("Multi-level weights & return computations" , {

  levels <- unlist(list(c("region", "sector", "market_cap")))
  h = heirarchy
  # Transform the hierarchy
  for (i in 2:length(levels)){
    h[[levels[i]]] = paste(h[[levels[i - 1]]],  h[[levels[i]]], sep = "-")
  }
  # Get returns and weights at all levels
  returns.p = list()
  weights.p = list()
  returns.b = list()
  weights.b = list()
  bs = list()
  rp = Return.portfolio(Rp, Wp)
  for(i in 1:length(levels)){
    weights.p[[i]] = Weight.level(Wp, Rp, h, level = levels[i])
    weights.b[[i]] = Weight.level(Wb, Rb, h, level = levels[i])
    returns.p[[i]] = Return.level(Rp, Wp, h, level = levels[i], weights.p[[i]])
    returns.b[[i]] = Return.level(Rb, Wb, h, level = levels[i], weights.b[[i]])
  }
    
  expect_equal(as.numeric(weights.p[[1]][2,1]), 0.53, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[1]][2,1]), 0.45, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[1]][2,2]), 0.47, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[1]][2,2]), 0.55, tolerance = epsilon)
  
  expect_equal(as.numeric(weights.p[[2]][2,1]), 0.25, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[2]][2,1]), 0.3, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[2]][2,2]), 0.28, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[2]][2,2]), 0.15, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[2]][2,3]), 0.12, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[2]][2,3]), 0.35, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[2]][2,4]), 0.35, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[2]][2,4]), 0.2, tolerance = epsilon)

  expect_equal(as.numeric(weights.p[[3]][2,1]), 0.15, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[3]][2,1]), 0.05, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[3]][2,2]), 0.1, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[3]][2,2]), 0.25, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[3]][2,3]), 0.05, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[3]][2,3]), 0.1, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[3]][2,4]), 0.23, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[3]][2,4]), 0.05, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[3]][2,5]), 0, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[3]][2,5]), 0.1, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[3]][2,6]), 0.12, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[3]][2,6]), 0.25, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[3]][2,7]), 0.18, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[3]][2,7]), 0, tolerance = epsilon)
  expect_equal(as.numeric(weights.p[[3]][2,8]), 0.17, tolerance = epsilon)
  expect_equal(as.numeric(weights.b[[3]][2,8]), 0.2, tolerance = epsilon)
  
  
  expect_equal(as.numeric(returns.p[[1]][2,1]), 0.1304, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[1]][2,1]), 0.0744, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[1]][2,2]), 0.0009, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[1]][2,2]), 0.0353, tolerance = epsilon)
  
  expect_equal(as.numeric(returns.p[[2]][2,1]), 0.14, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[2]][2,1]), 0.0533, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[2]][2,2]), 0.1218, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[2]][2,2]), 0.1167, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[2]][2,3]), 0.07, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[2]][2,3]), 0.084, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[2]][2,4]), -0.0229, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[2]][2,4]), -0.05, tolerance = epsilon)
  
  expect_equal(as.numeric(returns.p[[3]][2,1]), 0.1, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[3]][2,1]), -0.08, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[3]][2,2]), 0.2, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[3]][2,2]), 0.08, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[3]][2,3]), 0.07, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[3]][2,3]), 0.18, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[3]][2,4]), 0.133, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[3]][2,4]), -0.01, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[3]][2,5]), 0.1476, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[3]][2,5]), 0.1476, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[3]][2,6]), 0.07, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[3]][2,6]), 0.0586, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[3]][2,7]), 0.05, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[3]][2,7]), 0.05, tolerance = epsilon)
  expect_equal(as.numeric(returns.p[[3]][2,8]), -0.1, tolerance = epsilon)
  expect_equal(as.numeric(returns.b[[3]][2,8]), -0.05, tolerance = epsilon)
}
)

test_that("Multi-level Geometric example from Morningstar methodology paper" , {
  attribution_results = Attribution.levels(Rp, Wp, Rb, Wb, h = heirarchy, h_levels = c("region", "sector", "market_cap"), geometric = TRUE, anchored = TRUE)
  
  expect_equal(attribution_results$`Multi-level attribution`[2, "Level 1 Allocation"], 0.0029, tolerance = epsilon)
  expect_equal(attribution_results$`Multi-level attribution`[2, "Level 2 Allocation"], -0.0166, tolerance = epsilon)
  expect_equal(attribution_results$`Multi-level attribution`[2, "Level 3 Allocation"], -0.0273, tolerance = epsilon)
  
  expect_equal(attribution_results$`Multi-level attribution`[2, "Selection"], 0.05865, tolerance = epsilon)
  
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Asia"], 0.0016, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Europe"], 0.0013, tolerance = epsilon)
  
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Asia-Cyclical"], 0.0021, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Asia-Defensive"], 0.0041, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Europe-Cyclical"], -0.0083, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Europe-Defensive"], -0.0145, tolerance = epsilon)
  
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Asia-Cyclical-Large Cap Asia Cyclical"], -0.0139, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Asia-Cyclical-Small Cap Asia Cyclical"], -0.0028, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Asia-Defensive-Large Cap Asia Defensive"], -0.0083, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Asia-Defensive-Small Cap Asia Defensive"], -0.0167, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Europe-Cyclical-Large Cap Europe Cyclical"], -0.0021, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Europe-Cyclical-Small Cap Europe Cyclical"], -0.0008, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Europe-Defensive-Large Cap Europe Defensive"], 0.0173, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 3`[2, "Europe-Defensive-Small Cap Europe Defensive"], 0, tolerance = epsilon)
  
  expect_equal(attribution_results$`Security selection`[2, "Asia-Cyclical-Large Cap Asia Cyclical"], 0.0267, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Asia-Cyclical-Small Cap Asia Cyclical"], 0.0119, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Asia-Defensive-Large Cap Asia Defensive"], -0.0054, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Asia-Defensive-Small Cap Asia Defensive"], 0.0326, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Europe-Cyclical-Large Cap Europe Cyclical"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Europe-Cyclical-Small Cap Europe Cyclical"], 0.0014, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Europe-Defensive-Large Cap Europe Defensive"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Europe-Defensive-Small Cap Europe Defensive"], -0.0084, tolerance = epsilon)
  
  expect_equal(attribution_results$`Excess returns`[2,"Geometric"], 0.01576598, tolerance = epsilon)
}
)
