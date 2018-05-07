library(PortfolioAttribution)
library(testthat)

#----------------------------------------------------------------------------------------------

context("Contribution analysis")

epsilon = 1e-10

data(sample_data, package = "PortfolioAttribution")

test_that("Contribution data is computed correctly for portfolio and benchmark for arithmetic attribution analysis" , {
  attribution_results = Attribution(multi_period_portf_2$Rp, multi_period_portf_2$wp, 
                                    multi_period_portf_2$Rb, multi_period_portf_2$wb, contribution = TRUE)
  
  expect_equal(as.data.frame(attribution_results$`Portfolio contribution to return`),
               multi_period_portf_2$Rp*multi_period_portf_2$wp)
  expect_equal(as.data.frame(attribution_results$`Benchmark contribution to return`),
               multi_period_portf_2$Rb*multi_period_portf_2$wb)
  
}
)

test_that("Contribution data is computed correctly for portfolio and benchmark for geometric attribution analysis" , {
  attribution_results = Attribution(multi_period_portf_1$Rp, multi_period_portf_1$wp, 
                                    multi_period_portf_1$Rb, multi_period_portf_1$wb, 
                                    geometric = TRUE, contribution = TRUE)
  
  expect_equal(coredata(attribution_results$`Portfolio contribution to return`),
               coredata(multi_period_portf_1$Rp)*coredata(multi_period_portf_1$wp))
  expect_equal(coredata(attribution_results$`Benchmark contribution to return`),
               coredata(multi_period_portf_1$Rb)*coredata(multi_period_portf_1$wb))
}
)
