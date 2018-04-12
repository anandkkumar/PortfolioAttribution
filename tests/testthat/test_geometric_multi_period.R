library(PortfolioAttribution)
library(testthat)

context("Geometric Attribution Examples from Pratical Portfolio Performance Management and Attribution by Carl Bacon (2008)")
epsilon = 1e-4

cnames = c("UK equities", "Japanese equities", "US equities")

test_that("Multi-period geometric attribution example in Table 8.6" , {
  Wp = matrix(c(0.4, 0.3, 0.3, 0.7, 0.2, 0.1, 0.3, 0.5, 0.2, 0.3, 0.5, 0.2), 4, 3, TRUE, 
              dimnames = list(c("2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30"), cnames))
  Wb = matrix(c(0.4, 0.2, 0.4, 0.4, 0.3, 0.3, 0.5, 0.4, 0.1, 0.4, 0.4, 0.2), 4, 3, TRUE, 
              dimnames = list(c("2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30"), cnames))
  Rp = matrix(c(0.2, -0.05, 0.06, -0.05, 0.03, -0.05, -0.2, 0.08, -0.15, 0.1, -0.07, 0.25), 4, 3, TRUE, 
              dimnames = list(c("2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31"), cnames))
  Rb = matrix(c(0.1, -0.04, 0.08, -0.07, 0.04, -0.1, -0.25, 0.05, -0.2, 0.05, -0.05, 0.1), 4, 3, TRUE, 
              dimnames = list(c("2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31"), cnames))
  
  Rp = checkData(Rp)
  Rb = checkData(Rb)
  Wp = checkData(Wp)
  Wb = checkData(Wb)
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "none", geometric = TRUE)
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0098)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0015)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.0113)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.038) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.0029)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.0057)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.0295) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0075)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.009)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0113) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.0052)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.0148) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.0021)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.0053) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0179) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.0286) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.02) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0086)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.04) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.0165) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.0165) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.011) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.044) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.0029)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.0069)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.0098)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.0149) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.0099)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.0297) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.0347) < epsilon)
  
  #Four quarter totals (component-level multi-period totals are not computed by this algorithm)
  expect_true(abs(attribution_results$`Excess returns`[5,"Geometric"] - 0.1464) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.0129) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1319) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)
