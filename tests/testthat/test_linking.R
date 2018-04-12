library(PortfolioAttribution)
library(testthat)

context("Examples for linking approaches from Pratical Portfolio Performance Management and Attribution by Carl Bacon (2008)")
epsilon = 1e-4

cnames = c("UK equities", "Japanese equities", "US equities")

test_that("Multi-period attribution example in Table 8.1 structured as quarterly data" , {
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
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "none")
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.031) < epsilon)

  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0072)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0086)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0108) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.005)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.014) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.005) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.017) < epsilon)

  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.025) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0175) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0075)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.035) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.01) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.04) < epsilon)

  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.01)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.01)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.03) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.035) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.1 structured as daily data" , {
  Wp = matrix(c(0.4, 0.3, 0.3, 0.7, 0.2, 0.1, 0.3, 0.5, 0.2, 0.3, 0.5, 0.2), 4, 3, TRUE, 
              dimnames = list(c("2015-12-31", "2016-01-01", "2016-01-02", "2016-01-03"), cnames))
  Wb = matrix(c(0.4, 0.2, 0.4, 0.4, 0.3, 0.3, 0.5, 0.4, 0.1, 0.4, 0.4, 0.2), 4, 3, TRUE, 
              dimnames = list(c("2015-12-31", "2016-01-01", "2016-01-02", "2016-01-03"), cnames))
  Rp = matrix(c(0.2, -0.05, 0.06, -0.05, 0.03, -0.05, -0.2, 0.08, -0.15, 0.1, -0.07, 0.25), 4, 3, TRUE, 
              dimnames = list(c("2016-01-01", "2016-01-02", "2016-01-03", "2016-01-04"), cnames))
  Rb = matrix(c(0.1, -0.04, 0.08, -0.07, 0.04, -0.1, -0.25, 0.05, -0.2, 0.05, -0.05, 0.1), 4, 3, TRUE, 
              dimnames = list(c("2016-01-01", "2016-01-02", "2016-01-03", "2016-01-04"), cnames))
  
  Rp = checkData(Rp)
  Rb = checkData(Rb)
  Wp = checkData(Wp)
  Wb = checkData(Wb)

  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "none")
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.031) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0072)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0086)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0108) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.005)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.014) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.005) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.017) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.025) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0175) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0075)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.035) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.01) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.04) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.01)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.01)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.03) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.035) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.1 structured as data on arbitrary dates" , {
  # The weights can be on dates that are not necessarily exactly the day prior to those of the returns.
  Wp = matrix(c(0.4, 0.3, 0.3, 0.7, 0.2, 0.1, 0.3, 0.5, 0.2, 0.3, 0.5, 0.2), 4, 3, TRUE, 
              dimnames = list(c("2016-01-13", "2016-01-31", "2016-02-15", "2016-03-18"), cnames))
  Wb = matrix(c(0.4, 0.2, 0.4, 0.4, 0.3, 0.3, 0.5, 0.4, 0.1, 0.4, 0.4, 0.2), 4, 3, TRUE, 
              dimnames = list(c("2016-01-13", "2016-01-31", "2016-02-15", "2016-03-18"), cnames))
  Rp = matrix(c(0.2, -0.05, 0.06, -0.05, 0.03, -0.05, -0.2, 0.08, -0.15, 0.1, -0.07, 0.25), 4, 3, TRUE, 
              dimnames = list(c("2016-01-16", "2016-02-02", "2016-02-16", "2016-03-19"), cnames))
  Rb = matrix(c(0.1, -0.04, 0.08, -0.07, 0.04, -0.1, -0.25, 0.05, -0.2, 0.05, -0.05, 0.1), 4, 3, TRUE, 
              dimnames = list(c("2016-01-16", "2016-02-02", "2016-02-16", "2016-03-19"), cnames))
  
  Rp = checkData(Rp)
  Rb = checkData(Rb)
  Wp = checkData(Wp)
  Wb = checkData(Wb)
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "none")
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.031) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0072)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0086)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0108) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.005)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.014) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.005) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.017) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.025) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0175) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0075)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.035) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.01) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.04) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.01)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.01)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.03) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.035) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.2 with Carino linking with unadjusted per-period attribution" , {
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
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "carino", adjusted = FALSE)
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.031) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0072)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0086)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0108) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.005)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.014) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.005) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.017) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.025) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0175) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0075)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.035) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.01) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.04) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.01)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.01)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.03) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.035) < epsilon)
  
  #Four quarter totals
  expect_true(abs(attribution_results$Allocation[5,"UK equities"] - 0.0165) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Japanese equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"US equities"] - 0.0015) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.012) < epsilon)
  
  expect_true(abs(attribution_results$Selection[5,"UK equities"] - 0.0804) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Japanese equities"] - 0.0018) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"US equities"] - 0.0385) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1207) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.2 with Carino linking with adjusted per-period attribution" , {
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
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "carino", adjusted = TRUE)
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0094)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0014)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.0109)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.0362) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.0027)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.0054)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.028) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0073)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0087)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0109) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.0051)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.0142) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.0051) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0172) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.0266) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0186) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.008)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.0373) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.016) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.016) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.0106) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.0426) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.0028)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.0066)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.0094)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.0141) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.0094)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.0282) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.0329) < epsilon)
  
  #Four quarter totals
  expect_true(abs(attribution_results$Allocation[5,"UK equities"] - 0.0165) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Japanese equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"US equities"] - 0.0015) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.012) < epsilon)
  
  expect_true(abs(attribution_results$Selection[5,"UK equities"] - 0.0804) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Japanese equities"] - 0.0018) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"US equities"] - 0.0385) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1207) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.3 with Menchero linking with unadjusted per-period attribution" , {
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
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "menchero", adjusted = FALSE)
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.031) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0072)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0086)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0108) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.005)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.014) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.005) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.017) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.025) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0175) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0075)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.035) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.01) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.04) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.01)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.01)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.03) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.035) < epsilon)
  
  #Four quarter totals
  expect_true(abs(attribution_results$Allocation[5,"UK equities"] - 0.0156) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Japanese equities"] - (-0.0078)) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"US equities"] - 0.0014) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.0092) < epsilon)
  
  expect_true(abs(attribution_results$Selection[5,"UK equities"] - 0.0838) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Japanese equities"] - 0.0005) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"US equities"] - 0.0391) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1234) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.2 with Menchero linking with adjusted per-period attribution" , {
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
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "menchero", adjusted = TRUE)
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0103)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.0119)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.0396) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.0059)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.0307) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0071)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0085)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0107) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.0049)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.0138) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.0049) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0168) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.0257) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.018) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0077)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.036) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.0154) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.0154) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.0103) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.0411) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.0099)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.0149) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.0099)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.0298) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.0348) < epsilon)
  
  #Four quarter totals
  expect_true(abs(attribution_results$Allocation[5,"UK equities"] - 0.0156) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Japanese equities"] - (-0.0078)) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"US equities"] - 0.0014) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.0092) < epsilon)
  
  expect_true(abs(attribution_results$Selection[5,"UK equities"] - 0.0838) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Japanese equities"] - 0.0005) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"US equities"] - 0.0391) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1234) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.4 with GRAP linking with unadjusted per-period attribution" , {
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
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "grap", adjusted = FALSE)
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.031) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0072)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0086)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0108) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.005)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.014) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.005) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.017) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.025) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0175) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0075)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.035) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.01) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.04) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.01)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.01)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.03) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.035) < epsilon)
  
  #Four quarter totals
  expect_true(abs(attribution_results$Allocation[5,"UK equities"] - 0.0167) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Japanese equities"] - (-0.0055)) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"US equities"] - 0.0011) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.0124) < epsilon)
  
  expect_true(abs(attribution_results$Selection[5,"UK equities"] - 0.0785) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Japanese equities"] - 0.0016) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"US equities"] - 0.0402) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1203) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.4 with GRAP linking with adjusted per-period attribution" , {
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
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "grap", adjusted = TRUE)
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0089)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0014)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.0102)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.0341) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.0026)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.0051)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.0264) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0083)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0104) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.0048)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.0135) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.0019)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.0048) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0164) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.0267) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0187) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.008)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.0373) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.016) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.016) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.0107) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.0427) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.0099)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.0149) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.0099)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.0298) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.0348) < epsilon)
  
  #Four quarter totals
  expect_true(abs(attribution_results$Allocation[5,"UK equities"] - 0.0167) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Japanese equities"] - (-0.0055)) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"US equities"] - 0.0011) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.0124) < epsilon)
  
  expect_true(abs(attribution_results$Selection[5,"UK equities"] - 0.0785) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Japanese equities"] - 0.0016) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"US equities"] - 0.0402) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1203) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.4 with Frongello linking with unadjusted per-period attribution" , {
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
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "frongello", adjusted = FALSE)
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.031) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0072)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0086)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0108) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.005)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.014) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.005) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.017) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.025) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0175) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0075)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.035) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.01) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.04) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.007)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.01)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.015) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.01)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.03) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.035) < epsilon)
  
  #Four quarter totals
  expect_true(abs(attribution_results$Allocation[5,"UK equities"] - 0.0167) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Japanese equities"] - (-0.0055)) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"US equities"] - 0.0011) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.0124) < epsilon)
  
  expect_true(abs(attribution_results$Selection[5,"UK equities"] - 0.0785) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Japanese equities"] - 0.0016) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"US equities"] - 0.0402) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1203) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


test_that("Multi-period attribution example in Table 8.5 with Frongello linking with adjusted per-period attribution" , {
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
  
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = TRUE, method = "top.down", linking = "frongello", adjusted = TRUE)
  expect_true(abs(attribution_results$Allocation[1,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[1,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[1,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[1,"Total"] - 0.031) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - (-0.0078)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0088)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - 0.0118) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.0049)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.0133) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - 0.0057) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.017) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[3,"UK equities"] - 0.0271) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Japanese equities"] - 0.0207) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"US equities"] - (-0.0091)) < epsilon)
  expect_true(abs(attribution_results$Allocation[3,"Total"] - 0.0387) < epsilon)
  
  expect_true(abs(attribution_results$Selection[3,"UK equities"] - 0.009) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Japanese equities"] - 0.0163) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"US equities"] - 0.0105) < epsilon)
  expect_true(abs(attribution_results$Selection[3,"Total"] - 0.0358) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[4,"UK equities"] - (-0.0026)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Japanese equities"] - (-0.0069)) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"US equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[4,"Total"] - (-0.0095)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[4,"UK equities"] - 0.0162) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Japanese equities"] - (-0.0097)) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"US equities"] - 0.03) < epsilon)
  expect_true(abs(attribution_results$Selection[4,"Total"] - 0.0365) < epsilon)
  
  #Four quarter totals
  expect_true(abs(attribution_results$Allocation[5,"UK equities"] - 0.0167) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Japanese equities"] - (-0.0055)) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"US equities"] - 0.0011) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.0124) < epsilon)
  
  expect_true(abs(attribution_results$Selection[5,"UK equities"] - 0.0785) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Japanese equities"] - 0.0016) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"US equities"] - 0.0402) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1203) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)

test_that("Multi-period attribution example in Exhibit 8.10 with Davies & Laker linking" , {
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
  
  # The parameters of 'bf', 'method' and 'adjusted' don't apply to Davies Laker linking algorithm. By default it uses BHB.
  attribution_results = Attribution(Rp, Wp, Rb, Wb, linking = "davies.laker")
  
  #Four quarter totals (component-level multi-period totals are not computed by this algorithm)
  expect_true(abs(attribution_results$`Excess returns`[5,"Arithmetic"] - 0.1327) < epsilon)
  expect_true(abs(attribution_results$Allocation[5,"Total"] - 0.0117) < epsilon)
  expect_true(abs(attribution_results$Selection[5,"Total"] - 0.1318) < epsilon)
  expect_true(abs(attribution_results$Interaction[5,"Total"] - (-0.0108)) < epsilon)
}
)
