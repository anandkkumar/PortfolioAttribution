library(PortfolioAttribution)
library(testthat)

context("BHB Example from Pratical Portfolio Performance Management and Attribution by Carl Bacon (2008)")

cnames = c("UK equities", "Japanese equities", "US equities")

Wp = c(0.40, 0.30, 0.30)
Wb = c(0.40, 0.20, 0.40)

# BHB Example using one-period time series data
Rp = xts(matrix(c(0.20, -0.05, 0.06, 0, 0, 0),ncol=3,byrow=TRUE,
                dimnames=list(c("Rp", "Rp"),cnames)), 
         order.by=c(Sys.yearqtr(), as.yearqtr(as.Date(as.yearqtr(Sys.Date()))-1)))
Rb = xts(matrix(c(0.10, -0.04, 0.08, 0, 0, 0),ncol=3,byrow=TRUE,
                dimnames=list(c("Rb", "Rb"),cnames)), 
         order.by=c(Sys.yearqtr(), as.yearqtr(as.Date(as.yearqtr(Sys.Date()))-1)))

test_that("BHB Example in Table 5.2 with time series data" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="none", linking = "none", geometric = FALSE)
  expect_true(round(attribution_results$Allocation[2,"UK.equities"], 3) == 0)
  expect_true(round(attribution_results$Allocation[2,"Japanese.equities"], 3) == -0.004)
  expect_true(round(attribution_results$Allocation[2,"US.equities"],3) == -0.008)
  expect_true(round(attribution_results$Allocation[2,"Total"],3) == -0.012)
  
  expect_true(round(attribution_results$Selection[2,"UK.equities"], 3) == 0.04)
  expect_true(round(attribution_results$Selection[2,"Japanese.equities"], 3) == -0.002)
  expect_true(round(attribution_results$Selection[2,"US.equities"],3) == -0.008)
  expect_true(round(attribution_results$Selection[2,"Total"],3) == 0.03)
  
  expect_true(round(attribution_results$Interaction[2,"UK.equities"], 3) == 0)
  expect_true(round(attribution_results$Interaction[2,"Japanese.equities"], 3) == -0.001)
  expect_true(round(attribution_results$Interaction[2,"US.equities"],3) == 0.002)
  expect_true(round(attribution_results$Interaction[2,"Total"],3) == 0.001)
}
)


# BHB Example with single observation data
Rp = matrix(c(0.20, -0.05, 0.06),ncol=3,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rp = checkData(Rp)
Rb = matrix(c(0.10, -0.04, 0.08),ncol=3,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rb = checkData(Rb)

test_that("BHB Example  in Table 5.2 with single observation data" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="none", linking = "none", geometric = FALSE)
  expect_true(round(attribution_results$Allocation[,"UK.equities"], 3) == 0)
  expect_true(round(attribution_results$Allocation[,"Japanese.equities"], 3) == -0.004)
  expect_true(round(attribution_results$Allocation[,"US.equities"],3) == -0.008)
  expect_true(round(attribution_results$Allocation[,"Total"],3) == -0.012)
  
  expect_true(round(attribution_results$Selection[,"UK.equities"], 3) == 0.04)
  expect_true(round(attribution_results$Selection[,"Japanese.equities"], 3) == -0.002)
  expect_true(round(attribution_results$Selection[,"US.equities"],3) == -0.008)
  expect_true(round(attribution_results$Selection[,"Total"],3) == 0.03)
  
  expect_true(round(attribution_results$Interaction[,"UK.equities"], 3) == 0)
  expect_true(round(attribution_results$Interaction[,"Japanese.equities"], 3) == -0.001)
  expect_true(round(attribution_results$Interaction[,"US.equities"],3) == 0.002)
  expect_true(round(attribution_results$Interaction[,"Total"],3) == 0.001)
}
)
