library(PortfolioAttribution)
library(testthat)

context("BHB & BF Examples from Pratical Portfolio Performance Management and Attribution by Carl Bacon (2008)")

cnames = c("UK equities", "Japanese equities", "US equities")

Wp = c(0.40, 0.30, 0.30)
Wb = c(0.40, 0.20, 0.40)

epsilon = 1e-10

# Example using one-period time series data
Rp = xts(matrix(c(0.20, -0.05, 0.06, 0, 0, 0),ncol=3,byrow=TRUE,
                dimnames=list(c("Rp", "Rp"),cnames)), 
         order.by=c(Sys.yearqtr(), as.yearqtr(as.Date(as.yearqtr(Sys.Date()))-1)))
Rb = xts(matrix(c(0.10, -0.04, 0.08, 0, 0, 0),ncol=3,byrow=TRUE,
                dimnames=list(c("Rb", "Rb"),cnames)), 
         order.by=c(Sys.yearqtr(), as.yearqtr(as.Date(as.yearqtr(Sys.Date()))-1)))

test_that("BHB Example in Table 5.2 with time series data" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb,  bf = FALSE, method="none", geometric = FALSE)
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.004)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - (-0.008)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - (-0.008)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.03) < epsilon)
  
  expect_true(abs(attribution_results$Interaction[2,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"Japanese equities"] - (-0.001)) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"US equities"] - 0.002) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"Total"] - 0.001) < epsilon)
}
)

test_that("BF Example in Table 5.3 with time series data" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="none", geometric = FALSE)
  expect_true(attribution_results$Allocation[2,"UK equities"] - 0 < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.002))  < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - (-0.008))  < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.03) < epsilon)
  
  expect_true(abs(attribution_results$Interaction[2,"UK equities"] - 0)  < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"Japanese equities"] - (-0.001)) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"US equities"] - 0.002) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"Total"] - 0.001) < epsilon)
}
)

test_that("BF Example in Table 5.4 for top-down approach with time series data" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="top.down", geometric = FALSE)
  expect_true(attribution_results$Allocation[2,"UK equities"] - 0 < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.003))  < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - (-0.006))  < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.031) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)


# Example with single observation data
Rp = matrix(c(0.20, -0.05, 0.06),ncol=3,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rp = checkData(Rp)
Rb = matrix(c(0.10, -0.04, 0.08),ncol=3,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rb = checkData(Rb)

test_that("BHB Example in Table 5.2 with single observation data" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = FALSE, method="none", geometric = FALSE)
  expect_true(abs(attribution_results$Allocation[,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"Japanese equities"] - (-0.004)) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"US equities"] - (-0.008)) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[,"US equities"] - (-0.008)) < epsilon)
  expect_true(abs(attribution_results$Selection[,"Total"] - 0.03) < epsilon)
  
  expect_true(abs(attribution_results$Interaction[,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Interaction[,"Japanese equities"] - (-0.001)) < epsilon)
  expect_true(abs(attribution_results$Interaction[,"US equities"] - 0.002) < epsilon)
  expect_true(abs(attribution_results$Interaction[,"Total"] - 0.001) < epsilon)
}
)

test_that("BHB Example for top-down approach with single observation data" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, bf = FALSE, method="top.down", geometric = FALSE)
  expect_true(abs(attribution_results$Allocation[,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"Japanese equities"] - (-0.004)) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"US equities"] - (-0.008)) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[,"Total"] - 0.031) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)

test_that("BF Example in Table 5.3 with single observation data" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="none", bf = TRUE, geometric = FALSE)
  expect_true(abs(attribution_results$Allocation[,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[,"Japanese equities"] - (-0.002)) < epsilon)
  expect_true(abs(attribution_results$Selection[,"US equities"] - (-0.008)) < epsilon)
  expect_true(abs(attribution_results$Selection[,"Total"] - 0.03) < epsilon)
  
  expect_true(abs(attribution_results$Interaction[,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Interaction[,"Japanese equities"] - (-0.001)) < epsilon)
  expect_true(abs(attribution_results$Interaction[,"US equities"] - 0.002) < epsilon)
  expect_true(abs(attribution_results$Interaction[,"Total"] - 0.001) < epsilon)
}
)

test_that("BF Example in Table 5.4 for top-down approach with single observation data" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="top.down", bf = TRUE, geometric = FALSE)
  expect_true(abs(attribution_results$Allocation[,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"Japanese equities"] - (-0.0104)) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"US equities"] - (-0.0016)) < epsilon)
  expect_true(abs(attribution_results$Allocation[,"Total"] - (-0.012)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[,"Japanese equities"] - (-0.003)) < epsilon)
  expect_true(abs(attribution_results$Selection[,"US equities"] - (-0.006)) < epsilon)
  expect_true(abs(attribution_results$Selection[,"Total"] - 0.031) < epsilon)
  
  expect_null(attribution_results$Interaction)
}
)
