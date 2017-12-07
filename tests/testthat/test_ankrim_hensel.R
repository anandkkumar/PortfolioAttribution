library(PortfolioAttribution)
library(testthat)

context("Multi-currency Attribution Examples from Pratical Portfolio Performance Management and Attribution by Carl Bacon (2008)")
epsilon = 5e-5

cnames = c("UK equities", "Japanese equities", "US equities")
Wp = c(0.40, 0.30, 0.30)
Wb = c(0.40, 0.20, 0.40)
Wpf = c(0.20, -0.15, -0.05)
Wbf = c(0.30, -0.10, -0.20)

# Example using one-period time series data
Rp = xts(matrix(c(NA, NA, NA, 0.20, 0.045, 0.272),ncol=3,byrow=TRUE,
                dimnames=list(c("Rp", "Rp"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rb = xts(matrix(c(NA, NA, NA, 0.10, 0.056, 0.296),ncol=3,byrow=TRUE,
                dimnames=list(c("Rb", "Rb"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
S = xts(matrix(c(NA, NA, NA, 1.0, 1.0, 1.0, 1.0, 1.1, 1.2),ncol=3,byrow=TRUE,
               dimnames=list(c("SpotRates", "SpotRates", "SpotRates"),cnames)), 
        order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4"), as.yearqtr("2018 Q1")))
Fp = xts(matrix(c(NA, NA, NA, NA, NA, NA, 1.0, 1.01, 1.02),ncol=3,byrow=TRUE,
               dimnames=list(c("ForwardRates", "ForwardRates", "ForwardRates"),cnames)), 
        order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4"), as.yearqtr("2018 Q1")))
Fb = xts(matrix(c(NA, NA, NA, NA, NA, NA, 1.0, 1.01, 1.02),ncol=3,byrow=TRUE,
                dimnames=list(c("ForwardRates", "ForwardRates", "ForwardRates"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4"), as.yearqtr("2018 Q1")))


test_that("Ankrim-Hensel Example in Table 6.2" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, Wpf, Wbf, S, Fp, Fb, bf=FALSE, method="none", linking = "none", geometric = FALSE)
  expect_true(abs(attribution_results$Allocation[2,"UK.equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese.equities"] - (-0.01136)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US.equities"] - (-0.00264)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.014)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK.equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese.equities"] - (-0.0022)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US.equities"] - (-0.0096)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0282) < epsilon)
  
  expect_true(abs(attribution_results$Interaction[2,"UK.equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"Japanese.equities"] - (-0.0011)) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"US.equities"] - 0.0024) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"Total"] - 0.0013) < epsilon)
  
  expect_true(abs(attribution_results$`Currency management`[2,"UK equities"] - 0.009) < epsilon)
  expect_true(abs(attribution_results$`Currency management`[2,"Japanese equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$`Currency management`[2,"US equities"] - 0.004) < epsilon)
  expect_true(abs(attribution_results$`Currency management`[2,"Total"] - 0.013) < epsilon)
  
  expect_true(abs(attribution_results$`Forward Premium`[2,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$`Forward Premium`[2,"Japanese equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$`Forward Premium`[2,"US equities"] - (-0.001)) < epsilon)
  expect_true(abs(attribution_results$`Forward Premium`[2,"Total"] - (-0.001)) < epsilon)
}
)
