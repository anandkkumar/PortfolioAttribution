library(PortfolioAttribution)
library(testthat)

context("Geometric Multi-currency Attribution Examples from Pratical Portfolio Performance Management and Attribution by Carl Bacon (2008)")
epsilon = 5e-5

cnames = c("UK equities", "Japanese equities", "US equities")
Wp = c(0.40, 0.30, 0.30)
Wb = c(0.40, 0.20, 0.40)

# Example using one-period time series data
Rp = xts(matrix(c(NA, NA, NA, 0.20, 0.047, 0.28),ncol=3,byrow=TRUE,
                dimnames=list(c("Rp", "Rp"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rb = xts(matrix(c(NA, NA, NA, 0.10, 0.056, 0.296),ncol=3,byrow=TRUE,
                dimnames=list(c("Rb", "Rb"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rpl = xts(matrix(c(NA, NA, NA, 0.20, -0.05, 0.06),ncol=3,byrow=TRUE,
                dimnames=list(c("Rp", "Rp"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rbl = xts(matrix(c(NA, NA, NA, 0.10, -0.04, 0.08),ncol=3,byrow=TRUE,
                dimnames=list(c("Rb", "Rb"),cnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rbh = xts(matrix(c(NA, NA, NA, 0.10, -0.03, 0.10),ncol=3,byrow=TRUE,
                 dimnames=list(c("Rb", "Rb"),cnames)), 
          order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))


test_that("Multicurrency Geometric Example using data in Tables 6.7 and 6.8" , {
  attribution_results = Attribution.geometric(Rp, Wp, Rb, Wb, Rpl = Rpl, Rbl = Rbl, Rbh = Rbh)
  
  expect_true(abs(attribution_results$Selection[2,"UK.equities"] - 0.038) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese.equities"] - (-0.0029)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US.equities"] - (-0.0057)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0295) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK.equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese.equities"] - (-0.0088)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US.equities"] - (-0.0034)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.0122)) < epsilon)
  
  expect_true(abs(attribution_results$`Currency management`[2,"Hedging"] - (0.00095147)) < epsilon)
  expect_true(abs(attribution_results$`Currency management`[2,"Currency attribution"] - (-0.01040392)) < epsilon)
  
  expect_true(abs(attribution_results$`Excess returns`[2,"Geometric"] - 0.0073) < epsilon)
}
)


fcnames = c("Sterling", "Yen", "Dollar")
Wpf = c(0.20, -0.15, -0.05)
Wbf = c(0.30, -0.10, -0.20)

S = xts(matrix(c(NA, NA, NA, 1.0, 1.0, 1.0, 1.0, 1.1, 1.2),ncol=3,byrow=TRUE,
                       dimnames=list(c("SpotRates", "SpotRates", "SpotRates"),fcnames)), 
                order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4"), as.yearqtr("2018 Q1")))
Fp = xts(matrix(c(NA, NA, NA, NA, NA, NA, 1.0, 1.004566, 1.025641),ncol=3,byrow=TRUE,
                          dimnames=list(c("ForwardRates", "ForwardRates", "ForwardRates"),fcnames)), 
                   order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4"), as.yearqtr("2018 Q1")))
Fb = xts(matrix(c(NA, NA, NA, NA, NA, NA, 1.0, 1.010379, 1.018503),ncol=3,byrow=TRUE,
                dimnames=list(c("ForwardRates", "ForwardRates", "ForwardRates"),fcnames)), 
         order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4"), as.yearqtr("2018 Q1")))


test_that("Multicurrency Geometric Example using data in Tables 6.9 with forward contracts with main API" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, Wpf, Wbf, S, Fp, Fb, Rpl, Rbl, Rbh, geometric = TRUE)
  
  expect_true(abs(attribution_results$Selection[2,"UK.equities"] - 0.038) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese.equities"] - (-0.0029)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US.equities"] - (-0.0057)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0295) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK.equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese.equities"] - (-0.0088)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US.equities"] - (-0.0034)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.0122)) < epsilon)
  
  expect_true(abs(attribution_results$`Currency management`[2,"Hedging"] - (0.00095147)) < epsilon)
  expect_true(abs(attribution_results$`Currency management`[2,"Currency attribution"] - (0.008879913)) < epsilon)
  
  expect_true(abs(attribution_results$`Excess returns`[2,"Geometric"] - 0.02689563) < epsilon)
}
)

test_that("Multicurrency Geometric Example using data in Tables 6.9 with forward contracts using direct API" , {
  attribution_results = Attribution.geometric(Rp, Wp, Rb, Wb, Wpf, Wbf, S, Fp, Fb, Rpl, Rbl, Rbh)
  
  expect_true(abs(attribution_results$Selection[2,"UK.equities"] - 0.038) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese.equities"] - (-0.0029)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US.equities"] - (-0.0057)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0295) < epsilon)
  
  expect_true(abs(attribution_results$Allocation[2,"UK.equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese.equities"] - (-0.0088)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US.equities"] - (-0.0034)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.0122)) < epsilon)
  
  expect_true(abs(attribution_results$`Currency management`[2,"Hedging"] - (0.00095147)) < epsilon)
  expect_true(abs(attribution_results$`Currency management`[2,"Currency attribution"] - (0.008879913)) < epsilon)
  
  expect_true(abs(attribution_results$`Excess returns`[2,"Geometric"] - 0.02689563) < epsilon)
}
)
