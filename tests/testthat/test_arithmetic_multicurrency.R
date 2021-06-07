library(PortfolioAttribution)
library(testthat)

context("Multi-currency Attribution Examples from Practical Portfolio Performance Management and Attribution by Carl Bacon (2008)")
epsilon = 5e-5

cnames = c("UK equities", "Japanese equities", "US equities")
Wp = c(0.40, 0.30, 0.30)
Wb = c(0.40, 0.20, 0.40)
Wpf = c(0.20, -0.15, -0.05)
Wbf = c(0.30, -0.10, -0.20)

# Example using one-period time series data
Rp = xts::xts(matrix(c(NA, NA, NA, 0.20, 0.045, 0.272),ncol=3,byrow=TRUE,
                     dimnames=list(c("Rp", "Rp"),cnames)), 
              order.by=c(zoo::as.yearqtr("2017 Q3"), zoo::as.yearqtr("2017 Q4")))
Rb = xts::xts(matrix(c(NA, NA, NA, 0.10, 0.056, 0.296),ncol=3,byrow=TRUE,
                     dimnames=list(c("Rb", "Rb"),cnames)), 
              order.by=c(zoo::as.yearqtr("2017 Q3"), zoo::as.yearqtr("2017 Q4")))
S = xts::xts(matrix(c(NA, NA, NA, 1.0, 1.0, 1.0, 1.0, 1.1, 1.2),ncol=3,byrow=TRUE,
                    dimnames=list(c("SpotRates", "SpotRates", "SpotRates"),cnames)), 
             order.by=c(zoo::as.yearqtr("2017 Q3"), zoo::as.yearqtr("2017 Q4"), zoo::as.yearqtr("2018 Q1")))
Fp = xts::xts(matrix(c(NA, NA, NA, NA, NA, NA, 1.0, 1.01, 1.02),ncol=3,byrow=TRUE,
                     dimnames=list(c("ForwardRates", "ForwardRates", "ForwardRates"),cnames)), 
              order.by=c(zoo::as.yearqtr("2017 Q3"), zoo::as.yearqtr("2017 Q4"), zoo::as.yearqtr("2018 Q1")))
Fb = xts::xts(matrix(c(NA, NA, NA, NA, NA, NA, 1.0, 1.01, 1.02),ncol=3,byrow=TRUE,
                     dimnames=list(c("ForwardRates", "ForwardRates", "ForwardRates"),cnames)), 
              order.by=c(zoo::as.yearqtr("2017 Q3"), zoo::as.yearqtr("2017 Q4"), zoo::as.yearqtr("2018 Q1")))


test_that("Ankrim-Hensel Example in Table 6.2" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, Wpf, Wbf, S, Fp, Fb, bf=FALSE, method="none", 
                                    currency_method = "ankrim.hensel", geometric = FALSE)
  expect_true(abs(attribution_results$Allocation[2,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Japanese equities"] - (-0.01136)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"US equities"] - (-0.00264)) < epsilon)
  expect_true(abs(attribution_results$Allocation[2,"Total"] - (-0.014)) < epsilon)
  
  expect_true(abs(attribution_results$Selection[2,"UK equities"] - 0.04) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Japanese equities"] - (-0.0022)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"US equities"] - (-0.0096)) < epsilon)
  expect_true(abs(attribution_results$Selection[2,"Total"] - 0.0282) < epsilon)
  
  expect_true(abs(attribution_results$Interaction[2,"UK equities"] - 0) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"Japanese equities"] - (-0.0011)) < epsilon)
  expect_true(abs(attribution_results$Interaction[2,"US equities"] - 0.0024) < epsilon)
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

context("Multi-currency Attribution Examples with 'standard' currency attribution methodology")

data(sample_data, package = "PortfolioAttribution")

wp = as.numeric(simple_multi_curr_portf_1[1,])
Rpl = data.frame(simple_multi_curr_portf_1[2,], row.names = Sys.Date(), check.names = FALSE)
Rp = data.frame(simple_multi_curr_portf_1[3,], row.names = Sys.Date(), check.names = FALSE)
wb = as.numeric(simple_multi_curr_portf_1[4,])
Rbl = data.frame(simple_multi_curr_portf_1[5,], row.names = Sys.Date(), check.names = FALSE)
Rb = data.frame(simple_multi_curr_portf_1[6,], row.names = Sys.Date(), check.names = FALSE)


test_that("Single period multi-currency top-down arithmetic attribuition with 'standard' methodology works as expected" , {
  attribution_results_local = Attribution(Rpl, wp, Rbl, wb, method = "top.down", contribution = TRUE)
  attribution_results_base = Attribution(Rp, wp, Rb, wb, method = "top.down", contribution = TRUE)
  
  attribution_results = Attribution(Rp, wp, Rb, wb, Rpl=Rpl, Rbl=Rbl, method = "top.down", currency_method = "standard", contribution = TRUE)
  
  expect_true(all.equal(as.numeric(attribution_results_local$Allocation["Total",]), as.numeric(attribution_results$Allocation["Total",])))
  expect_true(all.equal(as.numeric(attribution_results_local$Selection["Total",]), as.numeric(attribution_results$Selection["Total",])))
  
  expect_true(all.equal(
    as.numeric(attribution_results_base$Allocation["Total",]) + as.numeric(attribution_results_base$Selection["Total",]), 
    as.numeric(attribution_results$Allocation["Total",]) + 
      as.numeric(attribution_results$Selection["Total",]) + 
      as.numeric(attribution_results$Currency["Total",])))
  
  expect_true(all.equal(
    as.numeric(attribution_results_base$Allocation["Total",]) + as.numeric(attribution_results_base$Selection["Total",]) -
    as.numeric(attribution_results_local$Allocation["Total",]) - as.numeric(attribution_results_local$Selection["Total",]), 
      as.numeric(attribution_results$Currency["Total",])))
  
  expect_true(all.equal(as.numeric(attribution_results_base$`Portfolio contribution to return`["Total",]), 
                        as.numeric(attribution_results$`Portfolio contribution to return`["Total",])))
  expect_true(all.equal(as.numeric(attribution_results_base$`Benchmark contribution to return`["Total",]), 
                        as.numeric(attribution_results$`Benchmark contribution to return`["Total",])))
  
  expect_true(all.equal(as.numeric(attribution_results_base$`Excess returns`["Cumulative Return",]), 
                        as.numeric(attribution_results$Allocation[1,"Total"]) + 
                          as.numeric(attribution_results$Selection[1,"Total"]) + 
                          as.numeric(attribution_results$Currency[1,"Total"])))
})

test_that("Single period multi-currency bottom-up arithmetic attribuition with 'standard' methodology works as expected" , {
  attribution_results_local = Attribution(Rpl, wp, Rbl, wb, method = "bottom.up", contribution = TRUE)
  attribution_results_base = Attribution(Rp, wp, Rb, wb, method = "bottom.up", contribution = TRUE)
  
  attribution_results = Attribution(Rp, wp, Rb, wb, Rpl=Rpl, Rbl=Rbl, method = "bottom.up", currency_method = "standard", contribution = TRUE)
  
  expect_true(all.equal(as.numeric(attribution_results_local$Allocation["Total",]), as.numeric(attribution_results$Allocation["Total",])))
  expect_true(all.equal(as.numeric(attribution_results_local$Selection["Total",]), as.numeric(attribution_results$Selection["Total",])))
  
  expect_true(all.equal(
    as.numeric(attribution_results_base$Allocation["Total",]) + as.numeric(attribution_results_base$Selection["Total",]), 
    as.numeric(attribution_results$Allocation["Total",]) + 
      as.numeric(attribution_results$Selection["Total",]) + 
      as.numeric(attribution_results$Currency["Total",])))
  
  expect_true(all.equal(
    as.numeric(attribution_results_base$Allocation["Total",]) + as.numeric(attribution_results_base$Selection["Total",]) -
      as.numeric(attribution_results_local$Allocation["Total",]) - as.numeric(attribution_results_local$Selection["Total",]), 
    as.numeric(attribution_results$Currency["Total",])))
  
  expect_true(all.equal(as.numeric(attribution_results_base$`Portfolio contribution to return`["Total",]), 
                        as.numeric(attribution_results$`Portfolio contribution to return`["Total",])))
  expect_true(all.equal(as.numeric(attribution_results_base$`Benchmark contribution to return`["Total",]), 
                        as.numeric(attribution_results$`Benchmark contribution to return`["Total",])))
  
  expect_true(all.equal(as.numeric(attribution_results_base$`Excess returns`["Cumulative Return",]), 
                        as.numeric(attribution_results$Allocation[1,"Total"]) + 
                          as.numeric(attribution_results$Selection[1,"Total"]) + 
                          as.numeric(attribution_results$Currency[1,"Total"])))
})

test_that("Single period multi-currency three-factor arithmetic attribuition with 'standard' methodology works as expected" , {
  attribution_results_local = Attribution(Rpl, wp, Rbl, wb, method = "none", contribution = TRUE)
  attribution_results_base = Attribution(Rp, wp, Rb, wb, method = "none", contribution = TRUE)
  
  attribution_results = Attribution(Rp, wp, Rb, wb, Rpl=Rpl, Rbl=Rbl, method = "none", currency_method = "standard", contribution = TRUE)
  
  expect_true(all.equal(as.numeric(attribution_results_local$Allocation["Total",]), as.numeric(attribution_results$Allocation["Total",])))
  expect_true(all.equal(as.numeric(attribution_results_local$Selection["Total",]), as.numeric(attribution_results$Selection["Total",])))
  expect_true(all.equal(as.numeric(attribution_results_local$Interaction["Total",]), as.numeric(attribution_results$Interaction["Total",])))
  
  expect_true(all.equal(
    as.numeric(attribution_results_base$Allocation["Total",]) + 
      as.numeric(attribution_results_base$Selection["Total",]) +
      as.numeric(attribution_results_base$Interaction["Total",]), 
    as.numeric(attribution_results$Allocation["Total",]) + 
      as.numeric(attribution_results$Selection["Total",]) + 
      as.numeric(attribution_results$Interaction["Total",]) +
      as.numeric(attribution_results$Currency["Total",])))
  
  expect_true(all.equal(
    as.numeric(attribution_results_base$Allocation["Total",]) + 
      as.numeric(attribution_results_base$Selection["Total",]) + 
      as.numeric(attribution_results_base$Interaction["Total",]) -
      as.numeric(attribution_results_local$Allocation["Total",]) - 
      as.numeric(attribution_results_local$Selection["Total",]) -
      as.numeric(attribution_results_local$Interaction["Total",]), 
    as.numeric(attribution_results$Currency["Total",])))
  
  expect_true(all.equal(as.numeric(attribution_results_base$`Portfolio contribution to return`["Total",]), 
                        as.numeric(attribution_results$`Portfolio contribution to return`["Total",])))
  expect_true(all.equal(as.numeric(attribution_results_base$`Benchmark contribution to return`["Total",]), 
                        as.numeric(attribution_results$`Benchmark contribution to return`["Total",])))
  
  expect_true(all.equal(as.numeric(attribution_results_base$`Excess returns`["Cumulative Return",]), 
                        as.numeric(attribution_results$Allocation[1,"Total"]) + 
                          as.numeric(attribution_results$Selection[1,"Total"]) + 
                          as.numeric(attribution_results$Interaction[1,"Total"]) + 
                          as.numeric(attribution_results$Currency[1,"Total"])))
})


test_that("Multi period multi-currency top-down arithmetic attribuition with 'standard' methodology & different linking methods, works as expected" , {
  # GRAP linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "top.down", linking = "grap", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "top.down",  linking = "grap", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "top.down", currency_method = "standard", linking = "grap", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection -
      attribution_results_local$Allocation - attribution_results_local$Selection, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))

  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))

  epsilon = 2e-16
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
  
  
  # Carino linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "top.down", linking = "carino", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "top.down",  linking = "carino", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "top.down", currency_method = "standard", linking = "carino", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection -
      attribution_results_local$Allocation - attribution_results_local$Selection, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 9e-17
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
  
  
  # Menchero linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "top.down", linking = "menchero", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "top.down",  linking = "menchero", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "top.down", currency_method = "standard", linking = "menchero", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection -
      attribution_results_local$Allocation - attribution_results_local$Selection, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 1e-20
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
  
  
  # Frongello linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "top.down", linking = "frongello", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "top.down",  linking = "frongello", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "top.down", currency_method = "standard", linking = "frongello", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection -
      attribution_results_local$Allocation - attribution_results_local$Selection, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 2e-16
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
})

test_that("Multi period multi-currency bottom-up arithmetic attribuition with 'standard' methodology & different linking methods, works as expected" , {
  # GRAP linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "bottom.up", linking = "grap", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "bottom.up",  linking = "grap", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "bottom.up", currency_method = "standard", linking = "grap", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection -
      attribution_results_local$Allocation - attribution_results_local$Selection, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 2e-16
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
  
  
  # Carino linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "bottom.up", linking = "carino", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "bottom.up",  linking = "carino", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "bottom.up", currency_method = "standard", linking = "carino", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection -
      attribution_results_local$Allocation - attribution_results_local$Selection, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 2e-16
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
  
  
  # Menchero linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "bottom.up", linking = "menchero", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "bottom.up",  linking = "menchero", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "bottom.up", currency_method = "standard", linking = "menchero", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection -
      attribution_results_local$Allocation - attribution_results_local$Selection, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 1e-20
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
  
  
  # Frongello linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "bottom.up", linking = "frongello", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "bottom.up",  linking = "frongello", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "bottom.up", currency_method = "standard", linking = "frongello", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection -
      attribution_results_local$Allocation - attribution_results_local$Selection, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 2e-16
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
})

test_that("Multi period multi-currency three-factor arithmetic attribuition with 'standard' methodology & different linking methods, works as expected" , {
  # GRAP linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "none", linking = "grap", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "none",  linking = "grap", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "none", currency_method = "standard", linking = "grap", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  expect_true(all.equal(attribution_results_local$Interaction, attribution_results$Interaction))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection + attribution_results_base$Interaction, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Interaction + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection + attribution_results_base$Interaction -
      attribution_results_local$Allocation - attribution_results_local$Selection - + attribution_results_local$Interaction, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 2e-16
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) + 
                       as.numeric(attribution_results$Interaction["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
  
  
  # Carino linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "none", linking = "carino", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "none",  linking = "carino", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "none", currency_method = "standard", linking = "carino", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  expect_true(all.equal(attribution_results_local$Interaction, attribution_results$Interaction))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection + attribution_results_base$Interaction, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Interaction + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection + attribution_results_base$Interaction -
      attribution_results_local$Allocation - attribution_results_local$Selection - attribution_results_local$Interaction, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 9e-17
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) +
                       as.numeric(attribution_results$Interaction["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
  
  
  # Menchero linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "none", linking = "menchero", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "none",  linking = "menchero", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "none", currency_method = "standard", linking = "menchero", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  expect_true(all.equal(attribution_results_local$Interaction, attribution_results$Interaction))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection + attribution_results_base$Interaction, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Interaction + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection + attribution_results_base$Interaction -
      attribution_results_local$Allocation - attribution_results_local$Selection - attribution_results_local$Interaction, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 1e-20
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) +
                       as.numeric(attribution_results$Interaction["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
  
  
  # Frongello linking
  attribution_results_local = Attribution(multi_period_portf_4$Rpl, multi_period_portf_4$wp, 
                                          multi_period_portf_4$Rbl, multi_period_portf_4$wb, 
                                          method = "none", linking = "frongello", contribution = TRUE)
  attribution_results_base = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                         multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                         method = "none",  linking = "frongello", contribution = TRUE)
  
  attribution_results = Attribution(multi_period_portf_4$Rp, multi_period_portf_4$wp, 
                                    multi_period_portf_4$Rb, multi_period_portf_4$wb, 
                                    Rpl=multi_period_portf_4$Rpl, Rbl=multi_period_portf_4$Rbl, 
                                    method = "none", currency_method = "standard", linking = "frongello", contribution = TRUE)
  
  
  expect_true(all.equal(attribution_results_local$Allocation, attribution_results$Allocation))
  expect_true(all.equal(attribution_results_local$Selection, attribution_results$Selection))
  expect_true(all.equal(attribution_results_local$Interaction, attribution_results$Interaction))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection + attribution_results_base$Interaction, 
    attribution_results$Allocation + attribution_results$Selection + attribution_results$Interaction + attribution_results$Currency))
  
  expect_true(all.equal(
    attribution_results_base$Allocation + attribution_results_base$Selection + attribution_results_base$Interaction -
      attribution_results_local$Allocation - attribution_results_local$Selection - attribution_results_local$Interaction, 
    attribution_results$Currency))
  
  expect_true(all.equal(attribution_results_base$`Portfolio contribution to return`, 
                        attribution_results$`Portfolio contribution to return`))
  expect_true(all.equal(attribution_results_base$`Benchmark contribution to return`, 
                        attribution_results$`Benchmark contribution to return`))
  
  expect_true(all.equal(attribution_results_base$`Excess returns`, 
                        attribution_results$`Excess returns`))
  
  epsilon = 2e-16
  expect_true(abs(as.numeric(attribution_results$`Excess returns`["Cumulative Return",]) -
                    (as.numeric(attribution_results$Allocation["Total", "Total"]) +
                       as.numeric(attribution_results$Selection["Total", "Total"]) +
                       as.numeric(attribution_results$Interaction["Total", "Total"]) + 
                       as.numeric(attribution_results$Currency["Total", "Total"])) 
  ) < epsilon)
})
