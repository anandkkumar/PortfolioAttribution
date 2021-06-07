context("Examples for calculations with and without imputation of portfolio & benchmark returns")
epsilon = 1e-4

data(sample_data)

rp = PerformanceAnalytics::Return.portfolio(multi_period_portf_6$Rp, multi_period_portf_6$wp)
rb = PerformanceAnalytics::Return.portfolio(multi_period_portf_6$Rb, multi_period_portf_6$wb)
rb = magrittr::set_names(rb, "bmk.returns")
periodic_excess_return = PerformanceAnalytics::Return.excess(rp, rb)

rp_cumulative = PerformanceAnalytics::Return.cumulative(rp)
rb_cumulative = PerformanceAnalytics::Return.cumulative(rb)
cumulative_excess_return = rp_cumulative - rb_cumulative

rp_annualized = PerformanceAnalytics::Return.annualized(rp)
rb_annualized = PerformanceAnalytics::Return.annualized(rb)
annualized_excess_return = PerformanceAnalytics::Return.annualized.excess(rp, rb, geometric = FALSE)

attribution_results_no_imputation = Attribution(multi_period_portf_6$Rp, 
                                                multi_period_portf_6$wp, 
                                                multi_period_portf_6$Rb,
                                                multi_period_portf_6$wb, contribution = TRUE, annualization = "standard", 
                                                impute_returns = FALSE)

test_that("arithmetic attribution without return imputation gives expected results" , {
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results_no_imputation$`Excess returns`[1:(NROW(attribution_results_no_imputation$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results_no_imputation$Allocation["Total", -NCOL(attribution_results_no_imputation$Allocation)])
  total_selection_effects = sum(attribution_results_no_imputation$Selection["Total", -NCOL(attribution_results_no_imputation$Selection)])
  total_interaction_effects = sum(attribution_results_no_imputation$Interaction["Total", -NCOL(attribution_results_no_imputation$Interaction)])
  total_effects = total_allocation_effects + total_selection_effects + total_interaction_effects
  
  epsilon = 7e-18
  expect_equal(total_allocation_effects, attribution_results_no_imputation$Allocation["Total", "Total"], tolerance = epsilon)
  epsilon = 3e-17
  expect_equal(total_selection_effects, attribution_results_no_imputation$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 1e-20
  expect_equal(total_interaction_effects, attribution_results_no_imputation$Interaction["Total", "Total"], tolerance = epsilon)
  epsilon = 3e-16
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(
    attribution_results_no_imputation$
      `Portfolio contribution to return`[1:(NROW(attribution_results_no_imputation$`Portfolio contribution to return`)-2),
                                         -NCOL(attribution_results_no_imputation$`Portfolio contribution to return`)])) -
      as.numeric(rowSums(multi_period_portf_6$Rp * zoo::coredata(multi_period_portf_6$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(
    attribution_results_no_imputation$
      `Benchmark contribution to return`[1:(NROW(attribution_results_no_imputation$`Benchmark contribution to return`)-2),
                                         -NCOL(attribution_results_no_imputation$`Benchmark contribution to return`)])) -
      as.numeric(rowSums(multi_period_portf_6$Rb * zoo::coredata(multi_period_portf_6$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 3e-16
  expect_equal(as.numeric(attribution_results_no_imputation$`Excess returns`[NROW(attribution_results_no_imputation$`Excess returns`), ]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results_no_imputation$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results_no_imputation$Selection["Annualized", "Total"])
  total_annualized_interaction_effects = sum(attribution_results_no_imputation$Interaction["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects + total_annualized_interaction_effects
  
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  expect_equal(sum(
    attribution_results_no_imputation$`Portfolio contribution to return`["Annualized",
                                                                         -NCOL(attribution_results_no_imputation$`Portfolio contribution to return`)]), 
    as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(
    attribution_results_no_imputation$`Benchmark contribution to return`["Annualized",
                                                                         -NCOL(attribution_results_no_imputation$`Benchmark contribution to return`)]), 
               as.numeric(rb_annualized), tolerance = epsilon)
  expect_equal(attribution_results_no_imputation$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results_no_imputation$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})

attribution_results_imputed = Attribution(multi_period_portf_6$Rp, 
                                          multi_period_portf_6$wp, 
                                          multi_period_portf_6$Rb,
                                          multi_period_portf_6$wb, contribution = TRUE, annualization = "standard", 
                                          impute_returns = TRUE)


test_that("arithmetic attribution with return imputation gives expected results" , {
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results_imputed$`Excess returns`[1:(NROW(attribution_results_imputed$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results_imputed$Allocation["Total", -NCOL(attribution_results_imputed$Allocation)])
  total_selection_effects = sum(attribution_results_imputed$Selection["Total", -NCOL(attribution_results_imputed$Selection)])
  total_interaction_effects = sum(attribution_results_imputed$Interaction["Total", -NCOL(attribution_results_imputed$Interaction)])
  total_effects = total_allocation_effects + total_selection_effects + total_interaction_effects
  
  epsilon = 1e-20
  expect_equal(total_allocation_effects, attribution_results_imputed$Allocation["Total", "Total"], tolerance = epsilon)
  epsilon = 3e-17
  expect_equal(total_selection_effects, attribution_results_imputed$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 7e-18
  expect_equal(total_interaction_effects, attribution_results_imputed$Interaction["Total", "Total"], tolerance = epsilon)
  epsilon = 3e-16
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(
    attribution_results_imputed$`Portfolio contribution to return`[1:(NROW(attribution_results_imputed$`Portfolio contribution to return`)-2),
                                                                   -NCOL(attribution_results_no_imputation$`Portfolio contribution to return`)])) -
                        as.numeric(rowSums(multi_period_portf_6$Rp * zoo::coredata(multi_period_portf_6$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(
    attribution_results_imputed$`Benchmark contribution to return`[1:(NROW(attribution_results_imputed$`Benchmark contribution to return`)-2),
                                                                   -NCOL(attribution_results_no_imputation$`Benchmark contribution to return`)])) -
                        as.numeric(rowSums(multi_period_portf_6$Rb * zoo::coredata(multi_period_portf_6$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 3e-16
  expect_equal(as.numeric(attribution_results_imputed$`Excess returns`[NROW(attribution_results_imputed$`Excess returns`), ]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results_imputed$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results_imputed$Selection["Annualized", "Total"])
  total_annualized_interaction_effects = sum(attribution_results_imputed$Interaction["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects + total_annualized_interaction_effects
  
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  expect_equal(sum(
    attribution_results_imputed$`Portfolio contribution to return`["Annualized",
                                                                   -NCOL(attribution_results_no_imputation$`Portfolio contribution to return`)]), 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(
    attribution_results_imputed$`Benchmark contribution to return`["Annualized",
                                                                   -NCOL(attribution_results_no_imputation$`Benchmark contribution to return`)]), 
    as.numeric(rb_annualized), tolerance = epsilon)
  expect_equal(attribution_results_imputed$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results_imputed$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})

test_that("comparison of portfolio/benchmark returns with and without imputation are identical" , {
  epsilon = 1e-20
  # The attribution effects should NOT be equal
  expect_true(!all(abs(attribution_results_no_imputation$Allocation - attribution_results_imputed$Allocation) < epsilon))
  expect_true(!all(abs(attribution_results_no_imputation$Selection - attribution_results_imputed$Selection) < epsilon))
  expect_true(!all(abs(attribution_results_no_imputation$Interaction - attribution_results_imputed$Interaction) < epsilon))

  # All return & contribution results should be equal
  expect_true(all(abs(attribution_results_no_imputation$`Excess returns` - attribution_results_imputed$`Excess returns`) < epsilon))
  expect_true(all(abs(attribution_results_no_imputation$`Portfolio contribution to return` - attribution_results_imputed$`Portfolio contribution to return`) < epsilon))
  expect_true(all(abs(attribution_results_no_imputation$`Benchmark contribution to return` - attribution_results_imputed$`Benchmark contribution to return`) < epsilon))
  expect_true(all(abs(attribution_results_no_imputation$`Portfolio returns` - attribution_results_imputed$`Portfolio returns`) < epsilon))
  expect_true(all(abs(attribution_results_no_imputation$`Benchmark returns` - attribution_results_imputed$`Benchmark returns`) < epsilon))
})


