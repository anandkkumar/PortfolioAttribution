library(PortfolioAttribution)

context("Examples of Annualization methods")

data(sample_data)

rp = PerformanceAnalytics::Return.portfolio(multi_period_portf_3$Rp, multi_period_portf_3$wp)
rb = PerformanceAnalytics::Return.portfolio(multi_period_portf_3$Rb, multi_period_portf_3$wb)
rb = magrittr::set_names(rb, "bmk.returns")
periodic_excess_return = PerformanceAnalytics::Return.excess(rp, rb)

rp_cumulative = PerformanceAnalytics::Return.cumulative(rp)
rb_cumulative = PerformanceAnalytics::Return.cumulative(rb)
cumulative_excess_return = rp_cumulative - rb_cumulative

rp_annualized = PerformanceAnalytics::Return.annualized(rp)
rb_annualized = PerformanceAnalytics::Return.annualized(rb)
annualized_excess_return = PerformanceAnalytics::Return.annualized.excess(rp, rb, geometric = FALSE)

test_that("3-factor arithmetic attribution with proportional annualization gives expected results" , {
  attribution_results = Attribution(multi_period_portf_3$Rp, 
                                    multi_period_portf_3$wp, 
                                    multi_period_portf_3$Rb,
                                    multi_period_portf_3$wb, contribution = TRUE, annualization = "proportional")
  
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results$`Excess returns`[1:(NROW(attribution_results$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results$Allocation["Total", -NCOL(attribution_results$Allocation)])
  total_selection_effects = sum(attribution_results$Selection["Total", -NCOL(attribution_results$Selection)])
  total_interaction_effects = sum(attribution_results$Interaction["Total", -NCOL(attribution_results$Interaction)])
  total_effects = total_allocation_effects + total_selection_effects + total_interaction_effects
  
  epsilon = 1e-20
  expect_equal(total_allocation_effects, attribution_results$Allocation["Total", "Total"], tolerance = epsilon)
  expect_equal(total_selection_effects, attribution_results$Selection["Total", "Total"], tolerance = epsilon)
  expect_equal(total_interaction_effects, attribution_results$Interaction["Total", "Total"], tolerance = epsilon)
  epsilon = 8e-13
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Portfolio contribution to return`[1:(NROW(attribution_results$`Portfolio contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Portfolio contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_3$Rp * zoo::coredata(multi_period_portf_3$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Benchmark contribution to return`[1:(NROW(attribution_results$`Benchmark contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Benchmark contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_3$Rb * zoo::coredata(multi_period_portf_3$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 6e-13
  expect_equal(as.numeric(attribution_results$`Excess returns`[NROW(attribution_results$`Excess returns`), ]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results$Selection["Annualized", "Total"])
  total_annualized_interaction_effects = sum(attribution_results$Interaction["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects + total_annualized_interaction_effects
  
  epsilon = 5e-13
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  epsilon = 3e-16
  expect_equal(sum(attribution_results$`Portfolio contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Portfolio contribution to return`)-1]), 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(attribution_results$`Benchmark contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Benchmark contribution to return`)-1]), 
               as.numeric(rb_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})

test_that("Top-down arithmetic attribution with proportional annualization gives expected results" , {
  attribution_results = Attribution(multi_period_portf_3$Rp, 
                                    multi_period_portf_3$wp, 
                                    multi_period_portf_3$Rb,
                                    multi_period_portf_3$wb, method = "top.down", contribution = TRUE, annualization = "proportional")
  
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results$`Excess returns`[1:(NROW(attribution_results$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results$Allocation["Total", -NCOL(attribution_results$Allocation)])
  total_selection_effects = sum(attribution_results$Selection["Total", -NCOL(attribution_results$Selection)])
  total_effects = total_allocation_effects + total_selection_effects
  
  epsilon = 1e-20
  expect_equal(total_allocation_effects, attribution_results$Allocation["Total", "Total"], tolerance = epsilon)
  expect_equal(total_selection_effects, attribution_results$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 8e-13
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Portfolio contribution to return`[1:(NROW(attribution_results$`Portfolio contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Portfolio contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_3$Rp * zoo::coredata(multi_period_portf_3$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Benchmark contribution to return`[1:(NROW(attribution_results$`Benchmark contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Benchmark contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_3$Rb * zoo::coredata(multi_period_portf_3$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 6e-13
  expect_equal(as.numeric(attribution_results$`Excess returns`[NROW(attribution_results$`Excess returns`),]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results$Selection["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects
  
  epsilon = 5e-13
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  epsilon = 3e-16
  expect_equal(sum(attribution_results$`Portfolio contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Portfolio contribution to return`)-1]), 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(attribution_results$`Benchmark contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Benchmark contribution to return`)-1]), 
               as.numeric(rb_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})

test_that("Bottom-up arithmetic attribution with proportional annualization gives expected results" , {
  attribution_results = Attribution(multi_period_portf_3$Rp, 
                                    multi_period_portf_3$wp, 
                                    multi_period_portf_3$Rb,
                                    multi_period_portf_3$wb, method = "bottom.up", contribution = TRUE, annualization = "proportional")
  
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results$`Excess returns`[1:(NROW(attribution_results$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results$Allocation["Total", -NCOL(attribution_results$Allocation)])
  total_selection_effects = sum(attribution_results$Selection["Total", -NCOL(attribution_results$Selection)])
  total_effects = total_allocation_effects + total_selection_effects
  
  epsilon = 9e-16
  expect_equal(total_allocation_effects, attribution_results$Allocation["Total", "Total"], tolerance = epsilon)
  epsilon = 1e-20
  expect_equal(total_selection_effects, attribution_results$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 8e-13
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Portfolio contribution to return`[1:(NROW(attribution_results$`Portfolio contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Portfolio contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_3$Rp * zoo::coredata(multi_period_portf_3$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Benchmark contribution to return`[1:(NROW(attribution_results$`Benchmark contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Benchmark contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_3$Rb * zoo::coredata(multi_period_portf_3$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 6e-13
  expect_equal(as.numeric(attribution_results$`Excess returns`[NROW(attribution_results$`Excess returns`),]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results$Selection["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects
  
  epsilon = 5e-13
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  epsilon = 3e-16
  expect_equal(sum(attribution_results$`Portfolio contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Portfolio contribution to return`)-1]), 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(attribution_results$`Benchmark contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Benchmark contribution to return`)-1]), 
               as.numeric(rb_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})

test_that("Top-down arithmetic attribution with standard annualization gives expected results" , {
  attribution_results = Attribution(multi_period_portf_3$Rp, 
                                    multi_period_portf_3$wp, 
                                    multi_period_portf_3$Rb,
                                    multi_period_portf_3$wb, method = "top.down", contribution = TRUE, annualization = "standard")
  
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results$`Excess returns`[1:(NROW(attribution_results$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results$Allocation["Total", -NCOL(attribution_results$Allocation)])
  total_selection_effects = sum(attribution_results$Selection["Total", -NCOL(attribution_results$Selection)])
  total_effects = total_allocation_effects + total_selection_effects
  
  epsilon = 1e-20
  expect_equal(total_allocation_effects, attribution_results$Allocation["Total", "Total"], tolerance = epsilon)
  expect_equal(total_selection_effects, attribution_results$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 8e-13
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Portfolio contribution to return`[1:(NROW(attribution_results$`Portfolio contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Portfolio contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_3$Rp * zoo::coredata(multi_period_portf_3$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Benchmark contribution to return`[1:(NROW(attribution_results$`Benchmark contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Benchmark contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_3$Rb * zoo::coredata(multi_period_portf_3$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 6e-13
  expect_equal(as.numeric(attribution_results$`Excess returns`[NROW(attribution_results$`Excess returns`),]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results$Selection["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects
  
  skip("Skipping these tests for now")
  epsilon = 5e-13
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_equal(sum(attribution_results$`Portfolio contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Portfolio contribution to return`)-1]), 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(attribution_results$`Benchmark contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Benchmark contribution to return`)-1]), 
               as.numeric(rb_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})

rp = PerformanceAnalytics::Return.portfolio(multi_period_portf_5$Rp, multi_period_portf_5$wp)
rb = PerformanceAnalytics::Return.portfolio(multi_period_portf_5$Rb, multi_period_portf_5$wb)
rb = magrittr::set_names(rb, "bmk.returns")
periodic_excess_return = PerformanceAnalytics::Return.excess(rp, rb)

rp_cumulative = PerformanceAnalytics::Return.cumulative(rp)
rb_cumulative = PerformanceAnalytics::Return.cumulative(rb)
cumulative_excess_return = rp_cumulative - rb_cumulative

rp_annualized = PerformanceAnalytics::Return.annualized(rp)
rb_annualized = PerformanceAnalytics::Return.annualized(rb)
annualized_excess_return = PerformanceAnalytics::Return.annualized.excess(rp, rb, geometric = FALSE)

test_that("Top-down arithmetic attribution with proportional annualization and default scale gives expected results" , {
  attribution_results = Attribution(multi_period_portf_5$Rp, 
                                    multi_period_portf_5$wp, 
                                    multi_period_portf_5$Rb,
                                    multi_period_portf_5$wb, method = "top.down", contribution = TRUE, 
                                    annualization = "proportional")
  
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results$`Excess returns`[1:(NROW(attribution_results$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results$Allocation["Total", -NCOL(attribution_results$Allocation)])
  total_selection_effects = sum(attribution_results$Selection["Total", -NCOL(attribution_results$Selection)])
  total_effects = total_allocation_effects + total_selection_effects
  
  epsilon = 1e-20
  expect_equal(total_allocation_effects, attribution_results$Allocation["Total", "Total"], tolerance = epsilon)
  expect_equal(total_selection_effects, attribution_results$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 8e-13
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Portfolio contribution to return`[1:(NROW(attribution_results$`Portfolio contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Portfolio contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rp * zoo::coredata(multi_period_portf_5$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Benchmark contribution to return`[1:(NROW(attribution_results$`Benchmark contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Benchmark contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rb * zoo::coredata(multi_period_portf_5$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 6e-13
  expect_equal(as.numeric(attribution_results$`Excess returns`[NROW(attribution_results$`Excess returns`),]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results$Selection["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects
  
  epsilon = 5e-13
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  epsilon = 6e-14
  expect_equal(sum(attribution_results$`Portfolio contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Portfolio contribution to return`)-1]), 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(attribution_results$`Benchmark contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Benchmark contribution to return`)-1]), 
               as.numeric(rb_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})


custom_scale = 365
rp_annualized = prod(1  + rp)^(custom_scale/NROW(multi_period_portf_5$Rp)) - 1
rb_annualized = prod(1  + rb)^(custom_scale/NROW(multi_period_portf_5$Rb)) - 1
annualized_excess_return = rp_annualized - rb_annualized


test_that("Top-down arithmetic attribution with proportional annualization and custom scale gives expected results" , {
  attribution_results = Attribution(multi_period_portf_5$Rp, 
                                    multi_period_portf_5$wp, 
                                    multi_period_portf_5$Rb,
                                    multi_period_portf_5$wb, method = "top.down", contribution = TRUE, 
                                    annualization = "proportional", 
                                    annualization_scale = custom_scale)
  
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results$`Excess returns`[1:(NROW(attribution_results$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results$Allocation["Total", -NCOL(attribution_results$Allocation)])
  total_selection_effects = sum(attribution_results$Selection["Total", -NCOL(attribution_results$Selection)])
  total_effects = total_allocation_effects + total_selection_effects
  
  epsilon = 1e-20
  expect_equal(total_allocation_effects, attribution_results$Allocation["Total", "Total"], tolerance = epsilon)
  expect_equal(total_selection_effects, attribution_results$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 8e-13
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Portfolio contribution to return`[1:(NROW(attribution_results$`Portfolio contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Portfolio contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rp * zoo::coredata(multi_period_portf_5$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Benchmark contribution to return`[1:(NROW(attribution_results$`Benchmark contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Benchmark contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rb * zoo::coredata(multi_period_portf_5$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 6e-13
  expect_equal(as.numeric(attribution_results$`Excess returns`[NROW(attribution_results$`Excess returns`),]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results$Selection["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects
  
  epsilon = 5e-13
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  epsilon = 6e-14
  expect_equal(sum(attribution_results$`Portfolio contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Portfolio contribution to return`)-1]), 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(attribution_results$`Benchmark contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Benchmark contribution to return`)-1]), 
               as.numeric(rb_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})

test_that("Top-down arithmetic attribution with standard annualization and custom scale gives expected results" , {
  attribution_results = Attribution(multi_period_portf_5$Rp, 
                                    multi_period_portf_5$wp, 
                                    multi_period_portf_5$Rb,
                                    multi_period_portf_5$wb, method = "top.down", contribution = TRUE, 
                                    annualization = "standard",
                                    annualization_scale = custom_scale)
  
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results$`Excess returns`[1:(NROW(attribution_results$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results$Allocation["Total", -NCOL(attribution_results$Allocation)])
  total_selection_effects = sum(attribution_results$Selection["Total", -NCOL(attribution_results$Selection)])
  total_effects = total_allocation_effects + total_selection_effects
  
  epsilon = 1e-20
  expect_equal(total_allocation_effects, attribution_results$Allocation["Total", "Total"], tolerance = epsilon)
  expect_equal(total_selection_effects, attribution_results$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 8e-13
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Portfolio contribution to return`[1:(NROW(attribution_results$`Portfolio contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Portfolio contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rp * zoo::coredata(multi_period_portf_5$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Benchmark contribution to return`[1:(NROW(attribution_results$`Benchmark contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Benchmark contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rb * zoo::coredata(multi_period_portf_5$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 6e-13
  expect_equal(as.numeric(attribution_results$`Excess returns`[NROW(attribution_results$`Excess returns`),]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results$Selection["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects
  
  skip("Skipping these tests for now")
  epsilon = 5e-13
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_equal(sum(attribution_results$`Portfolio contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Portfolio contribution to return`)-1]), 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(attribution_results$`Benchmark contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Benchmark contribution to return`)-1]), 
               as.numeric(rb_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})

# Note, this relies on the start and end dates for the sample data set to be "2015-02-15" and "2016-03-18", respectively
custom_num_periods = 14/28 + 12 + 18/31
rp_annualized = prod(1  + rp)^(12/custom_num_periods) - 1
rb_annualized = prod(1  + rb)^(12/custom_num_periods) - 1
annualized_excess_return = rp_annualized - rb_annualized

test_that("Top-down arithmetic attribution with proportional annualization and monthly scale gives expected results" , {
  epsilon = 1e-20
  expect_equal(PortfolioAttribution:::getMonthlyPeriods(xts::first(zoo::index(multi_period_portf_5$Rp)), 
                                                        xts::last(zoo::index(multi_period_portf_5$Rp))), 
               custom_num_periods, tolerance = epsilon)
  
  attribution_results = Attribution(multi_period_portf_5$Rp, 
                                    multi_period_portf_5$wp, 
                                    multi_period_portf_5$Rb,
                                    multi_period_portf_5$wb, method = "top.down", contribution = TRUE, 
                                    annualization = "proportional", 
                                    annualization_scale = "monthly")
  
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results$`Excess returns`[1:(NROW(attribution_results$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results$Allocation["Total", -NCOL(attribution_results$Allocation)])
  total_selection_effects = sum(attribution_results$Selection["Total", -NCOL(attribution_results$Selection)])
  total_effects = total_allocation_effects + total_selection_effects
  
  epsilon = 1e-20
  expect_equal(total_allocation_effects, attribution_results$Allocation["Total", "Total"], tolerance = epsilon)
  expect_equal(total_selection_effects, attribution_results$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 8e-13
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Portfolio contribution to return`[1:(NROW(attribution_results$`Portfolio contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Portfolio contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rp * zoo::coredata(multi_period_portf_5$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Benchmark contribution to return`[1:(NROW(attribution_results$`Benchmark contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Benchmark contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rb * zoo::coredata(multi_period_portf_5$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 6e-13
  expect_equal(as.numeric(attribution_results$`Excess returns`[NROW(attribution_results$`Excess returns`),]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results$Selection["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects
  
  epsilon = 5e-13
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_equal(sum(attribution_results$`Portfolio contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Portfolio contribution to return`)-1]), 
               as.numeric(rp_annualized))
  expect_equal(sum(attribution_results$`Benchmark contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Benchmark contribution to return`)-1]), 
               as.numeric(rb_annualized))
  
  epsilon = 6e-15
  expect_equal(attribution_results$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})

test_that("Top-down arithmetic attribution with standard annualization and monthly scale gives expected results" , {
  attribution_results = Attribution(multi_period_portf_5$Rp, 
                                    multi_period_portf_5$wp, 
                                    multi_period_portf_5$Rb,
                                    multi_period_portf_5$wb, method = "top.down", contribution = TRUE, 
                                    annualization = "standard",
                                    annualization_scale = "monthly")
  
  epsilon = 8e-13
  expect_true(all(abs(as.numeric(attribution_results$`Excess returns`[1:(NROW(attribution_results$`Excess returns`)-2),]) -
                        as.numeric(periodic_excess_return)) < epsilon))
  
  total_allocation_effects = sum(attribution_results$Allocation["Total", -NCOL(attribution_results$Allocation)])
  total_selection_effects = sum(attribution_results$Selection["Total", -NCOL(attribution_results$Selection)])
  total_effects = total_allocation_effects + total_selection_effects
  
  epsilon = 1e-20
  expect_equal(total_allocation_effects, attribution_results$Allocation["Total", "Total"], tolerance = epsilon)
  expect_equal(total_selection_effects, attribution_results$Selection["Total", "Total"], tolerance = epsilon)
  epsilon = 8e-13
  expect_equal(total_effects, as.numeric(cumulative_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Portfolio contribution to return`[1:(NROW(attribution_results$`Portfolio contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Portfolio contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rp * zoo::coredata(multi_period_portf_5$wp)))) < epsilon))
  expect_true(all(abs(as.numeric(rowSums(attribution_results$
                                           `Benchmark contribution to return`[1:(NROW(attribution_results$`Benchmark contribution to return`)-2),
                                                                              1:NCOL(attribution_results$`Benchmark contribution to return`)-1])) -
                        as.numeric(rowSums(multi_period_portf_5$Rb * zoo::coredata(multi_period_portf_5$wb)))) < epsilon))
  
  # Annualized returns and effects
  epsilon = 6e-13
  expect_equal(as.numeric(attribution_results$`Excess returns`[NROW(attribution_results$`Excess returns`),]),
               as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  total_annualized_allocation_effects = sum(attribution_results$Allocation["Annualized", "Total"])
  total_annualized_selection_effects = sum(attribution_results$Selection["Annualized", "Total"])
  total_annualized_effects = total_annualized_allocation_effects + total_annualized_selection_effects
  
  skip("Skipping these tests for now")
  epsilon = 5e-13
  expect_equal(total_annualized_effects, as.numeric(annualized_excess_return),  tolerance = epsilon)
  
  epsilon = 1e-20
  # TODO: there are NaNs in this result set at column index 5939; find out why and fix the issue
  expect_equal(sum(attribution_results$`Portfolio contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Portfolio contribution to return`)-1]), 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(sum(attribution_results$`Benchmark contribution to return`["Annualized",
                                                                          1:NCOL(attribution_results$`Benchmark contribution to return`)-1]), 
               as.numeric(rb_annualized), tolerance = epsilon)
  
  epsilon = 2e-14
  expect_equal(attribution_results$`Portfolio returns`["Annualized","Total"], 
               as.numeric(rp_annualized), tolerance = epsilon)
  expect_equal(attribution_results$`Benchmark returns`["Annualized","Total"], 
               as.numeric(rb_annualized), tolerance = epsilon)
})
