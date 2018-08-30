library(PortfolioAttribution)
library(testthat)

context("Multi-level Attribution validation")
epsilon = 1e-15

data("sample_data")

rp = PerformanceAnalytics::Return.portfolio(multi_period_portf_1$Rp, multi_period_portf_1$wp)
rb = PerformanceAnalytics::Return.portfolio(multi_period_portf_1$Rb, multi_period_portf_1$wb)
excess_return = PerformanceAnalytics::Return.cumulative(rp) - PerformanceAnalytics::Return.cumulative(rb)
geometric_excess_return = excess_return/(1 + PerformanceAnalytics::Return.cumulative(rb))

# Validate segment level results
hierarchy = data.frame("primary_id" = names(multi_period_portf_1$Rb),
                       "Segment" = paste("Segment", c(1:6)))

# Compute segment weights and returns
wp_segment = Weight.level(multi_period_portf_1$wp, multi_period_portf_1$Rp, hierarchy, level = "Segment")
Rp_segment = Return.level(multi_period_portf_1$Rp, multi_period_portf_1$wp, hierarchy, level = "Segment", relativeWeights = wp_segment)
wb_segment = Weight.level(multi_period_portf_1$wb, multi_period_portf_1$Rb, hierarchy, level = "Segment")
Rb_segment = Return.level(multi_period_portf_1$Rb, multi_period_portf_1$wb, hierarchy, level = "Segment", relativeWeights = wb_segment)

rp_segment = PerformanceAnalytics::Return.portfolio(Rp_segment, wp_segment)
rb_segment = PerformanceAnalytics::Return.portfolio(Rb_segment, wb_segment)

segment_excess_return = PerformanceAnalytics::Return.cumulative(rp_segment) - PerformanceAnalytics::Return.cumulative(rb_segment)
geometric_segment_excess_return = segment_excess_return/(1 + PerformanceAnalytics::Return.cumulative(rb_segment))

test_that("Segment based excess returns computed using Weight.level and Return.level methods give expected results" , {
  expect_true(all(abs(rp - rp_segment) < epsilon))
  expect_true(all(abs(rb - rb_segment) < epsilon))
  expect_equal(excess_return, segment_excess_return, tolerance = epsilon)
  expect_equal(geometric_excess_return, geometric_segment_excess_return, tolerance = epsilon)
})



## ARITHMETIC ATTRIBUTION

# Validate security level results
security_level_results = Attribution(multi_period_portf_1$Rp, 
                                     multi_period_portf_1$wp, 
                                     multi_period_portf_1$Rb, 
                                     multi_period_portf_1$wb, 
                                     method = 'top.down', linking = 'grap', 
                                     contribution = TRUE, adjusted = TRUE)

total_allocation_effects = sum(security_level_results$Allocation["Total",-NCOL(security_level_results$Allocation)])
total_selection_effects = sum(security_level_results$Selection["Total",-NCOL(security_level_results$Selection)])
total_effects = total_allocation_effects + total_selection_effects

test_that("Multi-period top-down arithmetic attribution at the security level gives expected results" , {
  expect_equal(as.numeric(excess_return), total_effects, tolerance = epsilon)
})

test_that("Multi-period top-down arithmetic attribution at the segment level gives expected results" , {
  segment_level_results = Attribution(Rp_segment, 
                                      wp_segment, 
                                      Rb_segment, 
                                      wb_segment, 
                                      method = 'top.down', linking = 'grap', 
                                      contribution = TRUE, adjusted = TRUE)
  
  total_segment_allocation_effects = sum(segment_level_results$Allocation["Total",-NCOL(segment_level_results$Allocation)])
  total_segment_selection_effects = sum(segment_level_results$Selection["Total",-NCOL(segment_level_results$Selection)])
  total_segment_effects = total_segment_allocation_effects + total_segment_selection_effects
  
  expect_equal(total_segment_effects, total_effects, tolerance = epsilon)
  expect_equal(as.numeric(segment_excess_return), total_segment_effects, tolerance = epsilon)
})

test_that("Multi-period top-down arithmetic attribution at the segment level using hierarchical API gives expected results" , {
  # Validate segment level results with hierarchical API
  hierarchical_segment_level_results = Attribution.levels(multi_period_portf_1$Rp, 
                                                          multi_period_portf_1$wp, 
                                                          multi_period_portf_1$Rb, 
                                                          multi_period_portf_1$wb,
                                                          h = hierarchy,
                                                          h_levels = "Segment",
                                                          method = 'top.down')
  
  total_hierarchical_segment_allocation_effects = hierarchical_segment_level_results$`Multi-level attribution`["Total", "Level 1 Allocation"]
  total_hierarchical_segment_selection_effects = hierarchical_segment_level_results$`Multi-level attribution`["Total", "Selection"]
  total_hierarchical_segment_effects = total_hierarchical_segment_allocation_effects + total_hierarchical_segment_selection_effects
  
  total_hierarchical_aggregated_segment_allocation_effects = 
    sum(hierarchical_segment_level_results$`Allocation at each level`$`Level 1`["Total",])
  total_hierarchical_aggregated_segment_selection_effects = 
    sum(hierarchical_segment_level_results$`Selection at each level`$`Level 1`["Total",])
  
  expect_equal(
    hierarchical_segment_level_results$`Multi-level attribution`$
      `Level 1 Allocation`[-NROW(hierarchical_segment_level_results$`Multi-level attribution`)],
    as.numeric(rowSums(hierarchical_segment_level_results$`Allocation at each level`$
                         `Level 1`[-NROW(hierarchical_segment_level_results$`Allocation at each level`$`Level 1`),])),
    tolerance = epsilon)
  expect_equal(
    hierarchical_segment_level_results$`Multi-level attribution`$
      Selection[-NROW(hierarchical_segment_level_results$`Multi-level attribution`)],
    as.numeric(rowSums(hierarchical_segment_level_results$`Selection at each level`$
                         `Level 1`[-NROW(hierarchical_segment_level_results$`Selection at each level`$`Level 1`),])),
    tolerance = epsilon)
  
  skip("The hierarchical approach for mutli-period arithmetic is not current. Eventually, these tests need to pass")
  expect_equal(as.numeric(excess_return), total_hierarchical_segment_effects, tolerance = epsilon)
  expect_equal(total_hierarchical_segment_allocation_effects, total_hierarchical_aggregated_segment_allocation_effects, tolerance = epsilon)
  expect_equal(total_hierarchical_segment_selection_effects, total_hierarchical_aggregated_segment_selection_effects, tolerance = epsilon)
})


## GEOMETRIC ATTRIBUTION


# Validate security level results
geometric_security_level_results = Attribution(multi_period_portf_1$Rp, 
                                               multi_period_portf_1$wp, 
                                               multi_period_portf_1$Rb, 
                                               multi_period_portf_1$wb, 
                                               geometric = TRUE,
                                               contribution = TRUE)

total_geometric_allocation_effects = geometric_security_level_results$Allocation["Total", "Total"]
total_geometric_selection_effects = geometric_security_level_results$Selection["Total", "Total"]
total_geometric_effects = (1+total_geometric_allocation_effects) * (1+total_geometric_selection_effects) - 1

test_that("Multi-period top-down geometric attribution at the security level gives expected results" , {
  expect_equal(as.numeric(geometric_excess_return), total_geometric_effects, tolerance = epsilon)
})

test_that("Multi-period top-down geometric attribution at the segment level gives expected results" , {
  geometric_segment_level_results = Attribution(Rp_segment, 
                                                wp_segment, 
                                                Rb_segment, 
                                                wb_segment, 
                                                geometric = TRUE,
                                                contribution = TRUE)
  
  total_geometric_segment_allocation_effects = geometric_segment_level_results$Allocation["Total", "Total"]
  total_geometric_segment_selection_effects = geometric_segment_level_results$Selection["Total", "Total"]
  total_geometric_segment_effects = (1+total_geometric_segment_allocation_effects) * (1+total_geometric_segment_selection_effects) - 1
  
  expect_equal(total_geometric_segment_effects, total_geometric_effects, tolerance = epsilon)
  expect_equal(as.numeric(geometric_segment_excess_return), total_geometric_segment_effects, tolerance = epsilon)
})

test_that("Multi-period top-down geometric attribution at the segment level using hierarchical API gives expected results" , {
  # Validate segment level results with hierarchical API
  hierarchical_geometric_segment_level_results = Attribution.levels(multi_period_portf_1$Rp, 
                                                                    multi_period_portf_1$wp, 
                                                                    multi_period_portf_1$Rb, 
                                                                    multi_period_portf_1$wb,
                                                                    h = hierarchy,
                                                                    h_levels = "Segment",
                                                                    geometric = TRUE)
  
  total_hierarchical_geometric_segment_allocation_effects = 
    hierarchical_geometric_segment_level_results$`Multi-level attribution`["Total", "Level 1 Allocation"]
  total_hierarchical_geometric_segment_selection_effects = 
    hierarchical_geometric_segment_level_results$`Multi-level attribution`["Total", "Selection"]
  total_hierarchical_geometric_segment_effects = 
    (1+total_hierarchical_geometric_segment_allocation_effects) * (1+total_hierarchical_geometric_segment_selection_effects) - 1 
  
  total_hierarchical_geometric_aggregated_segment_allocation_effects = 
    sum(hierarchical_geometric_segment_level_results$`Allocation at each level`$`Level 1`["Total",])
  total_hierarchical_geometric_aggregated_segment_selection_effects = 
    sum(hierarchical_geometric_segment_level_results$`Selection at each level`$`Level 1`["Total",])
  
  expect_equal(
    hierarchical_geometric_segment_level_results$`Multi-level attribution`$
      `Level 1 Allocation`[-NROW(hierarchical_geometric_segment_level_results$`Multi-level attribution`)],
    as.numeric(rowSums(hierarchical_geometric_segment_level_results$`Allocation at each level`$
                         `Level 1`[-NROW(hierarchical_geometric_segment_level_results$`Allocation at each level`$`Level 1`),])),
    tolerance = epsilon)
  expect_equal(
    hierarchical_geometric_segment_level_results$`Multi-level attribution`$
      Selection[-NROW(hierarchical_geometric_segment_level_results$`Multi-level attribution`)],
    as.numeric(rowSums(hierarchical_geometric_segment_level_results$`Selection at each level`$
                         `Level 1`[-NROW(hierarchical_geometric_segment_level_results$`Selection at each level`$`Level 1`),])),
    tolerance = epsilon)
  
  expect_equal(as.numeric(geometric_excess_return), total_hierarchical_geometric_segment_effects, tolerance = epsilon)
})
