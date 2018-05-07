## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(magrittr)
library(PortfolioAttribution)

## ---- echo=FALSE, results='asis'-----------------------------------------
data("sample_data", package = "PortfolioAttribution")

cbind(multi_period_portf_2$wp, multi_period_portf_2$Rp) %>% 
  kableExtra::kable("html") %>% 
  kableExtra::kable_styling("striped") %>% 
  # The 'striped' styling directive above does not appear to work and so we stripe if manually
  kableExtra::row_spec(seq(2, NROW(multi_period_portf_2$Rp), by=2), background = "#F7F7F7") %>% 
  kableExtra::add_header_above(c(" " = 1, "Portfolio Weights" = 3, "Portfolio Returns" = 3))

cbind(multi_period_portf_2$wb, multi_period_portf_2$Rb) %>% 
  kableExtra::kable("html") %>% 
  kableExtra::kable_styling("striped") %>% 
  # The 'striped' styling directive above does not appear to work and so we stripe if manually
  kableExtra::row_spec(seq(2, NROW(multi_period_portf_2$Rb), by=2), background = "#F7F7F7") %>% 
  kableExtra::add_header_above(c(" " = 1, "Benchmark Weights" = 3, "Benchmark Returns" = 3))

## ------------------------------------------------------------------------
data("sample_data", package = "PortfolioAttribution")
names(multi_period_portf_2)

## ------------------------------------------------------------------------
# the adjusted parameter defaults to FALSE
attribution_results_grap_unadjusted = Attribution(multi_period_portf_2$Rp, 
                                                  multi_period_portf_2$wp, 
                                                  multi_period_portf_2$Rb, 
                                                  multi_period_portf_2$wb, 
                                                  method = "top.down", 
                                                  linking = "grap", adjusted = FALSE) 

## ------------------------------------------------------------------------
attribution_results_grap_unadjusted$Allocation
attribution_results_grap_unadjusted$Selection

## ------------------------------------------------------------------------
total_portfolio_return = prod(1 + PerformanceAnalytics::Return.portfolio(multi_period_portf_2$Rp, 
                                                                         multi_period_portf_2$wp)) - 1
total_portfolio_return

total_benchmark_return = prod(1 + PerformanceAnalytics::Return.portfolio(multi_period_portf_2$Rb, 
                                                                         multi_period_portf_2$wb)) - 1
total_benchmark_return

total_excess_return = total_portfolio_return - total_benchmark_return
total_excess_return

## ------------------------------------------------------------------------
all.equal(attribution_results_grap_unadjusted$Allocation["Total", "Total"] + 
          attribution_results_grap_unadjusted$Selection["Total", "Total"],
          total_excess_return)

## ------------------------------------------------------------------------
attribution_results_grap = Attribution(multi_period_portf_2$Rp, 
                                       multi_period_portf_2$wp, 
                                       multi_period_portf_2$Rb, 
                                       multi_period_portf_2$wb, 
                                       method = "top.down", linking = "grap", adjusted = TRUE)

## ---- echo=FALSE, results='asis'-----------------------------------------
cbind(attribution_results_grap_unadjusted$Allocation, attribution_results_grap$Allocation) %>% 
  kableExtra::kable("html", caption = "Allocation Effects") %>% 
  kableExtra::kable_styling("striped") %>% 
  kableExtra::column_spec(2:5, color = "blue") %>%
  kableExtra::column_spec(6:9, color = "orange") %>%
  # The 'striped' styling directive above does not appear to work and so we stripe if manually
  kableExtra::row_spec(seq(2, NROW(attribution_results_grap_unadjusted$Allocation), by=2), background = "#F7F7F7") %>% 
  kableExtra::row_spec(NROW(attribution_results_grap_unadjusted$Allocation),  color="black", bold = TRUE) %>% 
  kableExtra::add_header_above(c(" " = 1, "Original effects" = 4, "Smoothed effects" = 4))

cbind(attribution_results_grap_unadjusted$Selection, attribution_results_grap$Selection) %>% 
  kableExtra::kable("html", caption = "Selection Effects") %>% 
  kableExtra::kable_styling("striped") %>% 
  kableExtra::column_spec(2:5, color = "blue") %>%
  kableExtra::column_spec(6:9, color = "orange") %>%
  # The 'striped' styling directive above does not appear to work and so we stripe if manually
  kableExtra::row_spec(seq(2, NROW(attribution_results_grap_unadjusted$Selection), by=2), background = "#F7F7F7") %>% 
  kableExtra::row_spec(NROW(attribution_results_grap_unadjusted$Selection),  color="black", bold = TRUE) %>% 
  kableExtra::add_header_above(c(" " = 1, "Original effects" = 4, "Smoothed effects" = 4))


## ------------------------------------------------------------------------
attribution_results_carino = Attribution(multi_period_portf_2$Rp, 
                                         multi_period_portf_2$wp, 
                                         multi_period_portf_2$Rb, 
                                         multi_period_portf_2$wb, 
                                         method = "top.down", linking = "carino", adjusted = TRUE)

## ------------------------------------------------------------------------
all.equal(attribution_results_carino$Allocation["Total", "Total"] + 
            attribution_results_carino$Selection["Total", "Total"],
          total_excess_return)

## ------------------------------------------------------------------------
attribution_results_menchero = Attribution(multi_period_portf_2$Rp, 
                                           multi_period_portf_2$wp, 
                                           multi_period_portf_2$Rb, 
                                           multi_period_portf_2$wb, 
                                           method = "top.down", linking = "menchero", adjusted = TRUE)

## ------------------------------------------------------------------------
all.equal(attribution_results_menchero$Allocation["Total", "Total"] + 
            attribution_results_menchero$Selection["Total", "Total"],
          total_excess_return)

## ------------------------------------------------------------------------
attribution_results_frongello = Attribution(multi_period_portf_2$Rp, 
                                            multi_period_portf_2$wp, 
                                            multi_period_portf_2$Rb, 
                                            multi_period_portf_2$wb, 
                                            method = "top.down", linking = "frongello", adjusted = TRUE)

## ------------------------------------------------------------------------
all.equal(attribution_results_frongello$Allocation["Total", "Total"] + 
            attribution_results_frongello$Selection["Total", "Total"],
          total_excess_return)

## ------------------------------------------------------------------------
attribution_results_davies_laker = Attribution(multi_period_portf_2$Rp, 
                                               multi_period_portf_2$wp, 
                                               multi_period_portf_2$Rb, 
                                               multi_period_portf_2$wb, 
                                               linking = "davies.laker")

## ------------------------------------------------------------------------
all.equal(attribution_results_davies_laker$Allocation["Total", "Total"] + 
            attribution_results_davies_laker$Selection["Total", "Total"] + 
            attribution_results_davies_laker$Interaction["Total", "Total"],
          total_excess_return)

## ---- echo=FALSE, results='asis'-----------------------------------------
cbind(attribution_results_grap$Allocation, 
      attribution_results_frongello$Allocation) %>% 
  kableExtra::kable("html", caption = "Allocation Effects") %>% 
  kableExtra::kable_styling("striped") %>% 
  kableExtra::column_spec(2:5, color = "blue") %>%
  kableExtra::column_spec(6:9, color = "orange") %>%
  # The 'striped' styling directive above does not appear to work and so we stripe if manually
  kableExtra::row_spec(seq(2, NROW(attribution_results_grap$Allocation), by=2), background = "#F7F7F7") %>% 
  kableExtra::row_spec(NROW(attribution_results_grap$Allocation), color = "black", bold = TRUE) %>% 
  kableExtra::add_header_above(c(" " = 1, "GRAP" = 4, "Frongello" = 4))

cbind(attribution_results_carino$Allocation,
      attribution_results_menchero$Allocation) %>% 
  kableExtra::kable("html", caption = "Allocation Effects") %>% 
  kableExtra::kable_styling("striped") %>% 
  kableExtra::column_spec(2:5, color = "darkcyan") %>%
  kableExtra::column_spec(6:9, color = "red") %>%
  # The 'striped' styling directive above does not appear to work and so we stripe if manually
  kableExtra::row_spec(seq(2, NROW(attribution_results_carino$Allocation), by=2), background = "#F7F7F7") %>% 
  kableExtra::row_spec(NROW(attribution_results_carino$Allocation), bold = TRUE) %>% 
  kableExtra::add_header_above(c(" " = 1, "Carino" = 4, "Menchero" = 4))

cbind(attribution_results_grap$Selection, 
      attribution_results_frongello$Selection) %>% 
  kableExtra::kable("html", caption = "Selection Effects") %>% 
  kableExtra::kable_styling("striped") %>% 
  kableExtra::column_spec(2:5, color = "blue") %>%
  kableExtra::column_spec(6:9, color = "orange") %>%
  # The 'striped' styling directive above does not appear to work and so we stripe if manually
  kableExtra::row_spec(seq(2, NROW(attribution_results_grap$Selection), by=2), background = "#F7F7F7") %>% 
  kableExtra::row_spec(NROW(attribution_results_grap$Selection), color = "black", bold = TRUE) %>% 
  kableExtra::add_header_above(c(" " = 1, "GRAP" = 4, "Frongello" = 4))

cbind(attribution_results_carino$Selection,
      attribution_results_menchero$Selection) %>% 
  kableExtra::kable("html", caption = "Selection Effects") %>% 
  kableExtra::kable_styling("striped") %>% 
  kableExtra::column_spec(2:5, color = "darkcyan") %>%
  kableExtra::column_spec(6:9, color = "red") %>%
  # The 'striped' styling directive above does not appear to work and so we stripe if manually
  kableExtra::row_spec(seq(2, NROW(attribution_results_carino$Selection), by=2), background = "#F7F7F7") %>% 
  kableExtra::row_spec(NROW(attribution_results_carino$Selection), bold = TRUE) %>% 
  kableExtra::add_header_above(c(" " = 1, "Carino" = 4, "Menchero" = 4))

cbind(attribution_results_davies_laker$Allocation,
      attribution_results_davies_laker$Selection,
      attribution_results_davies_laker$Interaction) %>% 
  kableExtra::kable("html", caption = "Davies and Laker") %>% 
  kableExtra::kable_styling("striped") %>% 
  kableExtra::column_spec(2:13, color = "purple") %>%
  # The 'striped' styling directive above does not appear to work and so we stripe if manually
  kableExtra::row_spec(seq(2, NROW(attribution_results_davies_laker$Allocation), by=2), background = "#F7F7F7") %>% 
  kableExtra::row_spec(NROW(attribution_results_davies_laker$Allocation), bold = TRUE) %>% 
  kableExtra::add_header_above(c(" " = 1, "Allocation" = 4, "Selection" = 4, "Interaction" = 4))

