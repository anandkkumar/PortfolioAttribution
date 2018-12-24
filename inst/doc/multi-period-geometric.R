## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(PortfolioAttribution)

## ---- echo=FALSE---------------------------------------------------------
data("sample_data", package = "PortfolioAttribution")

## ------------------------------------------------------------------------
library(PortfolioAttribution)
data("sample_data", package = "PortfolioAttribution")
# Portfolio weights
multi_period_portf_1$wp[1:3,1:4]

# Portfolio returns
multi_period_portf_1$Rp[1:3,1:4]

# Benchmark weights
multi_period_portf_1$wb[1:3,1:4]

# Benchmark returns
multi_period_portf_1$Rb[1:3,1:4]

## ------------------------------------------------------------------------
index(multi_period_portf_1$wp[1,])
index(multi_period_portf_1$Rp[1,])

index(multi_period_portf_1$wb[1,])
index(multi_period_portf_1$Rb[1,])

## ------------------------------------------------------------------------
attribution_results = Attribution(multi_period_portf_1$Rp, multi_period_portf_1$wp, 
                                  multi_period_portf_1$Rb, multi_period_portf_1$wb, 
                                  geometric = TRUE) 

## ------------------------------------------------------------------------
attribution_results$Allocation[1:3, c(1:3, NCOL(attribution_results$Allocation))]
attribution_results$Selection[1:3, c(1:3, NCOL(attribution_results$Allocation))]

## ------------------------------------------------------------------------
total_portfolio_return = prod(1 + multi_period_portf_1$periodicPortReturns) - 1
total_portfolio_return

total_benchmark_return = prod(1 + multi_period_portf_1$periodicBmkReturns) - 1
total_benchmark_return

total_geometric_excess_return = (1 + total_portfolio_return)/(1 + total_benchmark_return) - 1
total_geometric_excess_return


## ------------------------------------------------------------------------
total_effect = (1 + attribution_results$Allocation["Total", "Total"]) * 
               (1 + attribution_results$Selection["Total", "Total"]) - 1
all.equal(total_geometric_excess_return, total_effect)

