## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(PortfolioAttribution)

## ---- echo=FALSE---------------------------------------------------------
data("sample_data", package = "PortfolioAttribution")

## ------------------------------------------------------------------------
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
wp = multi_period_portf_1$wp[1,]
Rp = multi_period_portf_1$Rp[1,]
index(wp)
index(Rp)

wb = multi_period_portf_1$wb[1,]
Rb = multi_period_portf_1$Rb[1,]
index(wb)
index(Rb)

## ------------------------------------------------------------------------
attribution_results = Attribution(Rp, wp, Rb, wb, geometric = TRUE)

## ------------------------------------------------------------------------
attribution_results$Allocation[1, "Total"]
attribution_results$Selection[1, "Total"]

## ------------------------------------------------------------------------
geometric_excess_return = as.numeric((1+multi_period_portf_1$periodicPortReturns[1,])/
                                       (1+multi_period_portf_1$periodicBmkReturns[1,]) - 1)
geometric_excess_return

## ------------------------------------------------------------------------
total_effect = (1+attribution_results$Allocation[1, "Total"])*(1+attribution_results$Selection[1, "Total"])-1
all.equal(geometric_excess_return, total_effect)

