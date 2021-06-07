## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(PortfolioAttribution)

## ---- echo=FALSE--------------------------------------------------------------
data("sample_data", package = "PortfolioAttribution")

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
attribution_results = Attribution(multi_period_portf_1$Rp, multi_period_portf_1$wp, 
                                  multi_period_portf_1$Rb, multi_period_portf_1$wb, 
                                  contribution = TRUE)

## -----------------------------------------------------------------------------
names(attribution_results)

## -----------------------------------------------------------------------------
# Portfolio contribution to return
attribution_results$`Portfolio contribution to return`[1:3,1:4]

# Benchmark contribution to return
attribution_results$`Benchmark contribution to return`[1:3,1:4]

