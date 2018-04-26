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
# Portfolio 1 weights
multi_period_portf_1$wp[1:3,1:5]

# Portfolio 1 returns
multi_period_portf_1$Rp[1:3,1:5]

# Benchmark 1 weights
multi_period_portf_1$wb[1:3,1:5]

# Benchmark 1 returns
multi_period_portf_1$Rb[1:3,1:5]

# Portfolio 2 weights
multi_period_portf_3$wp[1:3,1:5]

# Portfolio 2 returns
multi_period_portf_3$Rp[1:3,1:5]

# Benchmark 2 weights
multi_period_portf_3$wb[1:3,1:5]

# Benchmark 2 returns
multi_period_portf_3$Rb[1:3,1:5]

## ------------------------------------------------------------------------
system.time(Attribution(multi_period_portf_1$Rp, multi_period_portf_1$wp, 
                        multi_period_portf_1$Rb, multi_period_portf_1$wb, 
                        method = "top.down"))

## ------------------------------------------------------------------------
system.time(Attribution(multi_period_portf_1$Rp, multi_period_portf_1$wp, 
                        multi_period_portf_1$Rb, multi_period_portf_1$wb, 
                        geometric = TRUE))

## ------------------------------------------------------------------------
system.time(Attribution(multi_period_portf_3$Rp, multi_period_portf_3$wp, 
                        multi_period_portf_3$Rb, multi_period_portf_3$wb, 
                        method = "top.down"))

## ------------------------------------------------------------------------
system.time(Attribution(multi_period_portf_3$Rp, multi_period_portf_3$wp, 
                        multi_period_portf_3$Rb, multi_period_portf_3$wb, 
                        geometric = TRUE))

