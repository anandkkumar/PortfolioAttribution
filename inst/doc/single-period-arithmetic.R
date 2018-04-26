## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(PortfolioAttribution)

## ---- echo=FALSE, results='asis'-----------------------------------------
data("sample_data", package = "PortfolioAttribution")
knitr::kable(simple_portf_1)

## ------------------------------------------------------------------------
data("sample_data", package = "PortfolioAttribution")
simple_portf_1

## ------------------------------------------------------------------------
wp = as.numeric(simple_portf_1[1,])
Rp = data.frame(simple_portf_1[2,], row.names = Sys.Date(), check.names = FALSE)
wb = as.numeric(simple_portf_1[3,])
Rb = data.frame(simple_portf_1[4,], row.names = Sys.Date(), check.names = FALSE)

## ------------------------------------------------------------------------
attribution_results = Attribution(Rp, wp, Rb, wb, bf = FALSE)

## ------------------------------------------------------------------------
attribution_results$Allocation
attribution_results$Selection
attribution_results$Interaction

## ------------------------------------------------------------------------
excess_returns = attribution_results$`Excess returns`
excess_returns

total_effects = attribution_results$Allocation[1,"Total"] + attribution_results$Selection[1,"Total"] + attribution_results$Interaction[1,"Total"]
total_effects

## ------------------------------------------------------------------------
# Note, the default it to apply Brinson Fachler so setting the parameter 'bf' to TRUE is optional.
attribution_results = Attribution(Rp, wp, Rb, wb, bf = TRUE)

## ------------------------------------------------------------------------
attribution_results$Allocation
attribution_results$Selection
attribution_results$Interaction

## ------------------------------------------------------------------------
excess_returns = attribution_results$`Excess returns`
excess_returns

total_effects = attribution_results$Allocation[1,"Total"] + attribution_results$Selection[1,"Total"] + attribution_results$Interaction[1,"Total"]
total_effects

