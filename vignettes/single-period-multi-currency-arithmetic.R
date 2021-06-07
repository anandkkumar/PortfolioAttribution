## ----setup, include = FALSE-----------------------------------------------------------------------
options(width = 100)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(magrittr)
library(PortfolioAttribution)

## ---- echo=FALSE, results='asis'------------------------------------------------------------------
data("sample_data", package = "PortfolioAttribution")
knitr::kable(simple_multi_curr_portf_1)

## -------------------------------------------------------------------------------------------------
library(PortfolioAttribution)
data("sample_data", package = "PortfolioAttribution")
simple_multi_curr_portf_1

## -------------------------------------------------------------------------------------------------
wp = as.numeric(simple_multi_curr_portf_1[1,])
Rpl = data.frame(simple_multi_curr_portf_1[2,], row.names = Sys.Date(), check.names = FALSE)
Rp = data.frame(simple_multi_curr_portf_1[3,], row.names = Sys.Date(), check.names = FALSE)
wb = as.numeric(simple_multi_curr_portf_1[4,])
Rbl = data.frame(simple_multi_curr_portf_1[5,], row.names = Sys.Date(), check.names = FALSE)
Rb = data.frame(simple_multi_curr_portf_1[6,], row.names = Sys.Date(), check.names = FALSE)

## -------------------------------------------------------------------------------------------------
attribution_results_local = Attribution(Rpl, wp, Rbl, wb, method = "top.down")
attribution_results = Attribution(Rp, wp, Rb, wb, method = "top.down")

## -------------------------------------------------------------------------------------------------
excess_returns_local = attribution_results_local$`Excess returns`["Cumulative Return",]
excess_returns_local

total_effects_local = attribution_results_local$Allocation[1,"Total"] + attribution_results_local$Selection[1,"Total"]
total_effects_local

excess_returns = attribution_results$`Excess returns`["Cumulative Return",]
excess_returns

total_effects = attribution_results$Allocation[1,"Total"] + attribution_results$Selection[1,"Total"]
total_effects

## -------------------------------------------------------------------------------------------------
currency_effects = attribution_results$Allocation + attribution_results$Selection  - attribution_results_local$Allocation - attribution_results_local$Selection

## -------------------------------------------------------------------------------------------------
all.equal(as.numeric(attribution_results_local$Allocation[1,"Total"] + 
                     attribution_results_local$Selection[1,"Total"] + 
                     currency_effects[1,"Total"]), 
          as.numeric(excess_returns))

## ----echo=FALSE, results='asis'-------------------------------------------------------------------
results_summary = as.data.frame(rbind(attribution_results$Allocation[1,], 
                                      attribution_results$Selection[1,], 
                                      attribution_results_local$Allocation[1,], 
                                      attribution_results_local$Selection[1,], 
                                      currency_effects[1,]))
rownames(results_summary) = c("Base Currency Allocation", "Base Currency Selection",
                              "Local Currency Allocation", "Local Currency Selection", 
                              "Curreny effect")
results_summary %>% kableExtra::kable("html") %>% 
  kableExtra::row_spec(3:5, background = "#F7F7F7") %>% 
  kableExtra::column_spec(13, bold = TRUE)

## ---- echo=FALSE, results='asis'------------------------------------------------------------------
data("sample_data", package = "PortfolioAttribution")
knitr::kable(simple_multi_curr_portf_2)
knitr::kable(simple_multi_curr_portf_2_spot_forward_rates)

## -------------------------------------------------------------------------------------------------
data("sample_data", package = "PortfolioAttribution")
simple_multi_curr_portf_2

simple_multi_curr_portf_2_spot_forward_rates

## -------------------------------------------------------------------------------------------------
wp = as.numeric(simple_multi_curr_portf_2[1,])
Rp = data.frame(simple_multi_curr_portf_2[2,], row.names = Sys.Date(), check.names = FALSE)
wb = as.numeric(simple_multi_curr_portf_2[3,])
Rb = data.frame(simple_multi_curr_portf_2[4,], row.names = Sys.Date(), check.names = FALSE)
wpf = as.numeric(simple_multi_curr_portf_2[5,])
wbf = as.numeric(simple_multi_curr_portf_2[6,])
S = data.frame(simple_multi_curr_portf_2_spot_forward_rates[1:2,], row.names = c(Sys.Date(), Sys.Date()+1), check.names = FALSE)
Fp = data.frame(simple_multi_curr_portf_2_spot_forward_rates[3:4,], row.names = c(Sys.Date(), Sys.Date()+1), check.names = FALSE)
Fb = data.frame(simple_multi_curr_portf_2_spot_forward_rates[5:6,], row.names = c(Sys.Date(), Sys.Date()+1), check.names = FALSE)

## -------------------------------------------------------------------------------------------------
Rp;Rb

S;Fp;Fb

## -------------------------------------------------------------------------------------------------
attribution_results = Attribution(Rp, wp, Rb, wb, wpf, wbf, S, Fp, Fb, currency_method = "ankrim.hensel")

## -------------------------------------------------------------------------------------------------
names(attribution_results)
attribution_results$`Currency management`
attribution_results$`Forward Premium`

## -------------------------------------------------------------------------------------------------
attribution_results$`Excess returns`["Cumulative Return",]

all.equal(as.numeric(attribution_results$Allocation[1,"Total"] + 
                     attribution_results$Selection[1,"Total"] + 
                     attribution_results$Interaction[1,"Total"] + 
                     attribution_results$`Currency management`[1,"Total"] +
                     attribution_results$`Forward Premium`[1,"Total"]),
          as.numeric(attribution_results$`Excess returns`["Cumulative Return",]))

