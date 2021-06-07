## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(magrittr)
library(PortfolioAttribution)

## ---- echo=FALSE, results='asis'------------------------------------------------------------------
data("sample_data", package = "PortfolioAttribution")
knitr::kable(simple_portf_2)

## -------------------------------------------------------------------------------------------------
library(PortfolioAttribution)
data("sample_data", package = "PortfolioAttribution")
simple_portf_2

## -------------------------------------------------------------------------------------------------
wp = as.numeric(simple_portf_2[1,])
Rp = data.frame(simple_portf_2[2,], row.names = Sys.Date(), check.names = FALSE)
wb = as.numeric(simple_portf_2[3,])
Rb = data.frame(simple_portf_2[4,], row.names = Sys.Date(), check.names = FALSE)

## -------------------------------------------------------------------------------------------------
# The default value for the 'method' parameter is 'none' 
# and so explicitly setting this parameter below is not needed.
attribution_results_standard = Attribution(Rp, wp, Rb, wb, method="none")

## -------------------------------------------------------------------------------------------------
attribution_results_top_down = Attribution(Rp, wp, Rb, wb, method="top.down")

## -------------------------------------------------------------------------------------------------
attribution_results_bottom_up = Attribution(Rp, wp, Rb, wb, method="bottom.up")

## -------------------------------------------------------------------------------------------------
is.null(attribution_results_top_down$Interaction)
is.null(attribution_results_bottom_up$Interaction)

## -------------------------------------------------------------------------------------------------
as.numeric(attribution_results_standard$`Excess returns`["Cumulative Return",1])

all.equal(as.numeric(attribution_results_standard$`Excess returns`["Cumulative Return",1]) , as.numeric(attribution_results_standard$Allocation[1,"Total"] + attribution_results_standard$Selection[1,"Total"] + attribution_results_standard$Interaction[1,"Total"]))

all.equal(as.numeric(attribution_results_top_down$`Excess returns`["Cumulative Return",1]) , as.numeric(attribution_results_top_down$Allocation[1,"Total"] + attribution_results_top_down$Selection[1,"Total"]))

all.equal(as.numeric(attribution_results_bottom_up$`Excess returns`["Cumulative Return",1]) , as.numeric(attribution_results_bottom_up$Allocation[1,"Total"] + attribution_results_bottom_up$Selection[1,"Total"]))

## ----echo=FALSE, results='asis'-------------------------------------------------------------------
effects = c("Standard Allocation", 
            "Standard Selection", 
            "Standard Interaction",
            "Top-down Allocation",
            "Top-down Selection",
            "Bottom-up Allocation",
            "Bottom-up Selection")
aggregated_results = cbind("Effect" = effects,
                           rbind(as.data.frame(attribution_results_standard$Allocation), 
                                 as.data.frame(attribution_results_standard$Selection), 
                                 as.data.frame(attribution_results_standard$Interaction),
                                 as.data.frame(attribution_results_top_down$Allocation),
                                 as.data.frame(attribution_results_top_down$Selection),
                                 as.data.frame(attribution_results_bottom_up$Allocation),
                                 as.data.frame(attribution_results_bottom_up$Selection))
                           )
aggregated_results %>% 
  dplyr::mutate(Total = kableExtra::cell_spec(Total, "html", 
                                              color = ifelse(seq_along(Total) %in% c(1,4), 
                                                             "blue", 
                                                             ifelse(seq_along(Total) %in% c(2,7), "orange", "black")
                                              ))) %>%
  kableExtra::kable("html", escape = FALSE) %>% 
  kableExtra::row_spec(4:5, background = "#F7F7F7") %>% 
  kableExtra::column_spec(12, bold = TRUE)

