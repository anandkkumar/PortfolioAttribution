library(PortfolioAttribution)
library(testthat)

context("Examples from FactSet on Portfolio attribution using the @PA application")

epsilon = 1e-5

data(factset_sample_single_currency_3factor_arithmetic)
# Note that the FactSet sample has all returns, weights and attribution effects as percentages
cnames = factset_example$Sector

Wp = factset_example$Port.Weight/100
Wb = factset_example$Bmk.Weight/100

Rp = matrix(factset_example$Port.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rb = matrix(factset_example$Bmk.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))


test_that("FactSet BF 3-factor arithmetic example for one period" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="none", bf = TRUE, linking = "none", geometric = FALSE)
  
  expect_equal(as.numeric(attribution_results$Allocation[,"Consumer.Discretionary"]), 
               factset_example$Allocation.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Consumer.Staples"]), 
               factset_example$Allocation.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Energy"]), 
               factset_example$Allocation.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Financials"]), 
               factset_example$Allocation.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Health.Care"]), 
               factset_example$Allocation.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Industrials"]), 
               factset_example$Allocation.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Information.Technology"]), 
               factset_example$Allocation.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Materials"]), 
               factset_example$Allocation.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Real.Estate"]), 
               factset_example$Allocation.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Telecommunication.Services"]), 
               factset_example$Allocation.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Utilities"]),  
               factset_example$Allocation.Effect[11]/100, tolerance = epsilon)
  
  expect_equal(as.numeric(attribution_results$Selection[,"Consumer.Discretionary"]), 
               factset_example$Selection.Effect[1]/100, tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Consumer.Staples"]), 
  #              factset_example$Selection.Effect[2], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Energy"]), 
  #              factset_example$Selection.Effect[3], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Financials"]), 
  #              factset_example$Selection.Effect[4], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Health.Care"]), 
  #              factset_example$Selection.Effect[5], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Industrials"]), 
  #              factset_example$Selection.Effect[6], tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Information.Technology"]),
               factset_example$Selection.Effect[7]/100, tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Materials"]), 
  #              factset_example$Selection.Effect[8], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Real.Estate"]), 
  #              factset_example$Selection.Effect[9], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Telecommunication.Services"]), 
  #              factset_example$Selection.Effect[10], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Utilities"]),  
  #              factset_example$Selection.Effect[11], tolerance = epsilon)
  
  expect_equal(as.numeric(attribution_results$Interaction[,"Consumer.Discretionary"]), 
               factset_example$Interaction.Effect[1]/100, tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Consumer.Staples"]), 
  #              factset_example$Interaction.Effect[2], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Energy"]), 
  #              factset_example$Interaction.Effect[3], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Financials"]), 
  #              factset_example$Interaction.Effect[4], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Health.Care"]), 
  #              factset_example$Interaction.Effect[5], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Industrials"]), 
  #              factset_example$Interaction.Effect[6], tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Interaction[,"Information.Technology"]), 
               factset_example$Interaction.Effect[7]/100, tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Materials"]), 
  #              factset_example$Interaction.Effect[8], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Real.Estate"]), 
  #              factset_example$Interaction.Effect[9], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Telecommunication.Services"]), 
  #              factset_example$Interaction.Effect[10], tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Utilities"]),  
  #              factset_example$Interaction.Effect[11], tolerance = epsilon)

  expect_equal(as.numeric(attribution_results$Allocation[,"Total"]), 0.0007407, tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Selection[,"Total"]), 0.000695061, tolerance = epsilon)
  # expect_equal(as.numeric(attribution_results$Interaction[,"Total"]), -0.022946782, tolerance = epsilon)
  
  expect_equal(attribution_results$`Excess returns`[1], 0.0005182, tolerance=epsilon)
}
)



data(factset_sample_single_currency_2factor_arithmetic)
# Note that the FactSet sample has all returns, weights and attribution effects as percentages
cnames = factset_example$Sector

Wp = factset_example$Port.Weight/100
Wb = factset_example$Bmk.Weight/100

Rp = matrix(factset_example$Port.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rb = matrix(factset_example$Bmk.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))

test_that("FactSet BF 2-factor arithmetic example for one period" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="top.down", bf = TRUE, linking = "none", geometric = FALSE)
  
  expect_equal(as.numeric(attribution_results$Allocation[,"Consumer.Discretionary"]), 
               factset_example$Allocation.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Consumer.Staples"]), 
               factset_example$Allocation.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Energy"]), 
               factset_example$Allocation.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Financials"]), 
               factset_example$Allocation.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Health.Care"]), 
               factset_example$Allocation.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Industrials"]), 
               factset_example$Allocation.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Information.Technology"]), 
               factset_example$Allocation.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Materials"]), 
               factset_example$Allocation.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Real.Estate"]), 
               factset_example$Allocation.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Telecommunication.Services"]), 
               factset_example$Allocation.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Utilities"]),  
               factset_example$Allocation.Effect[11]/100, tolerance = epsilon)
  
  expect_equal(as.numeric(attribution_results$Selection[,"Consumer.Discretionary"]), 
               factset_example$Selection.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Consumer.Staples"]), 
               factset_example$Selection.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Energy"]),
               factset_example$Selection.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Financials"]),
               factset_example$Selection.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Health.Care"]),
               factset_example$Selection.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Industrials"]),
               factset_example$Selection.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Information.Technology"]),
               factset_example$Selection.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Materials"]),
               factset_example$Selection.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Real.Estate"]),
               factset_example$Selection.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Telecommunication.Services"]),
               factset_example$Selection.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Utilities"]),
               factset_example$Selection.Effect[11]/100, tolerance = epsilon)
  
  expect_null(attribution_results$Interaction)

  expect_equal(as.numeric(attribution_results$Allocation[,"Total"]), 0.0007407, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Total"]), -0.000222517213043738, tolerance = epsilon)

  expect_equal(attribution_results$`Excess returns`[1], 0.000518217316, tolerance=epsilon)
}
)



data(factset_sample_single_currency_2factor_geometric)
# Note that the FactSet sample has all returns, weights and attribution effects as percentages
cnames = factset_example$Sector

Wp = factset_example$Port.Weight/100
Wb = factset_example$Bmk.Weight/100

Rp = matrix(factset_example$Port.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rb = matrix(factset_example$Bmk.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))


test_that("FactSet BF 2-factor geometric example for one period" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="none", bf = TRUE, linking = "none", geometric = TRUE)
  
  expect_equal(as.numeric(attribution_results$Allocation[1,"Consumer.Discretionary"]),
               factset_example$Geometric.Allocation.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Consumer.Staples"]),
               factset_example$Geometric.Allocation.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Energy"]),
               factset_example$Geometric.Allocation.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Financials"]),
               factset_example$Geometric.Allocation.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Health.Care"]),
               factset_example$Geometric.Allocation.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Industrials"]),
               factset_example$Geometric.Allocation.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Information.Technology"]),
               factset_example$Geometric.Allocation.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Materials"]),
               factset_example$Geometric.Allocation.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Real.Estate"]),
               factset_example$Geometric.Allocation.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Telecommunication.Services"]),
               factset_example$Geometric.Allocation.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Utilities"]),
               factset_example$Geometric.Allocation.Effect[11]/100, tolerance = epsilon)

  expect_equal(as.numeric(attribution_results$Selection[1,"Consumer.Discretionary"]),
               factset_example$Geometric.Selection.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Consumer.Staples"]),
               factset_example$Geometric.Selection.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Energy"]),
               factset_example$Geometric.Selection.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Financials"]),
               factset_example$Geometric.Selection.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Health.Care"]),
               factset_example$Geometric.Selection.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Industrials"]),
               factset_example$Geometric.Selection.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Information.Technology"]),
               factset_example$Geometric.Selection.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Materials"]),
               factset_example$Geometric.Selection.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Real.Estate"]),
               factset_example$Geometric.Selection.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Telecommunication.Services"]),
               factset_example$Geometric.Selection.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Utilities"]),
               factset_example$Geometric.Selection.Effect[11]/100, tolerance = epsilon)

  expect_null(attribution_results$Interaction)
  
  expect_equal(as.numeric(attribution_results$Allocation[1,"Total"]), 0.000740877085271929, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Total"]), -0.000222395269495301, tolerance = epsilon)
  
  expect_equal(attribution_results$`Excess returns`[1], 0.000518317, tolerance=epsilon)
}
)



data(factset_sample_single_currency_heirarchical_top_down_arithmetic)
# Note that the FactSet sample has all returns, weights and attribution effects as percentages
cnames = c(factset_example$Industry[2:8], rep("ALL", 5), factset_example$Industry[15:20], rep("ALL", 4))
primary_ids = c(factset_example$Industry[2:8], paste(factset_example$Sector[9:13],"ALL", sep = "-"),
           factset_example$Industry[15:20],
           paste(factset_example$Sector[21:24],"ALL", sep = "-"))

heirarchy = data.frame(
  primary_id = primary_ids,
  sector = c(factset_example$Sector[2:8], factset_example$Sector[9:13], factset_example$Sector[15:24]),
  industry = cnames,
  stringsAsFactors = FALSE
)


Wp = factset_example$Port.Weight[c(2:13,15:24)]/100
Wb = factset_example$Bmk.Weight[c(2:13,15:24)]/100

Rp = matrix(factset_example$Port.Total.Return[c(2:13,15:24)]/100,ncol=22,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),primary_ids))
Rb = matrix(factset_example$Bmk.Total.Return[c(2:13,15:24)]/100,ncol=22,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),primary_ids))


test_that("FactSet BF 2-factor heirarchical arithmetic example for one period" , {
  attribution_results = Attribution.levels(Rp, Wp, Rb, Wb, heirarchy, geometric = FALSE, anchored = TRUE, c("sector", "industry"))

  total_level1_top_down_allocation_effect = factset_example$Top.Down.Allocation.Effect[1]/100 +
                                            sum(factset_example$Top.Down.Allocation.Effect[9:14]/100) +
                                            sum(factset_example$Top.Down.Allocation.Effect[21:24]/100)
  total_level2_top_down_allocation_effect = sum(factset_example$Top.Down.Allocation.Effect[2:8]/100) +
                                            sum(factset_example$Top.Down.Allocation.Effect[15:20]/100)
  total_selection_effect = sum(factset_example$Top.Down.Selection.Effect[2:13]/100) +
                            sum(factset_example$Top.Down.Selection.Effect[15:24]/100)

  expect_equal(attribution_results$`Multi-level attribution`[1, "Level 1 Allocation"], total_level1_top_down_allocation_effect, tolerance = epsilon)
  expect_equal(attribution_results$`Multi-level attribution`[1, "Level 2 Allocation"], total_level2_top_down_allocation_effect, tolerance = epsilon)
  expect_equal(attribution_results$`Multi-level attribution`[1, "Selection"], total_selection_effect, tolerance = epsilon)

  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Consumer Discretionary"],
               factset_example$Top.Down.Allocation.Effect[1]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Consumer Staples"],
               factset_example$Top.Down.Allocation.Effect[9]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Energy"],
               factset_example$Top.Down.Allocation.Effect[10]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Financials"],
               factset_example$Top.Down.Allocation.Effect[11]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Health Care"],
               factset_example$Top.Down.Allocation.Effect[12]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Industrials"],
               factset_example$Top.Down.Allocation.Effect[13]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Information Technology"],
               factset_example$Top.Down.Allocation.Effect[14]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Materials"],
               factset_example$Top.Down.Allocation.Effect[21]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Real Estate"],
               factset_example$Top.Down.Allocation.Effect[22]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Telecommunication Services"],
               factset_example$Top.Down.Allocation.Effect[23]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[1, "Utilities"],
               factset_example$Top.Down.Allocation.Effect[24]/100, tolerance = epsilon)

  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Consumer Discretionary-Automobiles"],
               factset_example$Top.Down.Allocation.Effect[2]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Consumer Discretionary-Hotels Restaurants & Leisure"],
               factset_example$Top.Down.Allocation.Effect[3]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Consumer Discretionary-Internet & Direct Marketing Retail"],
               factset_example$Top.Down.Allocation.Effect[4]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Consumer Discretionary-Media"],
               factset_example$Top.Down.Allocation.Effect[5]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Consumer Discretionary-Multiline Retail"],
               factset_example$Top.Down.Allocation.Effect[6]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Consumer Discretionary-Specialty Retail"],
               factset_example$Top.Down.Allocation.Effect[7]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Consumer Discretionary-Textiles Apparel & Luxury Goods"],
               factset_example$Top.Down.Allocation.Effect[8]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Consumer Staples-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Energy-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Financials-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Health Care-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Industrials-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Information Technology-Communications Equipment"],
               factset_example$Top.Down.Allocation.Effect[15]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Information Technology-Internet Software & Services"],
               factset_example$Top.Down.Allocation.Effect[16]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Information Technology-IT Services"],
               factset_example$Top.Down.Allocation.Effect[17]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Information Technology-Semiconductors & Semiconductor Equipment"],
               factset_example$Top.Down.Allocation.Effect[18]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Information Technology-Software"],
               factset_example$Top.Down.Allocation.Effect[19]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Information Technology-Technology Hardware Storage & Peripherals"],
               factset_example$Top.Down.Allocation.Effect[20]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Materials-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Real Estate-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Telecommunication Services-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[1, "Utilities-ALL"], 0, tolerance = epsilon)

  expect_equal(attribution_results$`Security selection`[1, "Consumer Discretionary-Automobiles"],
               factset_example$Top.Down.Selection.Effect[2]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Consumer Discretionary-Hotels Restaurants & Leisure"],
               factset_example$Top.Down.Selection.Effect[3]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Consumer Discretionary-Internet & Direct Marketing Retail"],
               factset_example$Top.Down.Selection.Effect[4]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Consumer Discretionary-Media"],
               factset_example$Top.Down.Selection.Effect[5]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Consumer Discretionary-Multiline Retail"],
               factset_example$Top.Down.Selection.Effect[6]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Consumer Discretionary-Specialty Retail"],
               factset_example$Top.Down.Selection.Effect[7]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Consumer Discretionary-Textiles Apparel & Luxury Goods"],
               factset_example$Top.Down.Selection.Effect[8]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Consumer Staples-ALL"],
               factset_example$Top.Down.Selection.Effect[9]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Energy-ALL"],
               factset_example$Top.Down.Selection.Effect[10]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Financials-ALL"],
               factset_example$Top.Down.Selection.Effect[11]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Health Care-ALL"],
               factset_example$Top.Down.Selection.Effect[12]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Industrials-ALL"],
               factset_example$Top.Down.Selection.Effect[13]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Information Technology-Communications Equipment"],
               factset_example$Top.Down.Selection.Effect[15]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Information Technology-Internet Software & Services"],
               factset_example$Top.Down.Selection.Effect[16]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Information Technology-IT Services"],
               factset_example$Top.Down.Selection.Effect[17]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Information Technology-Semiconductors & Semiconductor Equipment"],
               factset_example$Top.Down.Selection.Effect[18]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Information Technology-Software"],
               factset_example$Top.Down.Selection.Effect[19]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Information Technology-Technology Hardware Storage & Peripherals"],
               factset_example$Top.Down.Selection.Effect[20]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Materials-ALL"],
               factset_example$Top.Down.Selection.Effect[21]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Real Estate-ALL"],
               factset_example$Top.Down.Selection.Effect[22]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Telecommunication Services-ALL"],
               factset_example$Top.Down.Selection.Effect[23]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[1, "Utilities-ALL"],
               factset_example$Top.Down.Selection.Effect[24]/100, tolerance = epsilon)

  expect_equal(as.numeric(attribution_results$`Excess returns`[,"Arithmetic"]), 0.000518217316, tolerance = epsilon)
}
)


# Same example with returns constructed as one-period time series data
Rp = xts(matrix(c(rep(NA, 22),
                      factset_example$Port.Total.Return[c(2:13,15:24)]/100),ncol=22,byrow=TRUE,
                    dimnames=list(c("Rp", "Rp"),primary_ids)),
             order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))
Rb = xts(matrix(c(rep(NA, 22),
                      factset_example$Bmk.Total.Return[c(2:13,15:24)]/100),ncol=22,byrow=TRUE,
                    dimnames=list(c("Rb", "Rb"),primary_ids)),
             order.by=c(as.yearqtr("2017 Q3"), as.yearqtr("2017 Q4")))

test_that("FactSet BF 2-factor heirarchical arithmetic example for one period using time-series data" , {
  attribution_results = Attribution.levels(Rp, Wp, Rb, Wb, heirarchy, geometric = FALSE, anchored = TRUE, c("sector", "industry"))

  total_level1_top_down_allocation_effect = factset_example$Top.Down.Allocation.Effect[1]/100 +
    sum(factset_example$Top.Down.Allocation.Effect[9:14]/100) +
    sum(factset_example$Top.Down.Allocation.Effect[21:24]/100)
  total_level2_top_down_allocation_effect = sum(factset_example$Top.Down.Allocation.Effect[2:8]/100) +
    sum(factset_example$Top.Down.Allocation.Effect[15:20]/100)
  total_selection_effect = sum(factset_example$Top.Down.Selection.Effect[2:13]/100) +
    sum(factset_example$Top.Down.Selection.Effect[15:24]/100)

  expect_equal(attribution_results$`Multi-level attribution`[2, "Level 1 Allocation"], total_level1_top_down_allocation_effect, tolerance = epsilon)
  expect_equal(attribution_results$`Multi-level attribution`[2, "Level 2 Allocation"], total_level2_top_down_allocation_effect, tolerance = epsilon) #F
  expect_equal(attribution_results$`Multi-level attribution`[2, "Selection"], total_selection_effect, tolerance = epsilon)

  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Consumer Discretionary"],
               factset_example$Top.Down.Allocation.Effect[1]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Consumer Staples"],
               factset_example$Top.Down.Allocation.Effect[9]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Energy"],
               factset_example$Top.Down.Allocation.Effect[10]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Financials"],
               factset_example$Top.Down.Allocation.Effect[11]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Health Care"],
               factset_example$Top.Down.Allocation.Effect[12]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Industrials"],
               factset_example$Top.Down.Allocation.Effect[13]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Information Technology"],
               factset_example$Top.Down.Allocation.Effect[14]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Materials"],
               factset_example$Top.Down.Allocation.Effect[21]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Real Estate"],
               factset_example$Top.Down.Allocation.Effect[22]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Telecommunication Services"],
               factset_example$Top.Down.Allocation.Effect[23]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 1`[2, "Utilities"],
               factset_example$Top.Down.Allocation.Effect[24]/100, tolerance = epsilon)

  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Consumer Discretionary-Automobiles"],
               factset_example$Top.Down.Allocation.Effect[2]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Consumer Discretionary-Hotels Restaurants & Leisure"],
               factset_example$Top.Down.Allocation.Effect[3]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Consumer Discretionary-Internet & Direct Marketing Retail"],
               factset_example$Top.Down.Allocation.Effect[4]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Consumer Discretionary-Media"],
               factset_example$Top.Down.Allocation.Effect[5]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Consumer Discretionary-Multiline Retail"],
               factset_example$Top.Down.Allocation.Effect[6]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Consumer Discretionary-Specialty Retail"],
               factset_example$Top.Down.Allocation.Effect[7]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Consumer Discretionary-Textiles Apparel & Luxury Goods"],
               factset_example$Top.Down.Allocation.Effect[8]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Consumer Staples-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Energy-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Financials-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Health Care-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Industrials-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Information Technology-Communications Equipment"],
               factset_example$Top.Down.Allocation.Effect[15]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Information Technology-Internet Software & Services"],
               factset_example$Top.Down.Allocation.Effect[16]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Information Technology-IT Services"],
               factset_example$Top.Down.Allocation.Effect[17]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Information Technology-Semiconductors & Semiconductor Equipment"],
               factset_example$Top.Down.Allocation.Effect[18]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Information Technology-Software"],
               factset_example$Top.Down.Allocation.Effect[19]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Information Technology-Technology Hardware Storage & Peripherals"],
               factset_example$Top.Down.Allocation.Effect[20]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Materials-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Real Estate-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Telecommunication Services-ALL"], 0, tolerance = epsilon)
  expect_equal(attribution_results$`Attribution at each level`$`Level 2`[2, "Utilities-ALL"], 0, tolerance = epsilon)

  expect_equal(attribution_results$`Security selection`[2, "Consumer Discretionary-Automobiles"],
               factset_example$Top.Down.Selection.Effect[2]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Consumer Discretionary-Hotels Restaurants & Leisure"],
               factset_example$Top.Down.Selection.Effect[3]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Consumer Discretionary-Internet & Direct Marketing Retail"],
               factset_example$Top.Down.Selection.Effect[4]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Consumer Discretionary-Media"],
               factset_example$Top.Down.Selection.Effect[5]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Consumer Discretionary-Multiline Retail"],
               factset_example$Top.Down.Selection.Effect[6]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Consumer Discretionary-Specialty Retail"],
               factset_example$Top.Down.Selection.Effect[7]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Consumer Discretionary-Textiles Apparel & Luxury Goods"],
               factset_example$Top.Down.Selection.Effect[8]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Consumer Staples-ALL"],
               factset_example$Top.Down.Selection.Effect[9]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Energy-ALL"],
               factset_example$Top.Down.Selection.Effect[10]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Financials-ALL"],
               factset_example$Top.Down.Selection.Effect[11]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Health Care-ALL"],
               factset_example$Top.Down.Selection.Effect[12]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Industrials-ALL"],
               factset_example$Top.Down.Selection.Effect[13]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Information Technology-Communications Equipment"],
               factset_example$Top.Down.Selection.Effect[15]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Information Technology-Internet Software & Services"],
               factset_example$Top.Down.Selection.Effect[16]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Information Technology-IT Services"],
               factset_example$Top.Down.Selection.Effect[17]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Information Technology-Semiconductors & Semiconductor Equipment"],
               factset_example$Top.Down.Selection.Effect[18]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Information Technology-Software"],
               factset_example$Top.Down.Selection.Effect[19]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Information Technology-Technology Hardware Storage & Peripherals"],
               factset_example$Top.Down.Selection.Effect[20]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Materials-ALL"],
               factset_example$Top.Down.Selection.Effect[21]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Real Estate-ALL"],
               factset_example$Top.Down.Selection.Effect[22]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Telecommunication Services-ALL"],
               factset_example$Top.Down.Selection.Effect[23]/100, tolerance = epsilon)
  expect_equal(attribution_results$`Security selection`[2, "Utilities-ALL"],
               factset_example$Top.Down.Selection.Effect[24]/100, tolerance = epsilon)

  expect_equal(as.numeric(attribution_results$`Excess returns`[2,"Arithmetic"]), 0.000518217316, tolerance = epsilon)
}
)



data(factset_sample_multi_currency_2factor_arithmetic)
# Note that the FactSet sample has all returns, weights and attribution effects as percentages
cnames = factset_example$Sector

Wp = factset_example$Port.Weight/100
Wb = factset_example$Bmk.Weight/100

Rp = matrix(factset_example$Port.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rb = matrix(factset_example$Bmk.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rpl = matrix(factset_example$Port.Total.Return.Local/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rbl = matrix(factset_example$Bmk.Total.Return.Local/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))


test_that("FactSet BF 2-factor arithmetic multi-currency example for one period with currency effect as the difference between attribution in base currency and local currency" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="top.down", bf = TRUE, linking = "none", geometric = FALSE)
  attribution_results_local = Attribution(Rpl, Wp, Rbl, Wb, method="top.down", bf = TRUE, linking = "none", geometric = FALSE)
  
  expect_equal(as.numeric(attribution_results$Allocation[,"Consumer.Discretionary"]), 
               factset_example$Allocation.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Consumer.Staples"]), 
               factset_example$Allocation.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Energy"]), 
               factset_example$Allocation.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Financials"]), 
               factset_example$Allocation.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Health.Care"]), 
               factset_example$Allocation.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Industrials"]), 
               factset_example$Allocation.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Information.Technology"]), 
               factset_example$Allocation.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Materials"]), 
               factset_example$Allocation.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Real.Estate"]), 
               factset_example$Allocation.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Telecommunication.Services"]), 
               factset_example$Allocation.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Utilities"]),  
               factset_example$Allocation.Effect[11]/100, tolerance = epsilon)
  
  expect_equal(as.numeric(attribution_results$Selection[,"Consumer.Discretionary"]), 
               factset_example$Selection.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Consumer.Staples"]), 
               factset_example$Selection.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Energy"]),
               factset_example$Selection.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Financials"]),
               factset_example$Selection.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Health.Care"]),
               factset_example$Selection.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Industrials"]),
               factset_example$Selection.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Information.Technology"]),
               factset_example$Selection.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Materials"]),
               factset_example$Selection.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Real.Estate"]),
               factset_example$Selection.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Telecommunication.Services"]),
               factset_example$Selection.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Utilities"]),
               factset_example$Selection.Effect[11]/100, tolerance = epsilon)
  
  expect_null(attribution_results$Interaction)
  
  expect_equal(as.numeric(attribution_results$Allocation[,"Total"]), -0.00154535597482145, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[,"Total"]), -0.00979618400454548, tolerance = epsilon)
  
  expect_equal(attribution_results$`Excess returns`[1], -0.011341539979, tolerance=epsilon)

    
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Consumer.Discretionary"]), 
               factset_example$Allocation.Effect.Local[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Consumer.Staples"]), 
               factset_example$Allocation.Effect.Local[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Energy"]), 
               factset_example$Allocation.Effect.Local[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Financials"]), 
               factset_example$Allocation.Effect.Local[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Health.Care"]), 
               factset_example$Allocation.Effect.Local[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Industrials"]), 
               factset_example$Allocation.Effect.Local[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Information.Technology"]), 
               factset_example$Allocation.Effect.Local[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Materials"]), 
               factset_example$Allocation.Effect.Local[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Real.Estate"]), 
               factset_example$Allocation.Effect.Local[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Telecommunication.Services"]), 
               factset_example$Allocation.Effect.Local[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[,"Utilities"]),  
               factset_example$Allocation.Effect.Local[11]/100, tolerance = epsilon)
  
  expect_equal(as.numeric(attribution_results_local$Selection[,"Consumer.Discretionary"]), 
               factset_example$Selection.Effect.Local[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Consumer.Staples"]), 
               factset_example$Selection.Effect.Local[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Energy"]),
               factset_example$Selection.Effect.Local[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Financials"]),
               factset_example$Selection.Effect.Local[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Health.Care"]),
               factset_example$Selection.Effect.Local[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Industrials"]),
               factset_example$Selection.Effect.Local[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Information.Technology"]),
               factset_example$Selection.Effect.Local[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Materials"]),
               factset_example$Selection.Effect.Local[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Real.Estate"]),
               factset_example$Selection.Effect.Local[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Telecommunication.Services"]),
               factset_example$Selection.Effect.Local[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Utilities"]),
               factset_example$Selection.Effect.Local[11]/100, tolerance = epsilon)
  
  expect_null(attribution_results_local$Interaction)

  expect_equal(as.numeric(attribution_results_local$Allocation[,"Total"]), -0.000266595048395356, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[,"Total"]), -0.00967827356917747, tolerance = epsilon)
  
  expect_equal(attribution_results_local$`Excess returns`[1], -0.009944868618, tolerance=epsilon)
  

  # Total Currency Effect at each level
  expect_equal(as.numeric(attribution_results$Allocation[,"Consumer.Discretionary"] + 
                            attribution_results$Selection[,"Consumer.Discretionary"] -
                            attribution_results_local$Allocation[,"Consumer.Discretionary"] - 
                            attribution_results_local$Selection[,"Consumer.Discretionary"]), 
               factset_example$Total.Currency.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Consumer.Staples"] +
                            attribution_results$Selection[,"Consumer.Staples"] -
                            attribution_results_local$Allocation[,"Consumer.Staples"] -
                            attribution_results_local$Selection[,"Consumer.Staples"]), 
               factset_example$Total.Currency.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Energy"] + 
                            attribution_results$Selection[,"Energy"] -
                            attribution_results_local$Allocation[,"Energy"] -
                            attribution_results_local$Selection[,"Energy"]), 
               factset_example$Total.Currency.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Financials"] +
                            attribution_results$Selection[,"Financials"] - 
                            attribution_results_local$Allocation[,"Financials"] -
                            attribution_results_local$Selection[,"Financials"]), 
               factset_example$Total.Currency.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Health.Care"] +
                            attribution_results$Selection[,"Health.Care"] - 
                            attribution_results_local$Allocation[,"Health.Care"] -
                            attribution_results_local$Selection[,"Health.Care"]), 
               factset_example$Total.Currency.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Industrials"] +
                            attribution_results$Selection[,"Industrials"] -
                            attribution_results_local$Allocation[,"Industrials"] -
                            attribution_results_local$Selection[,"Industrials"]), 
               factset_example$Total.Currency.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Information.Technology"] +
                            attribution_results$Selection[,"Information.Technology"] -
                            attribution_results_local$Allocation[,"Information.Technology"] -
                            attribution_results_local$Selection[,"Information.Technology"]), 
               factset_example$Total.Currency.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Materials"] +
                            attribution_results$Selection[,"Materials"] -
                            attribution_results_local$Allocation[,"Materials"] -
                            attribution_results_local$Selection[,"Materials"]), 
               factset_example$Total.Currency.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Real.Estate"] +
                            attribution_results$Selection[,"Real.Estate"] -
                            attribution_results_local$Allocation[,"Real.Estate"] -
                            attribution_results_local$Selection[,"Real.Estate"]), 
               factset_example$Total.Currency.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Telecommunication.Services"] +
                            attribution_results$Selection[,"Telecommunication.Services"] -
                            attribution_results_local$Allocation[,"Telecommunication.Services"] -
                            attribution_results_local$Selection[,"Telecommunication.Services"]), 
               factset_example$Total.Currency.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[,"Utilities"] +
                            attribution_results$Selection[,"Utilities"] -
                            attribution_results_local$Allocation[,"Utilities"] -
                            attribution_results_local$Selection[,"Utilities"]),  
               factset_example$Total.Currency.Effect[11]/100, tolerance = epsilon)
  
  # Total Currency Effect for the whole portfolio
  expect_equal(as.numeric(attribution_results$Selection[,"Total"] + attribution_results$Allocation[,"Total"] -
                          attribution_results_local$Selection[,"Total"] - attribution_results_local$Allocation[,"Total"]), 
               -0.00139667136179411, tolerance = epsilon)
}
)




data(factset_sample_multi_currency_2factor_geometric)
# Note that the FactSet sample has all returns, weights and attribution effects as percentages
cnames = factset_example$Sector

Wp = factset_example$Port.Weight/100
Wb = factset_example$Bmk.Weight/100

Rp = matrix(factset_example$Port.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rb = matrix(factset_example$Bmk.Total.Return/100,ncol=11,byrow=TRUE,
            dimnames=list(c(as.character(Sys.Date())),cnames))
Rpl = matrix(factset_example$Port.Total.Return.Local/100,ncol=11,byrow=TRUE,
             dimnames=list(c(as.character(Sys.Date())),cnames))
Rbl = matrix(factset_example$Bmk.Total.Return.Local/100,ncol=11,byrow=TRUE,
             dimnames=list(c(as.character(Sys.Date())),cnames))


test_that("FactSet BF 2-factor geometric multi-currency example for one period with currency effect as the difference between attribution in base currency and local currency" , {
  attribution_results = Attribution(Rp, Wp, Rb, Wb, method="none", bf = TRUE, linking = "none", geometric = TRUE)
  attribution_results_local = Attribution(Rpl, Wp, Rbl, Wb, method="none", bf = TRUE, linking = "none", geometric = TRUE)
  
  expect_equal(as.numeric(attribution_results$Allocation[1,"Consumer.Discretionary"]), 
               factset_example$Geometric.Allocation.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Consumer.Staples"]), 
               factset_example$Geometric.Allocation.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Energy"]), 
               factset_example$Geometric.Allocation.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Financials"]), 
               factset_example$Geometric.Allocation.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Health.Care"]), 
               factset_example$Geometric.Allocation.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Industrials"]), 
               factset_example$Geometric.Allocation.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Information.Technology"]), 
               factset_example$Geometric.Allocation.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Materials"]), 
               factset_example$Geometric.Allocation.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Real.Estate"]), 
               factset_example$Geometric.Allocation.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Telecommunication.Services"]), 
               factset_example$Geometric.Allocation.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Utilities"]),  
               factset_example$Geometric.Allocation.Effect[11]/100, tolerance = epsilon)
  
  expect_equal(as.numeric(attribution_results$Selection[1,"Consumer.Discretionary"]), 
               factset_example$Geometric.Selection.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Consumer.Staples"]), 
               factset_example$Geometric.Selection.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Energy"]),
               factset_example$Geometric.Selection.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Financials"]),
               factset_example$Geometric.Selection.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Health.Care"]),
               factset_example$Geometric.Selection.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Industrials"]),
               factset_example$Geometric.Selection.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Information.Technology"]),
               factset_example$Geometric.Selection.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Materials"]),
               factset_example$Geometric.Selection.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Real.Estate"]),
               factset_example$Geometric.Selection.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Telecommunication.Services"]),
               factset_example$Geometric.Selection.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Utilities"]),
               factset_example$Geometric.Selection.Effect[11]/100, tolerance = epsilon)
  
  expect_null(attribution_results$Interaction)
  
  expect_equal(as.numeric(attribution_results$Allocation[1,"Total"]), -0.00154112937941353, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Selection[1,"Total"]), -0.00978447027846341, tolerance = epsilon)
  
  expect_equal(as.numeric(attribution_results$`Excess returns`[1, "Geometric"]), -0.0113105205, tolerance=epsilon)
  
  
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Consumer.Discretionary"]), 
               factset_example$Geometric.Allocation.Effect.Local[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Consumer.Staples"]), 
               factset_example$Geometric.Allocation.Effect.Local[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Energy"]), 
               factset_example$Geometric.Allocation.Effect.Local[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Financials"]), 
               factset_example$Geometric.Allocation.Effect.Local[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Health.Care"]), 
               factset_example$Geometric.Allocation.Effect.Local[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Industrials"]), 
               factset_example$Geometric.Allocation.Effect.Local[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Information.Technology"]), 
               factset_example$Geometric.Allocation.Effect.Local[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Materials"]), 
               factset_example$Geometric.Allocation.Effect.Local[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Real.Estate"]), 
               factset_example$Geometric.Allocation.Effect.Local[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Telecommunication.Services"]), 
               factset_example$Geometric.Allocation.Effect.Local[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Utilities"]),  
               factset_example$Geometric.Allocation.Effect.Local[11]/100, tolerance = epsilon)
  
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Consumer.Discretionary"]), 
               factset_example$Geometric.Selection.Effect.Local[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Consumer.Staples"]), 
               factset_example$Geometric.Selection.Effect.Local[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Energy"]),
               factset_example$Geometric.Selection.Effect.Local[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Financials"]),
               factset_example$Geometric.Selection.Effect.Local[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Health.Care"]),
               factset_example$Geometric.Selection.Effect.Local[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Industrials"]),
               factset_example$Geometric.Selection.Effect.Local[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Information.Technology"]),
               factset_example$Geometric.Selection.Effect.Local[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Materials"]),
               factset_example$Geometric.Selection.Effect.Local[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Real.Estate"]),
               factset_example$Geometric.Selection.Effect.Local[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Telecommunication.Services"]),
               factset_example$Geometric.Selection.Effect.Local[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Utilities"]),
               factset_example$Geometric.Selection.Effect.Local[11]/100, tolerance = epsilon)
  
  expect_null(attribution_results_local$Interaction)
  
  expect_equal(as.numeric(attribution_results_local$Allocation[1,"Total"]), -0.000266696010962075, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results_local$Selection[1,"Total"]), -0.0096845216607695, tolerance = epsilon)
  
  expect_equal(as.numeric(attribution_results_local$`Excess returns`[1, "Geometric"]), -0.0099486348, tolerance=epsilon)
  
  
  # Total Currency Effect at each level
  expect_equal(as.numeric(attribution_results$Allocation[1,"Consumer.Discretionary"] + 
                            attribution_results$Selection[1,"Consumer.Discretionary"] -
                            attribution_results_local$Allocation[1,"Consumer.Discretionary"] - 
                            attribution_results_local$Selection[1,"Consumer.Discretionary"]), 
               factset_example$Geometric.Total.Currency.Effect[1]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Consumer.Staples"] +
                            attribution_results$Selection[1,"Consumer.Staples"] -
                            attribution_results_local$Allocation[1,"Consumer.Staples"] -
                            attribution_results_local$Selection[1,"Consumer.Staples"]), 
               factset_example$Geometric.Total.Currency.Effect[2]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Energy"] + 
                            attribution_results$Selection[1,"Energy"] -
                            attribution_results_local$Allocation[1,"Energy"] -
                            attribution_results_local$Selection[1,"Energy"]), 
               factset_example$Geometric.Total.Currency.Effect[3]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Financials"] +
                            attribution_results$Selection[1,"Financials"] - 
                            attribution_results_local$Allocation[1,"Financials"] -
                            attribution_results_local$Selection[1,"Financials"]), 
               factset_example$Geometric.Total.Currency.Effect[4]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Health.Care"] +
                            attribution_results$Selection[1,"Health.Care"] - 
                            attribution_results_local$Allocation[1,"Health.Care"] -
                            attribution_results_local$Selection[1,"Health.Care"]), 
               factset_example$Geometric.Total.Currency.Effect[5]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Industrials"] +
                            attribution_results$Selection[1,"Industrials"] -
                            attribution_results_local$Allocation[1,"Industrials"] -
                            attribution_results_local$Selection[1,"Industrials"]), 
               factset_example$Geometric.Total.Currency.Effect[6]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Information.Technology"] +
                            attribution_results$Selection[1,"Information.Technology"] -
                            attribution_results_local$Allocation[1,"Information.Technology"] -
                            attribution_results_local$Selection[1,"Information.Technology"]), 
               factset_example$Geometric.Total.Currency.Effect[7]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Materials"] +
                            attribution_results$Selection[1,"Materials"] -
                            attribution_results_local$Allocation[1,"Materials"] -
                            attribution_results_local$Selection[1,"Materials"]), 
               factset_example$Geometric.Total.Currency.Effect[8]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Real.Estate"] +
                            attribution_results$Selection[1,"Real.Estate"] -
                            attribution_results_local$Allocation[1,"Real.Estate"] -
                            attribution_results_local$Selection[1,"Real.Estate"]), 
               factset_example$Geometric.Total.Currency.Effect[9]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Telecommunication.Services"] +
                            attribution_results$Selection[1,"Telecommunication.Services"] -
                            attribution_results_local$Allocation[1,"Telecommunication.Services"] -
                            attribution_results_local$Selection[1,"Telecommunication.Services"]), 
               factset_example$Geometric.Total.Currency.Effect[10]/100, tolerance = epsilon)
  expect_equal(as.numeric(attribution_results$Allocation[1,"Utilities"] +
                            attribution_results$Selection[1,"Utilities"] -
                            attribution_results_local$Allocation[1,"Utilities"] -
                            attribution_results_local$Selection[1,"Utilities"]),  
               factset_example$Geometric.Total.Currency.Effect[11]/100, tolerance = epsilon)
  
  # Total Currency Effect for the whole portfolio
  expect_equal(as.numeric(attribution_results$Selection[1,"Total"] + attribution_results$Allocation[1,"Total"] -
                            attribution_results_local$Selection[1,"Total"] - attribution_results_local$Allocation[1,"Total"]), 
               -0.0013764971602499, tolerance = epsilon)
}
)
