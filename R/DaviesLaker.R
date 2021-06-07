#' calculates total attribution effects using Davies and Laker smoothing 
#' 
#' Calculates total attribution effects over multiple periods using 
#' Davies and Laker linking method. Used internally by the 
#' \code{\link{Attribution}} function. Arithmetic attribution effects do not 
#' naturally link over time. This function uses Davies and Laker linking method
#' to compute total attribution effects and uses Brinson, Hood & Beebower approach 
#' to defining allocation, selection & interaction effects. 
#' Arithmetic excess returns are decomposed as follows:
#' \deqn{R_{p} - R_{b} = Allocation + Selection + Interaction}{Rp - Rb = 
#' Allocation + Selection + Interaction}
#' \deqn{Allocation = \prod^{T}_{t=1}(1+bs_{t})-\prod^{T}_{t=1}(1+R_{bt})}
#' \deqn{Selection = \prod^{T}_{t=1}(1+rs_{t})-\prod^{T}_{t=1}(1+R_{bt})}
#' \deqn{Interaction = \prod^{T}_{t=1}(1+R_{pt})-\prod^{T}_{t=1}(1+rs_{t})-
#' \prod^{T}_{t=1}(1+bs_{t})+\prod^{T}_{t=1}(1+R_{bt})}
#' \eqn{R_{pi}}{Rpi} - portfolio returns at period \eqn{i}, 
#' \eqn{R_{bi}}{Rbi} - benchmark returns at period \eqn{i},
#' \eqn{rs_{i}}{rsi} - selection notional fund returns at period \eqn{i}, 
#' \eqn{bs_{i}}{bsi} - allocation notional fund returns at period \eqn{i}, 
#' \eqn{T} - number of periods
#' 
#' @aliases DaviesLaker
#' @param Rp xts of portfolio returns
#' @param wp xts of portfolio weights
#' @param Rb xts of benchmark returns
#' @param wb xts of benchmark weights
#' @param annualization Used to select the annualization method for multi-period returns, excess returns,
#' and attribution effects. May be any of:
#' \itemize{\item none - no annualized numbers are returned
#' \item standard 
#' \item proportional}
#' The default is 'none'. See vignette on annualization for detailed descriptions of the supported methods
#' @param annualization_scale Used to define a custom scale factor to use when annualizing returns. This is only
#' applicable if the periodicity of the data is daily and otherwise this parameter is ignored. May be any of:
#' \itemize{\item NA - use built-in scale factor
#' \item <numeric> - any numeric value
#' \item "monthly" - count the periods in terms of months (including fractional months) to determine scale}
#' @param impute_returns TRUE/FALSE, whether to impute returns from the benchmark, when the weight of the asset
#' is zero in the portfolio & vice-versa. These imputed returns are only used in the calculations
#' for attributions effects but not for cumulative asset returns (annualized or otherwise). Defaults to TRUE.
#' @return This function returns the data.frame with original attribution 
#' effects and total attribution effects over multiple periods
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
#' \code{\link{Grap}} \cr \code{\link{Carino}} \cr
#' \code{\link{Attribution.geometric}} \cr \code{\link{Frongello}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 201-204 \cr Davies, O. and Laker, D. (2001) 
#' \emph{Multiple-period performance attribution using the Brinson model}. 
#' Journal of Performance Measurement. Fall. p. 12-22 \cr
#' @keywords arithmetic attribution, Davies and Laker linking
#' @examples
#' 
#' data(attrib)
#' DaviesLaker(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], wb = attrib.weights[2, ])
#' 
#' @export
DaviesLaker <-
function(Rp, wp, Rb, wb,
         annualization = "none",
         annualization_scale = NA,
         impute_returns = TRUE)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # Davies and Laker linking. Used internally by the Attribution function
  
    # Inputs:
    # Rp       xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
  
    # Outputs: 
    # This function returns the data.frame with original attribution effects
    # and total attribution effects over multiple periods
    
    # FUNCTION:
    WP = wp
    WB = wb
    if (is.vector(wp)){
      wp = xts::as.xts(matrix(rep(wp, nrow(Rp)), nrow(Rp), ncol(Rp), byrow = TRUE), 
                       zoo::index(Rp))
      colnames(wp) = colnames(Rp)
    }
    else{
      wp = PerformanceAnalytics::checkData(WP)
    }
    if (is.vector(wb)){
      wb = xts::as.xts(matrix(rep(wb, nrow(Rb)), nrow(Rb), ncol(Rb), byrow = TRUE), 
                       zoo::index(Rb))
      colnames(wb) = colnames(Rb)
    }
    else{
      wb = PerformanceAnalytics::checkData(WB)
    }
    
    annualization = switch(annualization,
                           "none" = "none",
                           "standard" = "standard", 
                           stop("Valid annualization methods are 'none' and 'standard'"))
    
    if(annualization == "standard" && NROW(Rp) > 1) {
      freq = xts::periodicity(Rp)
      if(freq$scale != "daily" && !is.na(annualization_scale)) {
        warning("Ignoring specified annualization scale as it is only applicable for data with a daily periodicty")
      }
      if(freq$scale != "daily") {
        num_periods = NROW(Rp)
        scale = switch(freq$scale,
                       minute = stop("Data periodicity too high"),
                       hourly = stop("Data periodicity too high"),
                       weekly = 52,
                       monthly = 12,
                       quarterly = 4,
                       yearly = 1
        )
      } else if(!is.na(annualization_scale) && annualization_scale == "monthly") {
        # Get the number of periods as a fractional number of months
        num_periods = getMonthlyPeriods(xts::first(zoo::index(Rp)), xts::last(zoo::index(Rp)))
        # the scale is always 12 for 'monthy' annualization scale
        scale = 12
      } else {
        num_periods = NROW(Rp)
        # if provided scale is valid use it, else use the default
        scale = ifelse(!is.na(annualization_scale) && is.numeric(annualization_scale), annualization_scale, 252)
      }
    }
    
    Rp_raw = Rp
    Rb_raw = Rb
    if(impute_returns == TRUE) {
      # It is commmon to impute returns from the other entity when one asset's returns are missing in the
      # portfoli or benchmark due to it having a zero weight. This imputation is only relevant for 
      # computing attribution effects but should not be used when computing cumulative asset returns
      # (annualized or otherwise).
      wp_zero_weight_indexes = which(wp == 0 | is.na(wp))
      zoo::coredata(Rp)[wp_zero_weight_indexes] = zoo::coredata(Rb)[wp_zero_weight_indexes]
      
      wb_zero_weight_indexes = which(wb == 0 | is.na(wb))
      zoo::coredata(Rb)[wb_zero_weight_indexes] = zoo::coredata(Rp)[wb_zero_weight_indexes]
    }
    
    rp = PerformanceAnalytics::Return.portfolio(Rp, WP, geometric = FALSE)
    rb = PerformanceAnalytics::Return.portfolio(Rb, WB, geometric = FALSE)
    colnames(rp) = "Total"
    colnames(rb) = "Total"
    cumulative_rp = PerformanceAnalytics::Return.cumulative(rp)
    cumulative_rb = PerformanceAnalytics::Return.cumulative(rb)
    
    # Allocation notional fund returns
    bs = xts::reclass(rowSums((zoo::coredata(wp) * zoo::coredata(Rb[, 1:ncol(wp)]))), Rp) 
    # Selection notional fund returns
    rs = xts::reclass(rowSums((zoo::coredata(wb) * zoo::coredata(Rp[, 1:ncol(wb)]))), Rp) 
    a = apply(1 + bs, 2, prod) - apply(1 + rb, 2, prod)
    s = apply(1 + rs, 2, prod) - apply(1 + rb, 2, prod)
    i = apply(1 + rp, 2, prod) - apply(1 + rs, 2, prod) - 
      apply(1 + bs, 2, prod) + apply(1 + rb, 2, prod)
    
    # Compute attribution effects (Brinson, Hood and Beebower model)
    allocation = zoo::coredata(wp - wb) * Rb
    selection = zoo::coredata(wb) * (Rp - zoo::coredata(Rb))
    interaction = zoo::coredata(wp - wb) * (Rp - zoo::coredata(Rb))
    n = ncol(allocation)               # number of segments
    # We use the zoo version of cbind to avoid the column names from being mangled 
    # which the version in xts does without the option to override that behavior
    allocation = xts::as.xts(zoo::cbind.zoo(allocation, rowSums(allocation)))
    names(allocation)[n + 1] = "Total"  
    selection = xts::as.xts(zoo::cbind.zoo(selection, rowSums(selection)))
    names(selection)[n + 1] = "Total"   
    interaction = xts::as.xts(zoo::cbind.zoo(interaction, rowSums(interaction)))
    names(interaction)[n + 1] = "Total"
    
    allocation = rbind(as.data.frame(allocation), 
                       c(rep(NA, ncol(allocation) - 1), a))
    selection = rbind(as.data.frame(selection), 
                      c(rep(NA, ncol(selection) - 1), s))
    interaction = rbind(as.data.frame(interaction), 
                        c(rep(NA, ncol(interaction) - 1), i))
    rownames(allocation)[nrow(allocation)] = "Total"
    rownames(selection)[nrow(selection)] = "Total"
    rownames(interaction)[nrow(allocation)] = "Total"

    # Arithmetic excess returns + annualized arithmetic excess returns
    excess.returns = rp - zoo::coredata(rb)
    if (nrow(rp) > 1){
      cumulative_er = cumulative_rp - cumulative_rb
      if(annualization != "none") {
        if(freq$scale == "daily" && !is.na(annualization_scale)) {
          ann_er = prod(1 + rp)^(scale/num_periods) - prod(1 + zoo::coredata(rb))^(scale/num_periods)
        } else {
          ann_er = PerformanceAnalytics::Return.annualized.excess(rp, rb, geometric = FALSE)
        }
        excess.returns = rbind(as.matrix(excess.returns), cumulative_er, ann_er)
        rownames(excess.returns)[NROW(excess.returns)] = "Annualized"
      } else {
        excess.returns = rbind(as.matrix(excess.returns), cumulative_er)
      }
    }
    else {
      excess.returns = as.matrix(xts::xts(excess.returns, order.by = zoo::index(Rp)))
      # Since we only have one observation, the 'Cumulative Return' is just the same as that of the 
      # individual period but for consistency of returned result structure we add a 'Cumulative Return' row
      excess.returns = rbind(excess.returns, excess.returns)
      rownames(excess.returns)[NROW(excess.returns)] = "Cumulative Return"
    }
    colnames(excess.returns) = "Arithmetic"
    
    # Compute total portfolio returns
    if (nrow(rp) > 1){
      if(annualization != "none") {
        if(freq$scale == "daily" && !is.na(annualization_scale)) {
          ann_rp = prod(1 + rp)^(scale/num_periods) - 1
        } else {
          ann_rp = PerformanceAnalytics::Return.annualized(rp, geometric = TRUE)
        }
        port.returns = rbind(as.matrix(rp), cumulative_rp, ann_rp)
        rownames(port.returns)[NROW(port.returns)] = "Annualized"
      } else {
        port.returns = rbind(as.matrix(rp), cumulative_rp)
      }
    }
    else {
      # Since we only have one observation, the 'Cumulative Return' is just the same as that of the 
      # individual period but for consistency of returned result structure we add a 'Cumulative Return' row
      port.returns = rbind(as.matrix(rp), as.matrix(rp))
      rownames(port.returns)[NROW(port.returns)] = "Cumulative Return"
    }
    
    # Compute total benchmark returns
    if (nrow(rb) > 1){
      if(annualization != "none") {
        if(freq$scale == "daily" && !is.na(annualization_scale)) {
          ann_rb = prod(1 + rb)^(scale/num_periods) - 1
        } else {
          ann_rb = PerformanceAnalytics::Return.annualized(rb, geometric = TRUE)
        }
        bench.returns = rbind(as.matrix(rb), cumulative_rb, ann_rb)
        rownames(bench.returns)[NROW(bench.returns)] = "Annualized"
      } else {
        bench.returns = rbind(as.matrix(rb), cumulative_rb)
      }
    }
    else {
      # Since we only have one observation, the 'Cumulative Return' is just the same as that of the 
      # individual period but for consistency of returned result structure we add a 'Cumulative Return' row
      bench.returns = rbind(as.matrix(rb), as.matrix(rb))
      rownames(bench.returns)[NROW(bench.returns)] = "Cumulative Return"
    }
    
    result = list()
    result[[1]] = excess.returns
    result[[2]] = allocation
    result[[3]] = selection
    result[[4]] = interaction
    result[[5]] = port.returns
    result[[6]] = bench.returns
    
    names(result) = c("Excess returns", "Allocation", "Selection", 
                      "Interaction", "Portfolio returns", "Benchmark returns")
    return(result)
}
