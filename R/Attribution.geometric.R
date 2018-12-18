#' performs category-based geometric attribution 
#' 
#' Performs category-based geometric attribution of excess return. Calculates 
#' total geometric attribution effects over multiple periods. Used internally
#' by the \code{\link{Attribution}} function. 
#' No interaction effects are computed and the allocation & selection effects are as 
#' defined by the Brinson Fachler method. In addition, the individual category
#' effects are NOT adjusted using the possible approach outlined in the Carl Bacon
#' book. In any case these adjustments are small most of the time. Geometric attribution effects, in 
#' contrast with arithmetic, do naturally link over time multiplicatively.
#' 
#' The total geometric excess return over time is linked to the periodic geometric effects as follows:
#' \deqn{\frac{(1+R_{p})}{1+R_{b}}-1=\prod^{n}_{t=1}(1+A_{t}^{G}) \times \prod^{n}_{t=1}(1+S_{t}^{G})-1}{(1 + R_p) / (1 + R_b) - 1 = \prod {t=1 to n} (1 + A_t^G) * \prod {t=1 to n} (1+S_t^G) - 1}
#' where the total allocation effect at time \eqn{t} is:
#' \deqn{A_{t}^{G}=\frac{1+R_{bst}}{1+R_{bt}}-1}{A_t^G = (1 + R_bst) / (1 + R_bt) - 1}
#' and the total selection effect at time \eqn{t} is:
#' \deqn{S_{t}^{G}=\frac{1+R_{pt}}{1+R_{bst}}-1}{S_t^G = (1 + R_pt) / (1 + R_bst) - 1}
#' where the semi-notional fund returns at time \eqn{t} is:
#' \deqn{R_{bst}=\sum^{n}_{i=1}w_{pit}\times R_{bit}}{R_bst = \sum {i=1 to n} (w_pit * R_bit)}
#' with
#' \eqn{w_{pt}}{w_pt} being the portfolio weights at time \eqn{t},
#' \eqn{w_{bt}}{w_bt} being the benchmark weights at time \eqn{t},
#' \eqn{R_{pt}}{R_pt} being the portfolio returns at time \eqn{t},
#' \eqn{R_{bt}}{R_bt} being benchmark returns at time \eqn{t},
#' \eqn{R_{p}}{R_p} being the total portfolio returns,
#' \eqn{R_{b}}{R_b} being the total benchmark returns	and 
#' \eqn{n} being the number of periods.
#' 
#' The multi-currency geometric attribution is handled following the Appendix B
#' (Bacon, 2004). 
#' 
#' The individual selection effects are computed using:
#' \deqn{S_i = w_{pi}\times\left(\frac{1+R_{pli}}{1+R_{bli}}-1\right)\times \left(\frac{1+R_{bli}}{1+R_{bsl}}\right)}{S_i = w_pi * ((1 + R_pli) / (1 + R_bli) - 1) * ((1 + R_bli) / (1 + R_bsl))}
#' 
#' The individual allocation effects are computed using:
#' \deqn{A_i = (w_{pi}-w_{bi})\times\left(\frac{1+R_{bhi}}{1+R_{bl}}-1\right)}{A_i = (w_pi - w_bi) * ((1 + R_bhi) / (1 + R_bl) - 1)}
#' 
#' where the total semi-notional returns hedged into the base currency is:
#' \deqn{R_{bsh} = \sum^{n}_{i=1}(((w_{pi} - w_{bi})\times R_{bhi}) + w_{bi}\times R_{bli})}{R_bsh = \sum {i=1 to n} (((w_pi - w_bi) * R_bhi) + w_bi * R_bli)}
#' and the total semi-notional returns in the local currency is given by:
#' \deqn{R_{bsl} = \sum^{n}_{i=1}(w_{pi} \times R_{bli})}{R_bsl = \sum {i=1 to n} (w_pi * R_bli)}
#' with \eqn{R_{pli}}{R_pli} being the portfolio returns in the local currency,
#' \eqn{R_{bli}}{R_bli} being the benchmark returns in the local currency,
#' \eqn{R_{bhi}}{R_bhi} being the benchmark returns hedged into the base currency,
#' \eqn{R_{bl}}{R_bl} being the total benchmark returns in the local currency and
#' \eqn{R_{pl}}{R_pl} being the total portfolio returns in the local currency
#' 
#' The total excess returns are decomposed into:
#' \deqn{\frac{(1+R_{p})}{1+R_{b}}-1=\frac{1+R_{bsh}}{1+R_{bl}}\times\frac{1+R_{pl}}{1+R_{bsl}}\times\frac{1+R_{p}}{1+R_{pl}}\times\frac{1+R_{bl}}{1+R_{b}}\times\frac{1+R_{bsl}}{1+R_{bsh}}-1}{(1 + R_p)  / (1 + R_b) - 1 = ((1 + R_bsh) / (1 + R_bl)) * ((1 + R_pl) / (1 + R_bsl)) * ((1 + R_p) / (1 + R_pl)) * ((1 + R_bl) / (1 + R_b)) * ((1 + R_bsl) / (1 + R_bsh)) - 1}
#' 
#' where the first term corresponds to the allocation, second to the selection,
#' the next two to the naive currency attribution effect and the last to the hedging cost transferred.
#' 
#' @aliases Attribution.geometric
#' @param Rp xts of portfolio returns
#' @param wp xts of portfolio weights
#' @param Rb xts of benchmark returns
#' @param wb xts of benchmark weights
#' @param wpf vector, xts, data frame or matrix with portfolio weights of 
#' currency forward contracts
#' @param wbf vector, xts, data frame or matrix with benchmark weights of 
#' currency forward contracts
#' @param S (T+1) x n xts, data frame or matrix with spot rates. The first date
#' should coincide with the first date of portfolio returns
#' @param Fp (T+1) x n xts, data frame or matrix with forward rates for contracts in the portfolio. 
#' The first date should coincide with the first date of portfolio returns
#' @param Fb (T+1) x n xts, data frame or matrix with forward rates for contracts in the benchmark. 
#' The first date should coincide with the first date of benchmark returns
#' @param Rpl xts, data frame or matrix of portfolio returns in local currency
#' @param Rbl xts, data frame or matrix of benchmark returns in local currency
#' @param Rbh xts, data frame or matrix of benchmark returns hedged into the
#' base currency
#' @param contribution TRUE/FALSE, whether to also compute and return portfolio & benchmark contributions
#' for each category and period. Defaults to FALSE.
#' @param annualization Used to select the annualization method for multi-period excess returns, contributions
#' and attribution effects. May be any of:
#' \itemize{\item none - no annualized numbers are returned
#' \item standard 
#' \item proportional}
#' The default is 'none'. See vignette on annualization for detailed descriptions of the supported methods
#' @return This function returns the list with attribution effects (allocation
#' or selection effect) including total multi-period attribution effects
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 
#' 2009. Chapter 18-19 \cr Bacon, C. \emph{Practical Portfolio Performance 
#' Measurement and Attribution}. Wiley. 2004. Chapter 5, 8, Appendix A \cr
#' @keywords attribution, geometric attribution, geometric linking
#' @examples
#' 
#' data(sample_data)
#' Attribution.geometric(Rp = multi_period_portf_2$Rp, wp = multi_period_portf_2$wp, 
#'                       Rb = multi_period_portf_2$Rb, wb = multi_period_portf_2$wb)
#' 
#' @export
Attribution.geometric <-
function(Rp, wp, Rb, wb, 
         wpf = NA, wbf = NA, S = NA, Fp = NA, Fb = NA, Rpl = NA, Rbl = NA, Rbh = NA, 
         contribution = FALSE, annualization = "none")
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to perform the geometric attribution analysis.
  
    # Inputs:
    # Rp       xts of portfolio returns
    # wp       xts of portfolio weights
    # Rb       xts of benchmark returns
    # wb       xts of benchmark weights
    # wpf      vector, xts, data frame or matrix with portfolio weights of
    #          currency forward contracts
    # wbf      vector, xts, data frame or matrix with benchmark weights of 
    #          currency forward contracts
    # S        (T+1) x n xts, data frame or matrix with spot rates
    # Fp       (T+1) x n xts, data frame or matrix with forward rates for portfolio
    # Fb       (T+1) x n xts, data frame or matrix with forward rates for benchmark
    # Rpl      xts, data frame or matrix of portfolio returns in local currency
    # Rbl      xts, data frame or matrix of benchmark returns in local currency
    # Rbh      xts, data frame or matrix of benchmark returns hedged into the
    #          base currency
  
    # Outputs: 
    # This function returns the list with attribution effects (allocation or
    # selection effect) including total multi-period  attribution effects
  
    # FUNCTION:
  
    # Transform data to the xts objects
    Rb = PerformanceAnalytics::checkData(Rb)
    Rp = PerformanceAnalytics::checkData(Rp)
    WP = wp # Save original weights in order to avoid double conversion later
    WB = wb
    WPF = wpf # Save original weights in order to avoid double conversion later
    WBF = wbf
    
    
    if (is.vector(wp)){
      wp = xts::as.xts(matrix(rep(wp, nrow(Rp)), nrow(Rp), ncol(Rp), byrow = TRUE), 
                  index(Rp))
      colnames(wp) = colnames(Rp)
    }
    else{
      wp = PerformanceAnalytics::checkData(WP)
    }
    if (is.vector(wb)){
      wb = xts::as.xts(matrix(rep(wb, nrow(Rb)), nrow(Rb), ncol(Rb), byrow = TRUE), 
                  index(Rb))
      colnames(wb) = colnames(Rb)
    }
    else{
      wb = PerformanceAnalytics::checkData(WB)
    }
    
    if (!is.na(wpf) && is.vector(wpf)){
      wpf = xts::as.xts(matrix(rep(wpf, nrow(Rp)), nrow(Rp), ncol(Rp), byrow = TRUE), 
                   index(Rp))
      colnames(wpf) = colnames(Rp)
    }
    else{
      wpf = ifelse((is.na(WPF) || is.null(WPF)), WPF, PerformanceAnalytics::checkData(WPF))
    }
    if (!is.na(wbf) && is.vector(wbf)){
      wbf = xts::as.xts(matrix(rep(wbf, nrow(Rb)), nrow(Rb), ncol(Rb), byrow = TRUE), 
                   index(Rb))
      colnames(wbf) = colnames(Rb)
    }
    else{
      wbf = ifelse((is.na(WBF) || is.null(WBF)), WBF, PerformanceAnalytics::checkData(WBF))
    }
    
    annualization = switch(annualization,
                           "none" = "none",
                           "standard" = "standard", 
                           stop("Valid annualization methods are 'none' and 'standard'"))
    
    if(annualization == "standard" && NROW(Rp) > 1) {
      num_periods = NROW(Rp)
      freq = xts::periodicity(Rp)
      scale = switch(freq$scale,
                     minute = stop("Data periodicity too high"),
                     hourly = stop("Data periodicity too high"),
                     daily = 252,
                     weekly = 52,
                     monthly = 12,
                     quarterly = 4,
                     yearly = 1
      )
    }
    
    currency = !(is.null(dim(Rpl)) & is.null(dim(Rbl)) & is.null(dim(Rbh)))
    
    # Get total portfolio returns
    if (is.vector(WP) & is.vector(WB)){
      # If we have just one observation we simply sum up the contributions
      if(NROW(Rp) == 1 & NROW(Rb) == 1) {
        rp = as.matrix(sum(WP*Rp))
        rb = as.matrix(sum(WB*Rb))
        port_contr = as.matrix(sum(WP*Rp))
        bmk_contr = as.matrix(sum(WB*Rb))
      } else {
        port_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rp, WP, contribution = contribution)
        bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rb, WB, contribution = contribution)
        rp = port_returns_and_contr[,1]
        rb = bmk_returns_and_contr[,1]
        if(contribution) {
          port_contr = port_returns_and_contr[,-1]
          names(port_contr) = names(Rp)
          bmk_contr = bmk_returns_and_contr[,-1]
          names(bmk_contr) = names(Rb)
        }
      }
    } else{
      # If we have just one observation we simply sum up the contributions
      if(NROW(Rp) == 1 & NROW(WP) == 1 & NROW(Rb) == 1 & NROW(WB) == 1) {
        rp = as.matrix(sum(zoo::coredata(WP)*zoo::coredata(Rp)))
        rb = as.matrix(sum(zoo::coredata(WB)*zoo::coredata(Rb)))
        port_contr = as.matrix(zoo::coredata(WP)*zoo::coredata(Rp))
        bmk_contr = as.matrix(zoo::coredata(WB)*zoo::coredata(Rb))
      } else {
        port_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rp, WP, contribution = contribution)
        bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rb, WB, contribution = contribution)
        rp = port_returns_and_contr[,1]
        rb = bmk_returns_and_contr[,1]
        if(contribution) {
          port_contr = port_returns_and_contr[,-1]
          names(port_contr) = names(Rp)
          bmk_contr = bmk_returns_and_contr[,-1]
          names(bmk_contr) = names(Rb)
        }
      }
    }
    names(rp) = rownames(rp) = "Total"                    
    names(rb) = rownames(rb) = "Total"
    
    # Allocation notional fund returns
    bs = reclass(rowSums((wp * zoo::coredata(Rb[, 1:ncol(wp)]))), rp)
    if (!currency){
      # Geometric attribution effects for individual categories
      allocation = ((1 + Rb) / (1 + rep(rb, ncol(Rp))) - 1) * zoo::coredata(wp - wb) 
      selection = zoo::coredata(wp) * (Rp - zoo::coredata(Rb)) / (1 + rep(bs, ncol(Rp)))
      colnames(allocation) = colnames(Rp)

    } else{
      Rpl = PerformanceAnalytics::checkData(Rpl)
      Rbl = PerformanceAnalytics::checkData(Rbl)
      Rbh = PerformanceAnalytics::checkData(Rbh)
      
      
      if (!is.null(dim(S)) & !is.null(dim(Fp)) & !is.null(dim(Fb))){
        S = PerformanceAnalytics::checkData(S)
        Fp = PerformanceAnalytics::checkData(Fp)
        Fb = PerformanceAnalytics::checkData(Fb)
        
        Rc = stats::lag(S, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1
        Rpd = stats::lag(Fp, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1
        Rbd = stats::lag(Fb, -1)[1:nrow(Rb), ] / S[1:nrow(Rb), ] - 1
        
        Rpe = Rc - zoo::coredata(Rpd)
        Rbe = Rc - zoo::coredata(Rbd)
        
        Rl = Rb - zoo::coredata(Rc)
        Rpf = Rpe / (1 + Rpd)
        Rbf = Rbe / (1 + Rbd)
        
        # Recompute total portfolio returns to include forward contracts in the portfolio
        if (is.vector(WP) & is.vector(WB) & is.vector(WPF) & is.vector(WBF)){
          # If we have just one observation we simply sum up the contributions
          if(NROW(Rp) == 1 & NROW(Rb) == 1 & NROW(Rpf) == 1 & NROW(Rbf) == 1) {
            rp = as.matrix(sum(c(WP, WPF)*cbind(Rp, Rpf)))
            port_contr = as.matrix(c(WP, WPF)*cbind(Rp, Rpf))
            rb = as.matrix(sum(c(WB, WBF)*cbind(Rb, Rbf)))
            bmk_contr = as.matrix(c(WB, WBF)*cbind(Rb, Rbf))
          } else {
            if(!is.null(WPF) & !is.null(WBF)) {
              port_returns_and_contr = PerformanceAnalytics::Return.portfolio(cbind(Rp, Rpf), c(WP, WPF), contribution = contribution)
              bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(cbind(Rb, Rbf), c(WB, WBF), contribution = contribution)
            } else {
              port_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rp, WP, contribution = contribution)
              bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rb, WB, contribution = contribution)
            }
            rp = port_returns_and_contr[,1]
            rb = bmk_returns_and_contr[,1]
            if(contribution) {
              port_contr = port_returns_and_contr[,-1]
              names(port_contr) = names(Rp)
              bmk_contr = bmk_returns_and_contr[,-1]
              names(bmk_contr) = names(Rb)
            }
          }
        } else{
          # If we have just one observation we simply sum up the contributions
          if(NROW(Rp) == 1 & NROW(wp) == 1 & NROW(Rb) == 1 & NROW(wb) == 1 & 
             NROW(Rpf) == 1 & NROW(wpf) == 1 & NROW(Rbf) == 1 & NROW(wbf) == 1) {
            rp = as.matrix(sum(zoo::coredata(cbind(wp, wpf))*zoo::coredata(cbind(Rp, Rpf))))
            port_contr = as.matrix(zoo::coredata(cbind(wp, wpf))*zoo::coredata(cbind(Rp, Rpf)))
            rb = as.matrix(sum(zoo::coredata(cbind(wb, wbf))*zoo::coredata(cbind(Rb, Rbf))))
            bmk_contr = as.matrix(zoo::coredata(cbind(wb, wbf))*zoo::coredata(cbind(Rb, Rbf)))
          } else {
            if(!is.null(wpf) & !is.null(wbf)) {
              port_returns_and_contr = PerformanceAnalytics::Return.portfolio(cbind(Rp, Rpf), cbind(wp, wpf), contribution = contribution)
              bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(cbind(Rb, Rbf), cbind(wb, wbf), contribution = contribution)
            } else {
              port_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rp, wp, contribution = contribution)
              bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rb, wb, contribution = contribution)
            }
            rp = port_returns_and_contr[,1]
            rb = bmk_returns_and_contr[,1]
            if(contribution) {
              port_contr = port_returns_and_contr[,-1]
              names(port_contr) = names(Rp)
              bmk_contr = bmk_returns_and_contr[,-1]
              names(bmk_contr) = names(Rb)
            }
          }
        }
        names(rp) = rownames(rp) = "Total"                    
        names(rb) = rownames(rb) = "Total"
      }
      
      bsl = reclass(rowSums(Rbl * wp), Rpl)
      bsh = reclass(rowSums(((wp - wb) * Rbh + wb * Rbl)), Rpl)
      rpl = reclass(rowSums(Rpl * wp), Rpl)
      rbl = reclass(rowSums(Rbl * wb), Rbl)
      allocation = zoo::coredata(wp - wb) * ((1 + Rbh) / (1 + rep(rbl, ncol(Rbh))) - 1)
      selection = zoo::coredata(wp) * ((1 + Rpl) / (1 + Rbl) - 1) * ((1 + Rbl) / 
        (1 + rep(bsl, ncol(Rbl))))
      hedge = (1 + bsl) / (1 + bsh) - 1
      currency.attr = (1 + rp) * (1 + rbl) / (1 + rpl) / (1 + rb) - 1
      curr = cbind(hedge, currency.attr)
      colnames(curr) = c("Hedging", "Currency attribution")
    }
    
    # Total attribution effects are computed as a sum of individual effects
    # We use the zoo version of cbind to avoid the column names from being mangled 
    # which the version in xts does without the option to override that behavior
    allocation = xts::as.xts(zoo::cbind.zoo(allocation, rowSums(allocation)))
    selection = xts::as.xts(zoo::cbind.zoo(selection, rowSums(selection)))
    colnames(allocation)[ncol(allocation)] = "Total"
    colnames(selection)[ncol(selection)] = "Total"
    
    # Link single-period attribution effects
    a = (apply(1 + allocation[, ncol(allocation)], 2, prod) - 1)
    s = (apply(1 + selection[, ncol(selection)], 2, prod) - 1)
    allocation = rbind(as.data.frame(allocation), 
                       c(rep(NA, ncol(allocation) - 1), a))
    selection = rbind(as.data.frame(selection), 
                      c(rep(NA, ncol(selection) - 1), s))
    rownames(allocation)[nrow(allocation)] = "Total"
    rownames(selection)[nrow(selection)] = "Total"
    
    
    if(annualization == "standard") {
      ann_allocation = (1 + allocation["Total",])^(scale/num_periods) - 1
      allocation = rbind(allocation, zoo::coredata(ann_allocation))
      rownames(allocation)[NROW(allocation)] = "Annualized"
      
      ann_selection = (1 + selection["Total",])^(scale/num_periods) - 1
      selection = rbind(selection, zoo::coredata(ann_selection))
      rownames(selection)[NROW(selection)] = "Annualized"
    }
    
    # Geometric excess returns + annualized geometric excess returns
    excess.returns = (1 + rp) / (1 + zoo::coredata(rb)) - 1
    if (nrow(rp) > 1){
      cumulative_er = (1 + PerformanceAnalytics::Return.cumulative(rp))/(1 + PerformanceAnalytics::Return.cumulative(rb)) - 1
      if(annualization == "standard") {
        ann_er = PerformanceAnalytics::Return.annualized.excess(rp, rb)
        excess.returns = rbind(as.matrix(excess.returns), cumulative_er, ann_er)
      } else {
        excess.returns = rbind(as.matrix(excess.returns), cumulative_er)
      }
    }
    colnames(excess.returns) = "Geometric"
    
    result = list()
    result[[1]] = excess.returns
    result[[2]] = allocation
    result[[3]] = selection
    if (!currency){
      names(result) = c("Excess returns", "Allocation", "Selection")
    } else{
      result[[4]] = curr
      names(result) = c("Excess returns", "Allocation", "Selection", 
                        "Currency management")
    }
    
    if(contribution) {
      # Compute the multi-period contributions
      if(nrow(port_contr) > 1) {
        total_port_contr = PerformanceAnalytics::to.period.contributions(port_contr, period = "all")[,1:NCOL(port_contr)]
        port_contr = rbind(as.data.frame(port_contr), zoo::coredata(total_port_contr))
        rownames(port_contr)[NROW(port_contr)] = "Total"
        
        if(annualization == "standard") {
          ann_total_port_contr = (1 + total_port_contr)^(scale/num_periods) - 1
          port_contr = rbind(as.data.frame(port_contr), zoo::coredata(ann_total_port_contr))
          rownames(port_contr)[NROW(port_contr)] = "Annualized"
        }
      }
      if(nrow(bmk_contr) > 1) {
        total_bmk_contr = PerformanceAnalytics::to.period.contributions(bmk_contr, period = "all")[,1:NCOL(bmk_contr)]
        bmk_contr = rbind(as.data.frame(bmk_contr), zoo::coredata(total_bmk_contr))
        rownames(bmk_contr)[NROW(bmk_contr)] = "Total"
        
        if(annualization == "standard") {
          ann_total_bmk_contr = (1 + total_bmk_contr)^(scale/num_periods) - 1 
          bmk_contr = rbind(as.data.frame(bmk_contr), zoo::coredata(ann_total_bmk_contr))
          rownames(bmk_contr)[NROW(bmk_contr)] = "Annualized"
        }
      }
      
      result[[length(result) + 1]] = port_contr
      result[[length(result) + 1]] = bmk_contr
      names(result)[(length(result)-1):length(result)] = 
        c("Portfolio contribution to return", "Benchmark contribution to return")
    }
    return(result)
}
