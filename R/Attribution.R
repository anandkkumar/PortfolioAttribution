#' performs single-category attribution
#' 
#' Performs single-category attribution analysis. Portfolio 
#' performance measured relative to a benchmark gives an indication of the 
#' value-added by the portfolio. Equipped with weights and returns of portfolio
#' segments, we can dissect the value-added into useful components. This 
#' function is based on the group-based approach to the attribution. The 
#' workhorse is the Brinson model that explains the arithmetic difference 
#' between portfolio and benchmark returns. That is it breaks down the 
#' arithmetic excess returns at one category level. If returns and weights are available
#' at the lowest category level (e.g. for individual instruments), the aggregation up to
#' the chosen category from the hierarchy can be done using 
#' \code{\link{Return.level}} function. The attribution effects can be computed
#' for several periods. The multi-period summary is obtained using one of 
#' linking methods: Carino, Menchero, GRAP, Frongello or Davies Laker. It also
#' allows to break down the geometric excess returns, which link naturally over
#' time. Finally, it supports annualization of excess returns, attribution effects and contributions.
#' 
#' The arithmetic excess returns are decomposed into the sum of allocation, 
#' selection and interaction effects across \eqn{n} groups:
#' \deqn{R_{p}-R_{b}=\sum^{n}_{i=1}\left(A_{i}+S_{i}+I_{i}\right)}{R_p - R_b = \sum {i=1 to n} (A_i + S_i + I_i)}
#' The arithmetic attribution effects for the category i are computed
#' as suggested in the Brinson, Hood and Beebower (1986):
#' Allocation effect
#' \deqn{A_{i}=(w_{pi}-w_{bi})\times R_{bi}}{A_i = (w_pi - w_bi) * R_bi}
#' Selection effect
#' \deqn{S_{i}=w_{bi}\times(R_{pi}-R_{bi})}{S_i = w_bi * (R_pi - R_bi)}
#' Interaction effect
#' \deqn{I_{i}=(w_{pi}-w_{bi})
#' \times(R_{pi}-R_{bi})}{I_i = (w_pi - w_bi) * (R_pi - R_bi)}
#' \eqn{R_{p}}{R_p} - total portfolio returns,
#' \eqn{R_{b}}{R_b} - total benchmark returns, 
#' \eqn{w_{pi}}{w_pi} - weights of the category \eqn{i} in the portfolio, 
#' \eqn{w_{bi}}{w_bi} - weights of the category \eqn{i} in the benchmark, 
#' \eqn{R_{pi}}{R_pi} - returns of the portfolio category \eqn{i}, 
#' \eqn{R_{bi}}{R_bi} - returns of the  benchmark category \eqn{i}.
#' If Brinson and Fachler (1985) is selected the allocation effect differs:
#' \deqn{A_{i}=(w_{pi}-w_{bi})
#' \times (R_{bi} - R_{b})}{A_i = (w_pi - w_bi) * (R_bi - R_b)}
#' Depending on goals we can give priority to the allocation or to 
#' the selection effects. If the priority is given to the group allocation
#' the interaction term will be combined with the security selection effect
#' (top-down approach). If the priority is given to the security selection,
#' the interaction term will be combined with the asset-allocation effect
#' (bottom-up approach).
#' Usually we have more than one period. In that case individual arithmetic 
#' attribution effects should be adjusted using linking methods. Adjusted
#' arithmetic attribution effects can be summed up over time to provide the
#' multi-period summary: 
#' \deqn{R_{p}-R_{b}=\sum^{T}_{t=1}\left(A_{t}'+S_{t}'+I_{t}'\right)}{R_p - R_b = \sum {t=1 to T} (A_t'+S_t'+I_t')}
#' where \eqn{T} is the number of periods and prime stands for the adjustment 
#' (this is not applicable to Davies & Laker linking method).
#' The geometric attribution effects do not suffer from the linking problem.
#' Moreover we don't have the interaction term. For more details about the 
#' geometric attribution see the documentation to 
#' \code{\link{Attribution.geometric}}. Finally, arithmetic annualized excess 
#' returns are computed as the arithmetic difference between annualised 
#' portfolio and benchmark returns:
#' \deqn{E^A_ann = R_{pa}-R_{ba}}{E^A_ann = R_pa - R_ba} and the geometric annualized excess
#' returns are computed as the geometric difference between annualized 
#' portfolio and benchmark returns: 
#' \deqn{E^G_ann =\frac{1+R_{pa}}{1+R_{ba}}-1}{E^G_ann = (1 + R_pa) / (1 + R_ba) - 1}
#' In the case of \strong{arithmetic attribution} of a multi-currency portfolio, the currency return, currency
#' surprise and forward premium should be specified. The multi-currency
#' arithmetic attribution is handled following Ankrim and Hensel (1992).
#' Currency returns are decomposed into the sum of the currency surprise and
#' the forward premium: \deqn{R_{ci} = R_{ei} + R_{di}}{R_ci = R_ei + R_di}
#' where 
#' \deqn{R_{ei} = \frac{S_{i}^{t+1} - F_{i}^{t+1}}{S_{i}^{t}}}{R_ei = (S_i^(t+1) - F_i^(t+1)) / S_i^t}
#' \deqn{R_{di} = \frac{F_{i}^{t+1}}{S_{i}^{t}} - 1}{R_di = (F_i^(t+1) / S_i^t) - 1}
#' \eqn{S_i^t}{S_i^t} - spot rate for asset \eqn{i} at time \eqn{t},
#' \eqn{S_i^{t+1}}{S_i^(t+1)} - spot rate for asset \eqn{i} at time \eqn{t+1} and
#' \eqn{F_i^{t+1}}{F_i^(t+1)} - forward rate for asset \eqn{i} at time \eqn{t} for converstion at time \eqn{t+1}. 
#' Excess returns are decomposed into the sum of allocation, selection and 
#' interaction effects as in the standard Brinson model: 
#' \deqn{R_{p}-R_{b}=\sum^{n}_{i=1}\left(A_{i}+S_{i}+I_{i}\right)}{R_p - R_ b = \sum {i=1 to n} (A_i + S_ i + I_i)}
#' However the allocation effect is computed taking into account currency
#' effects:
#' \deqn{A_{i}=(w_{pi}-w_{bi})\times (R_{bi} - R_{ci} - R_{l})}{A_i = (w_pi - w_bi) * (R_bi - R_ci - R_l)}
#' Benchmark returns adjusted to the currency:
#' \deqn{R_{l} = \sum^{n}_{i=1}w_{bi}\times(R_{bi}-R_{ci})}{R_l = \sum {i=1 to n} (w_bi * (R_bi - R_ci))}
#' The contribution from the currency is analogous to asset allocation:
#' \deqn{C_{i} = (w_{pi} - w_{bi}) \times (R_{ei} - R_e) + (w_{pfi} - w_{bfi}) \times (R_{fi} - R_e)}{C_i = (w_pi - w_bi) * (R_ei - R_e) + (w_pfi - w_bfi) * (R_fi - R_e)}
#' where \deqn{R_e = \sum^{n}_{i=1}w_{bi}\times R_{ei}}{R_e = \sum {i=1 to n} (w_bi * R_ei)}
#' The final term, forward premium, is also analogous to the asset allocation:
#' \deqn{D_{i} = (w_{pi} - w_{bi}) \times (R_{di} - R_d)}{D_i = (w_pi - w_bi) * (R_di - R_d)}
#' where \deqn{R_d = \sum^{n}_{i=1}w_{bi}\times R_{di}}{R_d = \sum {i=1 to n} (w_bi * R_di)}
#' and \eqn{R_{di}}{R_di} is the forward premium.
#' In general if the intent is to estimate statistical parameters, the 
#' arithmetic excess return is preferred. However, due to the linking 
#' challenges, it may be preferable to use geometric excess return if the 
#' intent is to link and annualize excess returns.
#' 
#' @aliases Attribution
#' @param Rp T x n xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param Rb T x n xts, data frame or matrix of benchmark returns
#' @param wb vector, xts, data frame or matrix of benchmark weights
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
#' @param Rpl xts, data frame or matrix of portfolio returns in local currency (only used when geometric is set to TRUE)
#' @param Rbl xts, data frame or matrix of benchmark returns in local currency (only used when geometric is set to TRUE)
#' @param Rbh xts, data frame or matrix of benchmark returns hedged into the (only used when geometric is set to TRUE)
#' base currency
#' @param bf TRUE for Brinson and Fachler and FALSE for Brinson, Hood and 
#' Beebower arithmetic attribution. By default this set to TRUE and so Brinson and Fachler 
#' attribution is selected. This is only used for Arithmetic attribution while Geometric
#' attribution only supports Brinson Fachler.
#' @param method Used to select the priority between allocation and selection 
#' effects in arithmetic attribution. May be any of: \itemize{ \item none - 
#' present allocation, selection and interaction effects independently, 
#' \item top.down - the priority is given to the group allocation. Interaction
#' term is combined with the security selection effect, \item bottom.up - the 
#' priority is given to the security selection. Interaction term is combined 
#' with the group allocation effect} 
#' By default "none" is selected
#' @param linking Used to select the linking method to present the multi-period
#' summary of arithmetic attribution effects. May be any of: 
#' \itemize{\item carino - logarithmic linking coefficient method
#' \item menchero - Menchero's smoothing algorithm
#' \item grap - linking approach developed by GRAP
#' \item frongello - Frongello's linking method
#' \item davies.laker - Davies and Laker's linking method}
#' By default grap linking is selected. This option is ignored if 'geometric' is set to TRUE or
#' if the data does not imply multi-period attribution.
#' @param geometric TRUE/FALSE, whether to use geometric or arithmetic excess
#' returns for the attribution analysis. By default this is set to FALSE, which results
#' in arithmetic excess return attribution.
#' @param contribution TRUE/FALSE, whether to also compute and return portfolio & benchmark contributions
#' for each category and period. Defaults to FALSE.
#' @param adjusted TRUE/FALSE, whether to show original or smoothed attribution
#' effects for each period. By default unadjusted attribution effects are 
#' returned (this is not used for Davies and Laker's linking method for arithmetic attribution 
#' or for geometric attribution as it is not applicable in either case)
#' @param annualization Used to select the annualization method for multi-period excess returns, contributions
#' and attribution effects. May be any of:
#' \itemize{\item none - no annualized numbers are returned
#' \item standard 
#' \item proportional}
#' The default is 'none'. See vignette on annualization for detailed descriptions of the supported methods
#' @return returns a list with the following components: excess returns with
#' annualized excess returns over all periods, attribution effects (allocation, 
#' selection and interaction) and optionally, portfolio and benchmark contributions 
#' for each category and period.
#' @author Andrii Babii
#' @seealso \code{\link{Attribution.levels}}, 
#' \code{\link{Attribution.geometric}}
#' @references Ankrim, E. and Hensel, C. \emph{Multi-currency performance
#' attribution}. Russell Research Commentary. November 2002 \cr Bacon, C. 
#' \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley.
#' 2004. Chapter 5, 6, 8 \cr Christopherson, Jon A., Carino, David R., Ferson, 
#' Wayne E. \emph{Portfolio Performance Measurement and Benchmarking}. 
#' McGraw-Hill. 2009. Chapter 18-19 \cr Brinson, G. and Fachler, N. (1985)
#' \emph{Measuring non-US equity portfolio performance}. Journal of Portfolio
#' Management. Spring. p. 73 -76. \cr Gary P. Brinson, L. Randolph Hood, and 
#' Gilbert L. Beebower, \emph{Determinants of Portfolio Performance}. Financial
#' Analysts Journal. vol. 42, no. 4, July/August 1986, p. 39-44 \cr 
#' Karnosky, D. and Singer, B. \emph{Global asset management and performance 
#' attribution. The Research Foundation of the Institute of Chartered Financial
#' Analysts}. February 1994. \cr
#' @keywords attribution
#' @examples
#' 
#' data(sample_data)
#' Attribution(Rp = multi_period_portf_2$Rp, wp = multi_period_portf_2$wp, 
#'             Rb = multi_period_portf_2$Rb, wb = multi_period_portf_2$wb, 
#'             method = "top.down", linking = "grap")
#' 
#' @export
Attribution <- 
function (Rp, wp, Rb, wb, 
          wpf = NA, wbf = NA, S = NA, Fp = NA, Fb = NA, Rpl = NA, Rbl = NA, Rbh = NA,
          bf = TRUE,
          method = "none", 
          linking = "grap",
          geometric = FALSE, contribution = FALSE, adjusted = FALSE, 
          annualization = "none")
{   # @author Andrii Babii

    # DESCRIPTION:
    # Function to perform the attribution analysis.

    # Inputs:
    # Rp       T x n xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       T x n xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
    # wpf      vector, xts, data frame or matrix with portfolio weights of
    #          currency forward contracts
    # wbf      vector, xts, data frame or matrix with benchmark weights of 
    #          currency forward contracts
    # S        (T+1) x n xts, data frame or matrix with spot rates
    # Fp       (T+1) x n xts, data frame or matrix with forward rates for portfolio
    # Fb       (T+1) x n xts, data frame or matrix with forward rates for benchmark
    # Rpl      xts, data frame or matrix of portfolio returns in local currency (only used when geometric is set to TRUE)
    # Rbl      xts, data frame or matrix of benchmark returns in local currency (only used when geometric is set to TRUE)
    # Rbh      xts, data frame or matrix of benchmark returns hedged into the
    #          base currency (only used when geometric is set to TRUE)
  
    # Outputs: 
    # This function returns the attribution effects with multi-period summary
    # and annualized excess returns
  
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
    
    if (nrow(wp) < nrow(Rp)){ # Rebalancing occurs next day
      Rp = Rp[2:nrow(Rp)]
      Rb = Rb[2:nrow(Rb)]
    }
    if (ncol(Rb) == 1){
      Rb = xts::xts(matrix(rep(zoo::coredata(Rb), ncol(Rp)), nrow(Rp), ncol(Rp)),order.by=index(Rb))
    }
    if (ncol(Rb) != ncol(Rp)){
      stop("Please use benchmark xts that has columns with benchmarks for each
            asset or one common benchmark for all assets")
    }
    
    method = switch(method,
                    "none" = "none",
                    "top.down" = "top.down", 
                    "bottom.up" = "bottom.up",
                    stop("Valid methods are 'none', 'top.down' and 'bottom.up'"))
    linking = switch(linking,
                     "carino" = "carino", 
                     "menchero" = "menchero", 
                     "grap" = "grap", 
                     "frongello" = "frongello", 
                     "davies.laker" = "davies.laker",
                     stop("Valid linking options are 'carino', 'menchero', 'grap', 'frongello', 'davies.laker'"))
    annualization = switch(annualization,
                           "none" = "none",
                           "standard" = "standard", 
                           "proportional" = "proportional",
                           stop("Valid annualization methods are 'none', 'standard' and 'proportional'"))
    
    if(annualization %in% c("standard", "proportional") && NROW(Rp) > 1) {
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
    
    currency = !(is.null(dim(wpf)) & is.null(dim(wbf)) & 
                   is.null(dim(S)) & is.null(dim(Fp)) & is.null(dim(Fb)))
    
    if (geometric == FALSE & linking != "davies.laker"){ 
      # The function makes all computations for the arithmetic attribution
      # case (except for Davies and Laker linking)
    
      # Compute attribution effects (Brinson, Hood and Beebower model)
      # If portfolio is single-currency
      if (!currency){
        Rc = 0
        L = 0
        Rpbf = NULL
        WPF = WBF = NULL
      } else{         # If multi-currency portfolio
        S = PerformanceAnalytics::checkData(S)
        Fp = PerformanceAnalytics::checkData(Fp)
        Fb = PerformanceAnalytics::checkData(Fb)
        Rc = stats::lag(S, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1
        Rd = stats::lag(Fb, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1
        Re = Rc - zoo::coredata(Rd)
        Rl = Rb - zoo::coredata(Rc)
        Rpbf = Re / (1 + Rd)
        E = reclass(matrix(rep(rowSums(Re * zoo::coredata(wb)), ncol(Rb)), nrow(Rb),
                           ncol(Rb)), Rp)
        E = xts::as.xts(E)
        L = reclass(matrix(rep(rowSums(Rl * zoo::coredata(wb)), ncol(Rb)), nrow(Rb), 
                           ncol(Rb)), Rp)
        L = xts::as.xts(L)
        D = reclass(matrix(rep(rowSums(Rd * zoo::coredata(wb)), ncol(Rb)), nrow(Rb), 
                           ncol(Rb)), Rp)
        D = xts::as.xts(D)
        # Contribution to currency
        Cc = (wp - wb) * (Re - E) + (wpf - wbf) * (Rpbf - E) 
        # Forward premium
        Df = (wp - wb) * (Rd - D)
        # We use the zoo version of cbind to avoid the column names from being mangled 
        # which the version in xts does without the option to override that behavior
        Cc = xts::as.xts(zoo::cbind.zoo(Cc, rowSums(Cc)))
        Df = xts::as.xts(zoo::cbind.zoo(Df, rowSums(Df)))
        colnames(Cc) = c(colnames(S), "Total")
        colnames(Df) = colnames(Cc)
      }
      
      # Get total portfolio returns including any forward contracts, if present
      if (is.vector(WP) & is.vector(WB)){
        # If we have just one observation we simply sum up the contributions
        if(NROW(Rp) == 1 & NROW(Rb) == 1) {
          rp = as.matrix(sum(c(WP, WPF)*cbind(Rp, Rpbf)))
          rb = as.matrix(sum(c(WB, WBF)*cbind(Rb, Rpbf)))
          cumulative_rp = rp
          cumulative_rb = rb
          port_contr = as.matrix(c(WP, WPF)*cbind(Rp, Rpbf))
          bmk_contr = as.matrix(c(WB, WBF)*cbind(Rb, Rpbf))
        } else {
          if(!is.null(WPF) & !is.null(WBF)) {
            port_returns_and_contr = PerformanceAnalytics::Return.portfolio(cbind(Rp, Rpbf), c(WP, WPF), 
                                                      geometric = FALSE, contribution = contribution)
            bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(cbind(Rb, Rpbf), c(WB, WBF), 
                                                     geometric = FALSE, contribution = contribution)
          } else {
            port_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rp, WP, geometric = FALSE, contribution = contribution)
            bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rb, WB, geometric = FALSE, contribution = contribution)
          }
          rp = port_returns_and_contr[,1]
          rb = bmk_returns_and_contr[,1]
          cumulative_rp = PerformanceAnalytics::Return.cumulative(rp)
          cumulative_rb = PerformanceAnalytics::Return.cumulative(rb)
          if(contribution) {
            port_contr = port_returns_and_contr[,-1]
            names(port_contr) = names(Rp)
            bmk_contr = bmk_returns_and_contr[,-1]
            names(bmk_contr) = names(Rb)
          }
        }
      } else {
        # If we have just one observation we simply sum up the contributions
        if(NROW(Rp) == 1 & NROW(wp) == 1 & NROW(Rb) == 1 & NROW(wb) == 1) {
          rp = as.matrix(sum(zoo::coredata(cbind(wp, WPF))*zoo::coredata(cbind(Rp, Rpbf))))
          port_contr = as.matrix(zoo::coredata(cbind(wp, WPF))*zoo::coredata(cbind(Rp, Rpbf)))
          rb = as.matrix(sum(zoo::coredata(cbind(wb, WBF))*zoo::coredata(cbind(Rb, Rpbf))))
          bmk_contr = as.matrix(zoo::coredata(cbind(wb, WBF))*zoo::coredata(cbind(Rb, Rpbf)))
          cumulative_rp = rp
          cumulative_rb = rb
        } else {
          if(!is.null(WPF) & !is.null(WBF)) {
            port_returns_and_contr = PerformanceAnalytics::Return.portfolio(cbind(Rp, Rpbf), cbind(wp, WPF), 
                                                      geometric = FALSE, contribution = contribution)
            bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(cbind(Rb, Rpbf), cbind(wb, WBF), 
                                                     geometric = FALSE, contribution = contribution)
          } else {
            port_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rp, wp, geometric = FALSE, contribution = contribution)
            bmk_returns_and_contr = PerformanceAnalytics::Return.portfolio(Rb, wb, geometric = FALSE, contribution = contribution)
          }
          rp = port_returns_and_contr[,1]
          rb = bmk_returns_and_contr[,1]
          cumulative_rp = PerformanceAnalytics::Return.cumulative(rp)
          cumulative_rb = PerformanceAnalytics::Return.cumulative(rb)
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
      
      # Get individual attribution effects
      #if the benchmark weights are not specified allocation effect is equal to 0
      #selection contribution is equal to 0
      #if bm weights unknown all contribution is treated as interaction as it cannot be broken down, user is warned
      
      if(ncol(wb)==1){
        selection = (Rp  - zoo::coredata(Rb))*0
        allocation = (Rp  - zoo::coredata(Rb))*0
        interaction = zoo::coredata(wp) * (Rp - zoo::coredata(Rb))  
        warning("Benchmark weights unknown, all effects treated as interaction, returns wp*(Rp-Rb)")
      }
      else{
        if (bf == TRUE){ 
          # Brinson and Fachler (1985) allocation effect
          allocation = zoo::coredata(wp - wb) * (Rb - zoo::coredata(Rc) - zoo::coredata(L) - 
            rep(rb, ncol(Rb)))
        }else{
          # Brinson, Hood and Beebower (1986) allocation effect
          allocation = zoo::coredata(wp - wb) * (Rb - zoo::coredata(Rc) - zoo::coredata(L))
        }
                        
        selection = (Rp  - zoo::coredata(Rb)) * zoo::coredata(wb)
        interaction = zoo::coredata(wp - wb) * (Rp - zoo::coredata(Rb))         
      }
    
      
      # Get excess returns
      # Arithmetic excess returns + annualized arithmetic excess returns
      excess.returns = rp - zoo::coredata(rb)
      if (nrow(rp) > 1){
        cumulative_er = cumulative_rp - cumulative_rb
        if(annualization != "none") {
          ann_er = PerformanceAnalytics::Return.annualized.excess(rp, rb, geometric = FALSE)
          excess.returns = rbind(as.matrix(excess.returns), cumulative_er, ann_er)
        } else {
          excess.returns = rbind(as.matrix(excess.returns), cumulative_er)
        }
      }
      colnames(excess.returns) = "Arithmetic"
      
      # Get total attribution effects 
      n = ncol(allocation)               # number of segments
      # We use the zoo version of cbind to avoid the column names from being mangled 
      # which the version in xts does without the option to override that behavior
      allocation = xts::as.xts(zoo::cbind.zoo(allocation, rowSums(allocation)))
      names(allocation)[n + 1] = "Total"  
      selection = xts::as.xts(zoo::cbind.zoo(selection, rowSums(selection)))
      names(selection)[n + 1] = "Total"   
      interaction = xts::as.xts(zoo::cbind.zoo(interaction, rowSums(interaction)))
      names(interaction)[n + 1] = "Total"
      
      # Adjust attribution effects using one of linking methods if there are
      # mutliple periods
      if (nrow(allocation) > 1){
        if (linking == "carino"){
          allocation = Carino(rp, rb, allocation, adjusted)
          selection = Carino(rp, rb, selection, adjusted)
          interaction = Carino(rp, rb, interaction, adjusted)
        }
      
        if (linking == "menchero"){
          allocation = Menchero(rp, rb, allocation, adjusted)
          selection = Menchero(rp, rb, selection, adjusted)
          interaction = Menchero(rp, rb, interaction, adjusted)
        }    
      
        if (linking == "grap"){
          allocation = Grap(rp, rb, allocation, adjusted)
          selection = Grap(rp, rb, selection, adjusted)
          interaction = Grap(rp, rb, interaction, adjusted)
        }
      
        if (linking == "frongello"){
          allocation = Frongello(rp, rb, allocation, adjusted)
          selection = Frongello(rp, rb, selection, adjusted)
          interaction = Frongello(rp, rb, interaction, adjusted)
        }
        
        if(annualization == "standard") {
          ann_allocation = (1 + allocation["Total",])^(scale/num_periods) - 1
          allocation = rbind(allocation, zoo::coredata(ann_allocation))
          rownames(allocation)[NROW(allocation)] = "Annualized"
          
          ann_selection = (1 + selection["Total",])^(scale/num_periods) - 1
          selection = rbind(selection, zoo::coredata(ann_selection))
          rownames(selection)[NROW(selection)] = "Annualized"
          
          ann_interaction = (1 + interaction["Total",])^(scale/num_periods) - 1
          interaction = rbind(interaction, zoo::coredata(ann_interaction))
          rownames(interaction)[NROW(interaction)] = "Annualized"
        } else if(annualization == "proportional") {
          ann_excess_prop_ratio = as.numeric(ann_er/cumulative_er)
          
          ann_allocation = allocation["Total",] * ann_excess_prop_ratio
          allocation = rbind(allocation, zoo::coredata(ann_allocation))
          rownames(allocation)[NROW(allocation)] = "Annualized"
          
          ann_selection = selection["Total",] * ann_excess_prop_ratio
          selection = rbind(selection, zoo::coredata(ann_selection))
          rownames(selection)[NROW(selection)] = "Annualized"
          
          ann_interaction = interaction["Total",] * ann_excess_prop_ratio
          interaction = rbind(interaction, zoo::coredata(ann_interaction))
          rownames(interaction)[NROW(interaction)] = "Annualized"
        }
      }      
      
      # Select the appropriate result corresponding to the chosen method
      result = list()
      result[[1]] = excess.returns
      result[[2]] = allocation
      result[[3]] = selection
      if (method == "top.down"){     # Top-down attribution
        result[[3]] = result[[3]] + interaction
      }
      if (method == "bottom.up"){    # Bottom-up attribution
        result[[2]] = result[[2]] + interaction
      }
      if (method == "none"){
        result[[4]] = interaction
      }
   } else{ # The function takes output of the corresponding function 
            # (Attribution.geometric or DaviesLaker)
      if (geometric == TRUE){
        return(Attribution.geometric(Rp, WP, Rb, WB, WPF, WBF, S, Fp, Fb, Rpl, Rbl, Rbh, 
                                     contribution = contribution,
                                     # Geometric attribution does not support 'proportional' method
                                     annualization = ifelse(annualization == "proportional", "standard", annualization)))
      }
      
      if (linking == "davies.laker"){
        attrib = DaviesLaker(Rp, WP, Rb, WB)
      }
      result = attrib
    }
    
    # Label the output
    if (method == "none" | linking == "davies.laker"){
      names(result) = c("Excess returns", "Allocation", "Selection", 
                        "Interaction")
    } else{
      names(result) = c("Excess returns", "Allocation", "Selection")
    }
    
    # If multi-currency portfolio
    if (currency){
      result[[length(result) + 1]] = Cc
      result[[length(result) + 1]] = Df
      names(result)[(length(result)-1):length(result)] = 
        c("Currency management", "Forward Premium")
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
        } else if(annualization == "proportional") {
          ann_rp = PerformanceAnalytics::Return.annualized(rp, geometric = TRUE)
          ann_total_port_contr = total_port_contr * as.numeric(ann_rp/cumulative_rp)
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
        } else if(annualization == "proportional") {
          ann_rb = PerformanceAnalytics::Return.annualized(rb, geometric = TRUE)
          ann_total_bmk_contr = total_bmk_contr * as.numeric(ann_rb/cumulative_rb)
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
