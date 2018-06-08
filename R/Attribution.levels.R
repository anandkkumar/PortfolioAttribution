#' provides multi-level sector-based geometric attribution
#' 
#' Provides multi-level sector-based geometric attribution. The Brinson model
#' attributes excess returns at one level. This function works with more 
#' complex decision processes. For instance, the 3-level decision process 
#' may have the following levels: type of asset - country - sector. The 
#' levels should be specified in the vector with elements in the particular 
#' order: from the highest level to the lowest. Returns and weighs for 
#' portfolio and benchmark should be at the lowest level (e.g. individual 
#' instruments). Benchmark should have the same number of columns as portfolio.
#' That is there should be a benchmark for each instrument in the portfolio 
#' (possibly 0). The contribution to the allocation in the \eqn{i^{th}}
#' category for the \eqn{d^{th}} level is: 
#' \deqn{\left(^{d}w_{pi}-^{d}w_{bi}\right)\times
#' \left(\frac{1+^{d}R_{bi}}{1+^{d-1}R_{bi}}-1\right)
#' \times\frac{1+^{d-1}R_{bi}}{1+bs^{d-1}}}
#' The total attribution for each asset allocation step in the decision process
#' is: \deqn{\frac{1+^{d}bs}{1+^{d-1}bs}-1}
#' The final step, stock selection, is measured by:
#' \deqn{^{d}w_{pi}\times\left(\frac{1+R_{pi}}{1+^{d}R_{bi}}-1\right)
#' \times\frac{1+^{d}R_{bi}}{1+^{d}bs}}
#' The allocation is as defined by the Brinson & Fachler method and the alternative formulation as defined
#' by Brinson, Hood & Beebower is not supported by this method.
#' 
#' @aliases Attribution.levels
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param Rb xts, data frame or matrix of benchmark returns
#' @param wb vector, xts, data frame or matrix of benchmark weights
#' @param Rpl xts, data frame or matrix of portfolio returns in local currency
#' @param Rbl xts, data frame or matrix of benchmark returns in local currency
#' @param Rbh xts, data frame or matrix of benchmark returns hedged into the
#' base currency
#' @param method Used to select the priority between allocation and selection 
#' effects in arithmetic attribution. May be any of: \itemize{ \item none - 
#' present allocation, selection and interaction effects independently, 
#' \item top.down - the priority is given to the group allocation. Interaction
#' term is combined with the security selection effect, \item bottom.up - the 
#' priority is given to the security selection. Interaction term is combined 
#' with the group allocation effect} 
#' By default "top.down" is selected.
#' @param h data.frame with the hierarchy obtained from the buildHierarchy 
#' function or defined manually in the same style as buildHierarchy's
#' output
#' @param h_levels The remaining passthrough parameters represent the levels in the
#' hierarchy to aggregate by
#' @param geometric TRUE/FALSE,  whether to use geometric or arithmetic excess
#' returns for the attribution analysis. By default this is set to FALSE, which results
#' in arithmetic excess return attribution.
#' @param anchored TRUE/FALSE, to indicate if the weights at each level should be
#' anchored based on prior level's decision, as outlined in the Morningstar
#' methodology documented referenced below
#' @return returns the list with geometric excess returns including annualized
#' geometric excess returns, total attribution effects (allocation, selection 
#' and total) including total multi-period attribution effects, attribution 
#' effects at each level and security selection
#' @author Andrii Babii
#' @seealso \code{\link{Attribution.geometric}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 215-220
#' @references \url{https://corporate.morningstar.com/us/documents/MethodologyDocuments/MethodologyPapers/EquityPerformanceAttributionMeth.pdf}
#' @keywords multi-level attribution, geometric attribution
#' @examples
#' 
#' data(attrib)
#' Attribution.levels(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], 
#' wb = attrib.weights[2, ], h = attrib.hierarchy, c("type", "MarketCap", "Sector"))
#' Attribution.levels(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], 
#' wb = attrib.weights[2, ], h = attrib.hierarchy, c("type", "Sector"))
#' 
#' @export
Attribution.levels <-
function(Rp, wp, Rb, wb, 
         Rpl = NA, Rbl = NA, Rbh = NA, 
         method = "top.down", h, h_levels, geometric = FALSE, anchored = TRUE)
{   # @author Andrii Babii
    
    # DESCRIPTION:
    # Function to perform the geometric attribution analysis.
    
    # Inputs:
    # Rp       xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
    # h        data.frame with the hierarchy
    
    # Outputs: 
    # This function returns the list with total attribution effects 
    # (allocation, selection and total) including total multi-period 
    # attribution effects, attribution effects at each level and security
    # selection
    
    # FUNCTION:
    Rp = checkData(Rp)
    Rb = checkData(Rb)
    colnames(Rb) = colnames(Rp)
    WP = wp   # Save original weights in order to avoid double conversion later
    WB = wb
    if (is.vector(wp)){
      wp = as.xts(matrix(rep(wp, nrow(Rp)), nrow(Rp), ncol(Rp), byrow = TRUE), 
                  index(Rp))
      colnames(wp) = colnames(Rp)
    }
    else{
      wp = WP
    }
    if (is.vector(wb)){
      wb = as.xts(matrix(rep(wb, nrow(Rb)), nrow(Rb), ncol(Rb), byrow = TRUE), 
                  index(Rb))
      colnames(wb) = colnames(Rb)
    }
    else{
      wb = WB
    }
    
    if (nrow(wp) < nrow(Rp)){ # Rebalancing occurs next day
      Rp = Rp[2:nrow(Rp)]
      Rb = Rb[2:nrow(Rb)]
    }
    if (ncol(Rb) == 1){
      Rb = matrix(rep(coredata(Rb), ncol(Rp)), nrow(Rp), ncol(Rp))
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
    
    levels <- unlist(list(h_levels))
    if (!is.null(levels)) stopifnot(is.character(levels))
    if (length(levels) == 0){
      stop("Use Attribution function for the single level. This function is for
           the multi-level attribution")
    }
    
    # Get portfolio and benchmark returns
    if (is.vector(WP)  & is.vector(WB)){
      # If we have just one observation we simply sum up the contributions
      if(NROW(Rp) == 1 & NROW(Rb) == 1) {
        rp = as.matrix(sum(WP*Rp))
        rb = as.matrix(sum(WB*Rb))
      } else {
        rp = Return.portfolio(Rp, WP, geometric = geometric)
        rb = Return.portfolio(Rb, WB, geometric = geometric)
      }
    } else{
      # If we have just one observation we simply sum up the contributions
      if(NROW(Rp) == 1 & NROW(WP) == 1 & NROW(Rb) == 1 & NROW(WB) == 1) {
        rp = as.matrix(sum(coredata(WP)*coredata(Rp)))
        rb = as.matrix(sum(coredata(WB)*coredata(Rb)))
      } else {
        rp = Return.portfolio(Rp, WP, geometric = geometric)
        rb = Return.portfolio(Rb, WB, geometric = geometric)
      }
    }
    names(rp) = "Total"
    names(rb) = "Total"
    
    currency = !(is.null(dim(Rpl)) & is.null(dim(Rbl)) & is.null(dim(Rbh)))
    if(currency){
      Rpl = checkData(Rpl)
      Rbl = checkData(Rbl)
      Rbh = checkData(Rbh)
      
      if (nrow(wp) < nrow(Rpl)){ # Rebalancing occurs next day
        Rpl = Rpl[2:nrow(Rpl)]
        Rbl = Rbl[2:nrow(Rbl)]
      }
      if (ncol(Rbl) == 1){
        Rbl = matrix(rep(coredata(Rbl), ncol(Rpl)), nrow(Rpl), ncol(Rpl))
      }
      if (ncol(Rbl) != ncol(Rpl)){
        stop("Please use benchmark xts that has columns with benchmarks for each
           asset or one common benchmark for all assets")
      }
      
      # Get portfolio and benchmark local returns
      if (is.vector(WP)  & is.vector(WB)){
        # If we have just one observation we simply sum up the contributions
        if(NROW(Rpl) == 1 & NROW(Rbl) == 1) {
          rpl = as.matrix(sum(WP*Rpl))
          rbl = as.matrix(sum(WB*Rbl))
        } else {
          rpl = Return.portfolio(Rpl, WP, geometric = geometric)
          rbl = Return.portfolio(Rbl, WB, geometric = geometric)
        }
      } else{
        # If we have just one observation we simply sum up the contributions
        if(NROW(Rpl) == 1 & NROW(WP) == 1 & NROW(Rbl) == 1 & NROW(WB) == 1) {
          rpl = as.matrix(sum(coredata(WP)*coredata(Rpl)))
          rbl = as.matrix(sum(coredata(WB)*coredata(Rbl)))
        } else {
          rpl = Return.portfolio(Rpl, WP, geometric = geometric)
          rbl = Return.portfolio(Rbl, WB, geometric = geometric)
        }
      }
      names(rpl) = "Total"
      names(rbl) = "Total"
      
      # Compute currency effect
      bsl = reclass(rowSums(Rbl * wp), Rpl)
      bsh = reclass(rowSums(((wp - wb) * Rbh + wb * Rbl)), Rpl)
      if(geometric){
        hedge = (1 + bsl) / (1 + bsh) - 1
        currency.attr = (1 + rp) * (1 + rbl) / (1 + rpl) / (1 + rb) - 1
      } else{
        hedge = bsl - bsh
        currency.attr = (rp - rpl) - (rb - rbl)
      }
      curr = cbind(hedge, currency.attr)
      colnames(curr) = c("Hedging", "Currency attribution")
    }
    
    if(geometric){
      # Geometric excess returns + annualized geometric excess returns
      excess.returns = (1 + rp) / (1 + coredata(rb)) - 1
      if (nrow(rp) > 1){
        er = Return.annualized.excess(rp, rb)
        excess.returns = rbind(as.matrix(excess.returns), er)
      }
      colnames(excess.returns) = "Geometric"
    } else{
      # Arithmetic excess returns + annualized arithmetic excess returns
      excess.returns = rp - coredata(rb)
      if (nrow(rp) > 1){
        er = Return.annualized.excess(rp, rb, geometric = FALSE)
        excess.returns = rbind(as.matrix(excess.returns), er)
      }
      colnames(excess.returns) = "Arithmetic"
    }
    
    # Transform the hierarchy to the correct form
    if(length(levels) > 1){
      for (i in 2:length(levels)){
        if (is.numeric(h[[levels[i]]])){
          h = HierarchyQuintiles(h, levels[i])
        }
        h[[levels[i]]] = paste(h[[levels[i - 1]]],  h[[levels[i]]], sep = "-")
      }
    }
    
    # Get returns and weights at all levels
    returns.p = list()
    weights.p = list()
    returns.b = list()
    weights.b = list()
    bs = list()
    rs = list()
    for(i in 1:length(levels)){
      if(!currency){
        weights.p[[i]] = Weight.level(WP, Rp, h, level = levels[i])
        weights.b[[i]] = Weight.level(WB, Rb, h, level = levels[i])
        returns.p[[i]] = Return.level(Rp, WP, h, level = levels[i], weights.p[[i]])
        returns.b[[i]] = Return.level(Rb, WB, h, level = levels[i], weights.b[[i]])
        # semi-notional funds returns
        bs[[i]] = reclass(rowSums(returns.b[[i]] * weights.p[[i]]), rp)  
        rs[[i]] = reclass(rowSums(returns.p[[i]] * weights.b[[i]]), rp)
      } else{
        weights.p[[i]] = Weight.level(WP, Rpl, h, level = levels[i])
        weights.b[[i]] = Weight.level(WB, Rbl, h, level = levels[i])
        returns.p[[i]] = Return.level(Rpl, WP, h, level = levels[i], weights.p[[i]])
        returns.b[[i]] = Return.level(Rbl, WB, h, level = levels[i], weights.b[[i]])
        # semi-notional funds returns
        bs[[i]] = reclass(rowSums(returns.b[[i]] * weights.p[[i]]), rpl)  
        rs[[i]] = reclass(rowSums(returns.p[[i]] * weights.b[[i]]), rpl)
      }
    }
    names(returns.p) = levels
    names(weights.p) = levels
    names(returns.b) = levels
    names(weights.b) = levels
    
    if(!currency){
      # Total attribution effects
      allocation = matrix(rep(NA, nrow(Rp) * length(levels)), nrow(Rp), 
                          length(levels))
      interaction = matrix(rep(NA, nrow(Rp) * length(levels)), nrow(Rp), 
                           length(levels))
      
      if(geometric){
        allocation[, 1] = (1 + bs[[1]]) / coredata(1 + rb) - 1 # Allocation 1
        if(length(levels) > 1){
          for (i in 2:length(levels)){
            allocation[, i] = (1 + bs[[i]]) / (1 + bs[[i-1]]) - 1
          }
        }
        selection = (1 + rp) / (1 + last(bs)[[1]]) - 1
      } else{
        allocation[, 1] = bs[[1]] - coredata(rb) # Allocation 1
        if(length(levels) > 1){
          for (i in 2:length(levels)){
            allocation[, i] = bs[[i]] - bs[[i-1]]
          }
        }
        interaction[, 1] = coredata(rp) - rs[[1]] - bs[[1]] + coredata(rb) # Interaction 1
        if(length(levels) > 1){
          for (i in 2:length(levels)){
            interaction[, i] = rs[[i-1]] - rs[[i]] - bs[[i]] + bs[[i-1]]
          }
        }
        
        selection = last(rs)[[1]] - rb
        if (method == "top.down") {
          selection = selection + rowSums(interaction)
        }
        else if (method == "bottom.up") {
          allocation = allocation + interaction
        }
      }
    } else{
      # Total attribution effects
      allocation = matrix(rep(NA, nrow(Rpl) * length(levels)), nrow(Rpl), 
                          length(levels))
      interaction = matrix(rep(NA, nrow(Rpl) * length(levels)), nrow(Rpl), 
                           length(levels))
      
      if(geometric){
        allocation[, 1] = (1 + bs[[1]]) / coredata(1 + rbl) - 1 # Allocation 1
        if(length(levels) > 1){
          for (i in 2:length(levels)){
            allocation[, i] = (1 + bs[[i]]) / (1 + bs[[i-1]]) - 1
          }
        }
        selection = (1 + rpl) / (1 + last(bs)[[1]]) - 1
      } else{
        allocation[, 1] = bs[[1]] - coredata(rbl) # Allocation 1
        if(length(levels) > 1){
          for (i in 2:length(levels)){
            allocation[, i] = bs[[i]] - bs[[i-1]]
          }
        }
        interaction[, 1] = coredata(rpl) - rs[[1]] - bs[[1]] + coredata(rbl) # Interaction 1
        if(length(levels) > 1){
          for (i in 2:length(levels)){
            interaction[, i] = rs[[i-1]] - rs[[i]] - bs[[i]] + bs[[i-1]]
          }
        }
        
        selection = last(rs)[[1]] - rbl
        if (method == "top.down") {
          selection = selection + rowSums(interaction)
        }
        else if (method == "bottom.up") {
          allocation = allocation + interaction
        }
      }
    }
    
    # Transform portfolio, benchmark returns and semi-notional funds returns to
    # conformable matrices for multi-level attribution
    if(!currency){
      if(NROW(Rp) == 1)
      {
        b = as.xts(matrix(rep(rb, ncol(returns.b[[1]])), nrow(rb), 
                          ncol(returns.b[[1]])), index(Rb))
        r = as.xts(matrix(rep(rp, ncol(returns.p[[1]])), nrow(rp), 
                          ncol(returns.p[[1]])), index(Rp))
      } else{
        b = as.xts(matrix(rep(rb, ncol(returns.b[[1]])), nrow(rb), 
                          ncol(returns.b[[1]])), index(rb))
        r = as.xts(matrix(rep(rp, ncol(last(returns.p)[[1]])), nrow(rp),
                          ncol(last(returns.p)[[1]])), index(rp))
      }
    } else{
      if(NROW(Rpl) == 1)
      {
        b = as.xts(matrix(rep(rbl, ncol(returns.b[[1]])), nrow(rbl), 
                          ncol(returns.b[[1]])), index(Rbl))
        r = as.xts(matrix(rep(rpl, ncol(returns.p[[1]])), nrow(rpl), 
                          ncol(returns.p[[1]])), index(Rpl))
      } else{
        b = as.xts(matrix(rep(rbl, ncol(returns.b[[1]])), nrow(rbl), 
                          ncol(returns.b[[1]])), index(rbl))
        r = as.xts(matrix(rep(rpl, ncol(last(returns.p)[[1]])), nrow(rpl),
                          ncol(last(returns.p)[[1]])), index(rpl))
      }
    }
    
    returns.b2 = list()
    weights.p2 = list()
    weights.b2 = list()
    if(length(levels) > 1){
      for (j in 1:(length(levels) - 1)){ 
        if(!currency){
          # make benchmark returns & weights conformable at different levels
          wp_l = Weight.level(WP, Rp, h, level = levels[j])
          wb_l = Weight.level(WB, Rb, h, level = levels[j])
          wp_h = Weight.level(WP, Rp, h, level = levels[j+1])
          wb_h = Weight.level(WB, Rb, h, level = levels[j+1])
          r_l = Return.level(Rb, WB, h, level = levels[j], Weight.level(WB, Rb, h, level = levels[j]))
          r_h = Return.level(Rb, WB, h, level = levels[j + 1], Weight.level(WB, Rb, h, level = levels[j+1]))
        } else{
          # make benchmark returns & weights conformable at different levels
          wp_l = Weight.level(WP, Rpl, h, level = levels[j])
          wb_l = Weight.level(WB, Rbl, h, level = levels[j])
          wp_h = Weight.level(WP, Rpl, h, level = levels[j+1])
          wb_h = Weight.level(WB, Rbl, h, level = levels[j+1])
          r_l = Return.level(Rbl, WB, h, level = levels[j], Weight.level(WB, Rbl, h, level = levels[j]))
          r_h = Return.level(Rbl, WB, h, level = levels[j + 1], Weight.level(WB, Rbl, h, level = levels[j+1]))
        }
        hierarchy = split(h[levels[j]], h[levels[j + 1]])
        for (i in 1:ncol(r_h)){
          r_h[, i] = r_l[, hierarchy[[i]][1, 1]]
          wp_h[, i] = wp_l[, hierarchy[[i]][1, 1]]
          wb_h[, i] = wb_l[, hierarchy[[i]][1, 1]]
        }
        returns.b2[[j]] = r_h
        weights.p2[[j]] = wp_h
        weights.b2[[j]] = wb_h
      }
    }
    
    if(length(bs) > 1){
      for (i in 1:(length(bs) - 1)){
        bs[[i]] = as.xts(matrix(rep(bs[[i]], ncol(returns.b2[[i]])), nrow(r), 
                                ncol(returns.b2[[i]])), index(r))
      }
      # Use the last iteration index for the number of columns to set the last list element
      bs[[length(bs)]] = as.xts(matrix(rep(bs[[length(bs)]], ncol(returns.b2[[i]])), nrow(r), ncol(returns.b2[[i]])), index(r))
    } else{
      # Use the last iteration index for the number of columns to set the last list element
      bs[[length(bs)]] = as.xts(matrix(rep(bs[[length(bs)]], ncol(returns.b[[1]])), nrow(r), ncol(returns.b[[1]])), index(r))
    }
    
    # Attribution at each level
    level = list() # represents allocation effects at each level
    interaction_level = list() # interaction effects at each level
    if(geometric) {
      level[[1]] = (weights.p[[1]] - weights.b[[1]]) * ((1 + returns.b[[1]]) 
                                                        / (1 + b) - 1)
    } else{
      # Brinson and Fachler (1985) allocation effect
      level[[1]] = (weights.p[[1]] - weights.b[[1]]) * (returns.b[[1]] - b)
      
      interaction_level[[1]] = (weights.p[[1]] - weights.b[[1]]) * (returns.p[[1]] - returns.b[[1]])
      if (method == "bottom.up") {
        level[[1]] = level[[1]] + interaction_level[[1]]
      }
    }
    if(length(levels) > 1){
      for (i in 2:length(levels)){ 
        if(geometric){
          if(anchored){
            level[[i]] = (weights.p[[i]] - weights.b[[i]]*weights.p2[[i-1]]/weights.b2[[i-1]]) * 
              ((1 + returns.b[[i]]) / (1 + returns.b2[[i-1]]) - 1) * 
              ((1 + returns.b2[[i-1]]) / (1 + bs[[i-1]]))
          } else{
            level[[i]] = (weights.p[[i]] - weights.b[[i]]) * 
              ((1 + returns.b[[i]]) / (1 + returns.b2[[i-1]]) - 1) * 
              ((1 + returns.b2[[i-1]]) / (1 + bs[[i-1]]))
          }
        } else{
          if(anchored){
            # Brinson and Fachler (1985) allocation effect
            level[[i]] = (weights.p[[i]] - weights.b[[i]]*weights.p2[[i-1]]/weights.b2[[i-1]]) * 
              (returns.b[[i]] - returns.b2[[i-1]])
            
            interaction_level[[i]] = (weights.p[[i]] - weights.b[[i]]*weights.p2[[i-1]]/weights.b2[[i-1]]) * 
              (returns.p[[i]] - returns.b[[i]])
          } else{
            # Brinson and Fachler (1985) allocation effect
            level[[i]] = (weights.p[[i]] - weights.b[[i]]) * 
              (returns.b[[i]] - returns.b2[[i-1]])
            
            interaction_level[[i]] = (weights.p[[i]] - weights.b[[i]]) * 
              (returns.p[[i]] - returns.b[[i]])
          }
          if (method == "bottom.up") {
            level[[i]] = level[[i]] + interaction_level[[i]]
          }
        }
      }
    }
    
    if(!currency){
      if(geometric){
        # Security/Asset selection
        select = reclass(weights.p[[length(weights.p)]], rp) * 
          ((1 + returns.p[[length(returns.p)]]) / (1 + returns.b[[length(returns.b)]]) - 1) * 
          ((1 + returns.b[[length(returns.b)]]) / (1 + bs[[length(bs)]]))
      } else{
        # Security/Asset selection
        if(anchored) {
          select = reclass(weights.b[[length(weights.b)]]*weights.p2[[length(weights.b)-1]]/weights.b2[[length(weights.b)-1]], rb) * 
            (returns.p[[length(returns.p)]] - returns.b[[length(returns.b)]])
          
        } else {
          select = reclass(weights.b[[length(weights.b)]], rb) * 
            (returns.p[[length(returns.p)]] - returns.b[[length(returns.b)]])
        }
        if(method == "top.down") {
          select = select + last(interaction_level)[[1]]
        }
      }
    } else{
      if(geometric){
        # Security/Asset selection
        select = reclass(weights.p[[length(weights.p)]], rpl) * 
          ((1 + returns.p[[length(returns.p)]]) / (1 + returns.b[[length(returns.b)]]) - 1) * 
          ((1 + returns.b[[length(returns.b)]]) / (1 + bs[[length(bs)]]))
      } else{
        # Security/Asset selection
        if(anchored) {
          select = reclass(weights.b[[length(weights.b)]]*weights.p2[[length(weights.b)-1]]/weights.b2[[length(weights.b)-1]], rbl) *
            (returns.p[[length(returns.p)]] - returns.b[[length(returns.b)]])
        } else {
          select = reclass(weights.b[[length(weights.b)]], rbl) *
            (returns.p[[length(returns.p)]] - returns.b[[length(returns.b)]])
        }
        if(method == "top.down") {
          select = select + last(interaction_level)[[1]]
        }
      }
    }
    # Get the multi-period summary
    if(geometric == FALSE & method == "none") {
      general = cbind(allocation, interaction, selection)
    } else {
      general = cbind(allocation, selection)
    }
    general = rbind(as.data.frame(general), (apply(1 + general, 2, prod) - 1))
    
    for (i in 1:length(level)){
      level[[i]] = rbind(as.data.frame(level[[i]]), 
                         (apply(1 + level[[i]], 2, prod) - 1))
      rownames(level[[i]])[nrow(level[[i]])] = "Total"

      if(geometric == FALSE) {
        interaction_level[[i]] = rbind(as.data.frame(interaction_level[[i]]), 
                                       (apply(1 + interaction_level[[i]], 2, prod) - 1))
        rownames(interaction_level[[i]])[nrow(interaction_level[[i]])] = "Total"
      }
    }
    select = rbind(as.data.frame(select), (apply(1 + select, 2, prod) - 1))
    rownames(general)[nrow(general)] = "Total"
    rownames(select)[nrow(select)] = "Total"
    
    # Label the output
    result = list()
    labels = paste(rep("Level", length(levels)), 1:length(levels))
    names(level) = labels
    if(geometric == FALSE & method == "none") {
      colnames(general) = c(paste(labels, (rep("Allocation", 
                                               length(levels)))), 
                            paste(labels, (rep("Interaction", 
                                               length(levels)))), "Selection")
    } else {
      colnames(general) = c(paste(labels, (rep("Allocation", 
                                               length(levels)))), "Selection")
    }
    
    result[[1]] = excess.returns
    result[[2]] = general
    result[[3]] = level
    if (!currency){
      if(geometric == FALSE & method == "none") {
        result[[4]] = interaction_level
        result[[5]] = select
        names(result) = c("Excess returns", "Multi-level attribution", 
                          "Allocation at each level", "Interaction at each level", "Security selection")
      } else {
        result[[4]] = select
        names(result) = c("Excess returns", "Multi-level attribution", 
                          "Allocation at each level", "Security selection")
      }
    } else{
      if(geometric == FALSE & method == "none") {
        result[[4]] = interaction_level
        result[[5]] = select
        result[[6]] = curr
        names(result) = c("Excess returns", "Multi-level attribution", 
                          "Allocation at each level", "Interaction at each level", "Security selection", 
                          "Currency management")
      } else {
        result[[4]] = select
        result[[5]] = curr
        names(result) = c("Excess returns", "Multi-level attribution", 
                          "Allocation at each level", "Security selection", 
                          "Currency management")
      }
    }
    return(result)
}
