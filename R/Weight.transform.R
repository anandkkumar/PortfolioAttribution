#' transforms weights for the attribution functions
#' 
#' Makes transformation of weights to the xts object conformable with returns
#' and to be taken by the attribution functions
#' 
#' @aliases Weight.transform
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @author Andrii Babii
#' @seealso  \code{buildHierarchy} \cr \code{\link{Attribution}} \cr 
#' \code{\link{Weight.level}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 
#' 2009. Chapter 17
#' @keywords attribution
#' @examples
#' 
#' data(attrib)
#' Weight.transform(wp = attrib.weights[1, ], Rp = attrib.returns[, 1:10])
#' 
#' @export
Weight.transform <- 
  function(wp, Rp)
  {   # @author Andrii Babii
    
    # DESCRIPTION:
    # Function to transform weights to the xts object conformable with returns
    # used by aggregation and attribution functions
    
    # Inputs:
    # wp      vector, xts, data frame or matrix of portfolio weights
    # Rp      xts, data frame or matrix of portfolio returns
    
    # Outputs: 
    # This function returns the xts object with weights conformable with 
    # returns
    
    # FUNCTION:
    if (is.vector(wp)){
      wp = xts::as.xts(matrix(rep(wp, nrow(Rp)), nrow(Rp), ncol(Rp), byrow = TRUE), 
                       zoo::index(Rp))
      colnames(wp) = colnames(Rp)
    } else{
      if(as.Date(xts::last(zoo::index(Rp))) < (as.Date(zoo::index(wp[1, ])) + 1)){
        stop(paste('last date in series', as.Date(xts::last(zoo::index(Rp))),
                   'occurs before beginning of first rebalancing period',
                   as.Date(first(zoo::index(wp))) + 1))
      }
      wp = PerformanceAnalytics::checkData(wp, method = "xts")
      wp = merge(wp, xts::xts(, zoo::index(Rp)))
      wp = na.locf(wp)
      if(as.Date(first(zoo::index(Rp))) > (as.Date(zoo::index(wp[1,]))+1)) {
        warning(paste('data series starts on', as.Date(first(zoo::index(Rp))), ', 
                      which is after the first rebalancing period', 
                      as.Date(first(zoo::index(wp)))+1)) 
        wp = wp
      } else{
        wp = wp[2:nrow(wp)]
      }
    }
    return(wp)
}
