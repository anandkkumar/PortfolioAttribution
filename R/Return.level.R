#' aggregates portfolio returns up to the chosen level from the hierarchy
#' 
#' Aggregates returns and weights up to the chosen level from the hierarchy.
#' Hierarchy can be used from the \code{buildHierarchy} function or 
#' defined manually in the same way as the \code{buildHierarchy}'s 
#' output. If for the selected level the values in the hierarchy are numeric, 
#' the aggregation of returns or weights is performed by quintiles.
#'
#' @aliases Return.level
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param h  data.frame with portfolio hierarchy
#' @param level level from the hierarchy to which returns and weights will be 
#' aggregated
#' @param relativeWeights the total weight from the prior level used to normalize 
#' the weights for the current level
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
#' Return.level(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], h = attrib.hierarchy, level = "MarketCap")
#' 
#' @export
Return.level <-
function(Rp, wp, h, level = "Sector", relativeWeights = NULL)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to aggregate returns up to the chosen level from the hierarchy
    
    # Inputs:
    # Rp      xts, data frame or matrix of portfolio returns
    # wp      vector, xts, data frame or matrix of portfolio weights
    # h       data.frame with portfolio hierarchy
    # level   level from the hierarchy to which the aggregation will be done
  
    # Outputs: 
    # This function returns portfolio returns at the chosen level
  
    # FUNCTION:
    # Transform data to the xts objects    
    Rp = checkData(Rp, method = "xts")
    if (is.vector(wp)){
      wp = as.xts(matrix(rep(wp, nrow(Rp)), nrow(Rp), ncol(Rp), byrow = TRUE), 
                  index(Rp))
      colnames(wp) = colnames(Rp)
    }
    
    if(is.null(relativeWeights)){
      relativeWeights=matrix(1, nrow = NROW(wp), ncol = NCOL(wp))
    }
    
    # If level has numeric values we replace numeric values by quintiles
    if (is.numeric(h[[level]])){
      h = HierarchyQuintiles(h, level)
    }
    h = split(h$primary_id, h[level])
    returns = as.xts(matrix(NA, ncol = length(h), nrow = nrow(Rp)), index(Rp))
    for(i in 1:length(h)){
      # Check to see if the relativeWeights are all the same as the given weights
      if(all(dim(wp[,h[[i]]]) == dim(relativeWeights[, i])) && all(wp[,h[[i]]] == relativeWeights[, i])) {
        returns[, i] = rowSums(Rp[, h[[i]], drop = FALSE])
      } else {
        returns[, i] = rowSums(Rp[, h[[i]], drop = FALSE] * coredata(wp[, h[[i]], drop = FALSE])/
                                 coredata(matrix(rep(relativeWeights[, i], length(h[[i]])), ncol = length(h[[i]]))))
        # Replace the NAs with zeroes for cases where the relative weights are 0
        returns[, i] = tidyr::replace_na(returns[, i], 0)
      }
    }
    colnames(returns) = names(h)
    return(returns)
}
