#' Title
#'
#' @param obj TMB object
#'
#' @return Return estimated parameters and uncertainty
#' @export
#'
#' @examples
#' 
#' 
#' 
extract_fixed = function( obj ){
  if( length(obj$env$random)==0 ){
    Return = obj$env$last.par.best
  }else{
    Return = obj$env$last.par.best[-c(obj$env$random)]
  }
  return( Return )
}

Check_Identifiable_vs2 = function( obj , printParams = TRUE ){

    # Finite-different hessian
  ParHat = extract_fixed( obj )
  List = NULL
  List[["Hess"]] = optimHess( par=ParHat, fn=obj$fn, gr=obj$gr )

  # Check eigendecomposition
  if(is.nan(max(List[['Hess']]))){
    print('model not converging')
  }else{
  List[["Eigen"]] = eigen( List[["Hess"]] )
  List[["WhichBad"]] = which(List[["Eigen"]]$values < sqrt(.Machine$double.eps) )

  # Check for parameters
  if(length( List[["Eigen"]]$vectors[,List[["WhichBad"]]])>0){
    RowMax = apply(as.matrix(List[["Eigen"]]$vectors[,List[["WhichBad"]]]),
                   MARGIN=1, FUN=function(vec){max(abs(vec))} )
  }else{
    RowMax <- rep(0,length(List[['Eigen']]$values))
  }


  List[["BadParams"]] = data.frame("Param"=names(obj$par), "MLE"=ParHat, "Param_check"=ifelse(RowMax>0.1, "Bad","OK"))

  # Message
  if( length(List[["WhichBad"]])==0 ){
    message( "All parameters are identifiable" )
  }else{
    if(printParams == TRUE){
    print( List[["BadParams"]] )
    }else{

    }
  }

  # Return
  return( invisible(List) )
  }
}
