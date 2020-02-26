Check_Identifiable_vs2 = function( obj ){
  # Finite-different hessian
  ParHat = TMBhelper:::extract_fixed( obj )
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
    print( List[["BadParams"]] )
  }
  
  # Return
  return( invisible(List) )
  }
}