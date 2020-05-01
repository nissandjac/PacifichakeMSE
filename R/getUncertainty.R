#' Calculate and organize the uncertainty on parameters and derived values
#'
#' @param name name of the parameter or value to extract
#' @param data vector of data for organizing the years - remove later
#' @param sdrep list of standard deviations
#'
#' @return returns a data frame ready for plotting
#' @export
#'
#' @examples
getUncertainty <- function(name,data,sdrep){

  df <- data.frame(value = sdrep[rep.values == name,1])
  df$SE <- sdrep[rep.values == name,2]
  df$min <- df$value-2*df$SE
  df$max <- df$value+2*df$SE

  if(dim(df)[1] == data$tEnd){
    df$year <- data$years
  }

  if(dim(df)[1] == (data$tEnd*data$nage)){
    # do this later
  }

  if(dim(df)[1] == (data$tEnd*data$age_maxage)){

    df$age <- rep(1:data$age_maxage, data$tEnd)
    df$year <- rep(data$year, each =length(1:data$age_maxage))

  }

  return(df)
}

