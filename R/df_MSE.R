#' sum spatial and seasonal data into a data frame
#'
#' @param x list of data
#' @param idx which index to sum over
#'
#' @return returns list as data frame
#' @export
#'
#' @examples
#' catchtmp.country <- map(df.MSE[names(df.MSE) == 'Catch'],.f = fnsum, idx = c(2,3))
#'
#'
fnsum <- function(x, idx){

  apply(x, MARGIN = idx, sum)
}


#' Turn whole list into a concanated polished data frame
#'
#' @param df.MSE MSE that contains the data
#' @param id object to extract
#' @param idx which indexes to save to
#' @param fn function to summarize data sums; sums data over indices, rows extracts data
#' @param spacenames naming convention for spatial areas
#' @param runs number of runs in MSE
#' @param nspace number of spaces

#'
#' @return returns a concatenated data frame
#' @export
#'
#' @examples
#' processMSE(test) # not run
#'
processMSE <- function(
  df.MSE,
  id,
  idx = 1,
  fn = 'sums',
  spacenames = NA,
  runs = 100,
  nspace = 2
){

  if(fn == 'sums'){
  tmp <- lapply(purrr::map(df.MSE[names(df.MSE) == id],
                         .f = fnsum, idx = idx), data.frame, stringsAsFactors = FALSE)
  }

  if(fn == 'rows'){
  tmp <- lapply(purrr::map(df.MSE[names(df.MSE) == id],
                         .f = select, idx), data.frame, stringsAsFactors = FALSE)
  }

  if(length(idx) == 1){
  tmp <- lapply(tmp, setNames, nm = 'value')
  }

  if(all(is.na(spacenames) == 0)){
  tmp <- lapply(tmp, setNames, spacenames)
  }


yrs <- as.numeric(rownames(df.MSE[names(df.MSE) == 'F0'][[1]]))

names(tmp) <- paste('run',1:runs, sep = '')

tmp.df <- data.table::rbindlist(tmp, idcol = 'run')
tmp.df$year <- rep(yrs, runs)

mnames <- names(tmp.df)
mnames <- mnames[-which(mnames %in% c('year','run'))]

if(length(idx) > 1){
  tmp.df <- reshape2::melt(tmp.df, measure.vars = mnames)
}


return(tmp.df)
}

