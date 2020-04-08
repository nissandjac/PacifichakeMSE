#' Turn whole list into a concanated polished data frame
#'
#' @param df.MSE MSE that contains the data
#' @param id object to extract
#' @param idx which indexes to save to
#' @param spacenames naming convention for spatial areas
#' @param runs number of runs in MSE
#' @param nspace number of spaces

#'
#' @return returns a concanated data frame
#' @export
#'
#' @examples
#' processMSE(test) # not run
#'
processMSE <- function(
  df.MSE,
  id,
  idx = 1,
  spacenames = c('CAN','USA'),
  runs = 100,
  nspace = 2
){

tmp <- lapply(purrr::map(df.MSE[names(df.MSE) == id],
                         .f = apply(x, MARGIN = idx, sum), idx = idx), data.frame, stringsAsFactors = FALSE)

yrs <- as.numeric(rownames(tmp[[1]]))

if(length(idx) > 1){
tmp <- lapply(tmp, setNames, spacenames)
}

names(tmp) <- paste('run',1:runs, sep = '')

tmp.df <- data.table::rbindlist(tmp, idcol = 'run')
tmp.df$year <- rep(yrs, runs)

return(tmp.df)
}

