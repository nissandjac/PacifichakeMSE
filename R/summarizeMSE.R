#' Title
#'
#' @param ls.save a saved list of MSE's
#' @param perc Percentiles to save
#'
#' @return Returns summaeized data
#' @export
#'
#' @examples
#' summarizeMSE(ls.save) # Get a summary of the MSE
summarizeMSE <- function(ls.save, perc = c(0.1,0.9)){


  # This function needs  to be generalized

  df.MSE <- purrr::flatten(ls.save) # Change the list a little bit


  catchcdf <- processMSE(df.MSE, 'Catch', idx = c(2,3), spacenames = c('CAN', 'USA'), runs = 100,nspace = 2)
  catchdf <- processMSE(df.MSE, 'Catch', idx = 2, spacenames = c('CAN', 'USA'), runs = 100,nspace = 2)
  SSB_mid <- processMSE(df.MSE, id = 'SSB.mid', idx = c(1,2), spacenames = c('CAN', 'USA'), runs = 100,nspace = 2)
  SSB_tot <- processMSE(df.MSE, id = 'SSB', idx = 1, spacenames = c('CAN', 'USA'), runs = 100,nspace = 2)
  # Calculate AAV
  AAVdf <- AAV(catchcdf)
  AAVtottmp <- AAV(catchdf)

  catch.tot.tmp <- catchdf %>%
    group_by(year) %>%
    summarise(catchmean = median(value),
              quantsmax = quantile(value, probs =perc[2]),
              quantsmin = quantile(value, probs= perc[1]))


  SSB.tot.tmp <- SSB_tot  %>%
    group_by(year) %>%
    summarise(SSBmean = median(value),
              quantsmax = quantile(value, probs = perc[2]),
              quantsmin = quantile(value, probs= perc[1]))


  AAV.tot.tmp <- AAVtottmp[AAVtottmp$year > 1966,] %>%
    group_by(year) %>%
    summarise(AAVmean = median(AAV, na.rm = TRUE),
              quantsmax = quantile(AAV, probs = perc[2]),
              quantsmin = quantile(AAV, probs= perc[1]))



return(list(catch = catch.tot.tmp,
            SSB = SSB.tot.tmp,
            AAV = AAV.tot.tmp,
            catchcountry = catchcdf,
            SSB_mid = SSB_mid))
}
