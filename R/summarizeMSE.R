summarizeMSE <- function(ls.save){
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
              quants95 = quantile(value, probs =perc[2]),
              quants5 = quantile(value, probs= perc[1]))


  SSB.tot.tmp <- SSB_tot  %>%
    group_by(year) %>%
    summarise(SSBmean = median(value),
              quants95 = quantile(value, probs = perc[2]),
              quants5 = quantile(value, probs= perc[1]))


  AAV.tot.tmp <- AAVtottmp[AAVtottmp$year > 1966,] %>%
    group_by(year) %>%
    summarise(AAVmean = median(AAV, na.rm = TRUE),
              quants95 = quantile(AAV, probs = perc[2]),
              quants5 = quantile(AAV, probs= perc[1]))


  strs <- strsplit(files[i], split = '_')[[1]]

  runname <- paste(strs[4],strs[6],strsplit(strs[8], split = '.Rdata')[[1]], sep = '_')

  catchdf$run <- runname
  catchcdf$run <- runname
  catch.tot.tmp$run <- runname

  SSB_mid$run <- runname
  SSB.tot.tmp$run <- runname

  AAVdf$run <- runname
  AAV.tot.tmp$run <- runname

  HCRname <- strsplit(strs[8], split = '.Rdata')[[1]]

  if(HCRname == 'TAC1'){
    HCRname <- 'HCR0'
  }
  if(HCRname == 'TAC2'){
    HCRname <- 'historical'
  }
  if(HCRname == 'TAC3'){
    HCRname <- 'Realized'
  }

  catchdf$HCR <- HCRname
  catchcdf$HCR <- HCRname
  catch.tot.tmp$HCR <- HCRname

  SSB_mid$HCR <- HCRname
  SSB.tot.tmp$HCR <- HCRname

  AAVdf$HCR <- HCRname
  AAV.tot.tmp$HCR <- HCRname

  catchdf$climate <- strs[6]
  catchcdf$climate <- strs[6]
  catch.tot.tmp$climate <- strs[6]

  SSB_mid$climate <- strs[6]
  SSB.tot.tmp$climate <- strs[6]
  catch.tot.tmp$climate <- strs[6]
  AAV.tot.tmp$climate <- strs[6]
  AAVdf$climate <- strs[6]

  if(i == 1){
    catchdfexp <- catchdf
    catchcdfexp <- catchcdf
    catch.tot <- catch.tot.tmp
    SSBdf <- SSB_mid
    SSB.tot <- SSB.tot.tmp
    AAVdfexp <- AAVdf
    AAV.tot <- AAV.tot.tmp

  }else{
    catchdfexp <- rbind(catchdfexp,catchdf)
    catchcdfexp <- rbind(catchcdfexp,catchcdf)
    catch.tot <- rbind(catch.tot,catch.tot.tmp)

    SSBdf <- rbind(SSBdf, SSB_mid)
    SSB.tot <- rbind(SSB.tot, SSB.tot.tmp)

    AAVdfexp <- rbind(AAVdfexp, AAVdf)
    AAV.tot <- rbind(AAV.tot, AAV.tot.tmp)
  }



}

}
