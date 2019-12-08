fn_rerun_OM <- function(folders, df, climate = FALSE){

# Get the data from the MSE runs 
ls <- dir(folders)[grep('.Rdata',dir(folders))]

  
for(mse in length(ls)){  
  
  ls_mse <- load(paste(folders,ls[mse], sep =''))
  
  ls.tmp  <- list()
  
  for (i in 1:nruns){
    if(is.null(ls.JMC[[i]])){
      ls.tmp[[i]] <- NULL
    }else{
      tmp <- run_multiple_OMs(simyears = 30, seeds[i],df, Catchin = ls.JMC[[i]]$Catch[(df$nyear+1):(length(year.future)-1)])
      ls.JMC.s[[i]] <- tmp
    }
  }
  save(ls.JMC.s,file = 'results/Operating models/JMC_OM.RData')

}


}