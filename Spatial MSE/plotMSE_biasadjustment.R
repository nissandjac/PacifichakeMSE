## Load MSE's and compare ## 
plotMSE_biasadjustment <- function(results, plotnames = NA, plotexp = FALSE){
  
  source('df_lists.R')
  source('df_lists_OM.R')
  source('load_files_OM.R')
  source('fn_plot_MSE.R')
  source('hake_objectives.R')
  
  require(ggplot2)
  require(dplyr)
  require(scales)
  require(RColorBrewer)
  require(cowplot)
  require(gridExtra)
  
  # Load data 
  
  fls <- dir(results)
  ls.plots <- list()
  
  fls <- fls[grep('.Rdata', x = fls)]
  
  for(i in 1:length(fls)){
    load(paste(results,fls[i], sep= ''))
    ls.plots[[i]] <- ls.save
  }
  
  # Just plot the biomass with no fishing
  
  if(is.na(plotnames[1])){
    plotnames <- rep(NA, length(fls))
    for(i in 1:length(fls)){
      plotnames[i] <- strsplit(fls[i],split = '\\.')[[1]][1] 
      
    }
  }else{
    
    if(length(plotnames) != length(ls.plots)){
      stop('incorrect number of names')
    }
  }
  
  ls.data <- list()
  
  for(i in 1:length(plotnames)){
    ls.data[[i]] <- df_lists_OM(ls.plots[[i]], plotnames[i]) 
    
  }
  
  cols <- brewer.pal(6, 'Dark2')
  
  df.country<- data.frame(ls.data[[1]][[3]]$SSB)
  df.all <- data.frame(ls.data[[1]][[3]]$SSB.all)
  
  for(i in 2:length(plotnames)){
    df.country <- rbind(df.country, ls.data[[i]][[3]]$SSB)
    df.all <- rbind(df.all, ls.data[[i]][[3]]$SSB.all)
      }
  
  
  df.country$run <- as.factor(df.country$run)
  
  p1 <- ggplot(df.country, aes(x = year, y = med.can*1e-6))+geom_line(color = 'darkred', size = 1.5)+
    geom_line(aes(y = med.US*1e-6), color = 'darkblue', size = 1.5)+theme_classic()+scale_y_continuous(name ='SSB (million tonnes)')+facet_wrap(~run)+  
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
    geom_ribbon(aes(ymin = p5.can*1e-6, ymax = p95.can*1e-6), fill = alpha('red', alpha = 0.2), linetype = 0)+
    geom_ribbon(aes(ymin = p5.US*1e-6, ymax = p95.US*1e-6), fill = alpha('blue', alpha = 0.2), linetype = 0)
  
  p1
  
  df <- load_data_seasons()
  sim.data <- run.agebased.true.catch(df)
  
  p2 <- ggplot(df.all[df.all$year>2005,], aes(x = year, y = med*1e-6, color = run, fill = run))+geom_line(size = 1.5)+
    theme_classic()+scale_y_continuous(name ='SSB (million tonnes)')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
    scale_color_manual(values = cols[1:length(plotnames)])+
    geom_ribbon(aes(ymin = p5*1e-6, ymax = p95*1e-6), 
                linetype = 0)+
    scale_fill_manual(values = alpha(cols[1:length(plotnames)], alpha = 0.2))+
    geom_hline(aes(yintercept = sum(sim.data$SSB0*1e-6)), color = 'black', linetype = 2)
   
  p2
  
  if(plotexp == TRUE){
  png(paste(results,'SSB_0.png'), width = 16, height =12, res = 400, unit = 'cm')
  print(p1)
  dev.off()
  
  
  png(paste(results,'SSB_country.png'), width = 16, height =12, res = 400, unit = 'cm')
  print(p2)
  dev.off()
  }else{
   grid.arrange(p1,p2)
  }
  
return(list(p1,p2))  
}